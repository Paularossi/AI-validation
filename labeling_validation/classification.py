import requests
import json
import base64
import os
import time
import re
import random
import torch
from PIL import Image

from openai import OpenAI
from huggingface_hub import login
from transformers import AutoModelForImageTextToText, Gemma3ForConditionalGeneration, AutoProcessor, LlavaForConditionalGeneration, AutoModelForCausalLM, Qwen2_5_VLForConditionalGeneration
from qwen_vl_utils import process_vision_info

from persistent.AI_validation.labeling_validation.WHO_questions import *


# use the GPU if available
device = "cuda:0" if torch.cuda.is_available() else "cpu"
torch_dtype = torch.float16 if torch.cuda.is_available() else torch.float32

TEXT_MODELS = ["google/gemma-3-4b-it", "CohereForAI/aya-vision-8b"]
MULTIMODAL_MODELS = ["Qwen/Qwen2.5-VL-7B-Instruct", "mistralai/Pixtral-12B-2409"]

pattern_api = re.compile(r"\*{1,2}(.*?)\*{1,2}: ([^\n]+?) [–-] (.*?)(?=\n\*|$)", re.DOTALL)
pattern_aya = re.compile(r"\*{1,2}(.*?)\*{1,2}:\s*(Yes|No)(?:\s*[–-]\s*(.*?))?(?=\n\*|$)", re.DOTALL)

all_questions = [alcohol, type_ad, marketing_str, premium_offer, who_cat, target_age_group]
expected_labels = [label for section in all_questions for (label, _) in section] + ['SPECULATION_LEVEL'] # all question labels

image_folder = "persistent/AI_validation/data/unique_images"
images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]

# read the keys
with open('persistent/AI_validation/keys.txt') as f:
    json_data = json.load(f)

hugg_key = json_data["huggingface"]

def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")


# change this later to do a set of images
ad_id = "123"
ad_creative_bodies = "'Shop slim, eet lekker op Black Friday🍕 Enkel vandaag: 20% korting op 1 pizza, 30% op 2 en 40% op 3 of meer🍕!"
page_name = "Domino's Pizza Belgium"

temp_image = "ad_2016607565369658_img.png" # "ad_186810294481985_img.png"
image_path = os.path.join(image_folder, temp_image)
base64_image = encode_image(image_path)


# =============== APIs ===============

# === add GPT-4o ===
#client = OpenAI()
openai_model_id = "gpt-4o"
openai_api_url = "https://api.openai.com/v1/chat/completions"
openai_api_key = json_data["openai"]

# === Mistral AI ===
mistral_model_id = "pixtral-12b-2409"
mistral_api_url = "https://api.mistral.ai/v1/chat/completions"
mistral_api_key = json_data["mistralai"]


# for mistral ai and gpt-4o
def start_classification_apis(model_id, api_key, api_url):
    print(f"Classifying using model {model_id}...")
    
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }

    image_url = f"data:image/jpeg;base64,{base64_image}"
    user_content = []
    messages = [{"role": "system", "content": instructions_1 if model_id == "gpt-4o" else instructions_new}]

    if model_id == "gpt-4o":
        # shuffle the order of the questions
        all_shuffled_questions = random.sample(all_questions, len(all_questions))

        shuffled_questions = []
        for main_question in all_shuffled_questions:
            shuffled_questions.extend(main_question)
                
        # add speculation
        shuffled_questions.extend(speculation)

        user_content = [{"type": "text", "text": q[0] + ": " + q[1]} for q in shuffled_questions]
        user_content.append({"type": "image_url", "image_url": {"url": image_url}}) # for gpt-4o
        
    else:
        user_content = create_user_content()
        user_content.append({"type": "image_url", "image_url": image_url}) # for mistral
    
    # add other user content (page name, caption)
    user_content.extend([
        {"type": "text", "text": f"Name of the page running the ad: {page_name}"},
        {"type": "text", "text": f"Ad caption: {ad_creative_bodies}"}
    ])

    messages.append({"role": "user", "content": user_content})

    payload = {
        "model": model_id,
        "messages": messages,
        "temperature": 0.01
    }

    start_time = time.time()
    response = requests.post(api_url, headers = headers, json = payload)
    end_time = time.time() 
    print(f"Time taken to generate response: {end_time - start_time} seconds") # 18 seconds with gpu

    print(response.json()) # for gpt-4o, this format doesn't work!!!! change the instructions?

    print(f"Done classifying using model {model_id}!!! YAYYYY !!!!")

    return response



# =============== Transformers ===============
login(hugg_key) # log into hugging face (gated models)

def initiate_transformers_model(model_id):
    """Load the right model and processor based on model_id."""
    # available models
    MODEL_MAP = { # try bigger
        "google/gemma-3-4b-it": Gemma3ForConditionalGeneration, # Gemma3 - https://huggingface.co/google/gemma-3-12b-it
        "Qwen/Qwen2.5-VL-7B-Instruct": Qwen2_5_VLForConditionalGeneration, # Qwen - https://huggingface.co/Qwen/Qwen2.5-VL-7B-Instruct
        "CohereForAI/aya-vision-8b": AutoModelForImageTextToText # Aya Vision - https://huggingface.co/CohereForAI/aya-vision-8b
    }

    if model_id not in MODEL_MAP:
        print(f"Model {model_id} not available.")
        return None, None

    # initiate the right model
    model = MODEL_MAP[model_id].from_pretrained(model_id, device_map="auto", trust_remote_code=True).eval() # use .eval() to switch to evaluation (inference) mode
    processor = AutoProcessor.from_pretrained(model_id, trust_remote_code = True)
    
    print(f"Successfully loaded model and processor with id {model_id}.")
    return model, processor


def start_classification_trns(model, processor, model_id):
    
    if model is None:
        return None

    # load the instructions and questions
    messages = [{"role": "system", "content": [{"type": "text", "text": instructions_new}]}]
    user_content = create_user_content()

    # add other user content (page name, caption)
    user_content.extend([
        {"type": "text", "text": f"Name of the page running the ad: {page_name}"},
        {"type": "text", "text": f"Ad caption: {ad_creative_bodies}"},
        #{"type": "image", "image": base64_image} # for qwen
        {"type": "image", "image": image_path} # for aya
        #{"type": "image_url", "image_url": f"data:image/png;base64,{base64_image}"}
    ])
    messages.append({"role": "user", "content": user_content})

    # prepare the input based on the model being used
    if model_id in TEXT_MODELS: # use the chat template
        inputs = processor.apply_chat_template(
            messages, add_generation_prompt=True, tokenize=True,
            return_dict=True, return_tensors="pt" # pytorch tensor format output for gpu acceleration
        ).to(model.device, dtype=torch.bfloat16) # bfloat16 instead of float16 for less memory consumption (best for inference)

        input_len = inputs["input_ids"].shape[-1] # length of input prompt (to remove)
    
    elif model_id in MULTIMODAL_MODELS: # need separate processing for images
        text_input = processor.apply_chat_template(messages, tokenize=False, add_generation_prompt=True)
        image_inputs, _ = process_vision_info(messages) # qwen requires separate image processing
        inputs = processor(
            text=[text_input], images=image_inputs, padding=True, return_tensors="pt"
        ).to(model.device)
        # use padding to make sure all inputs are the same length (pytorch can't create a tensor otherwise)

    print(f"Starting classification with model {model_id}...")
    start_time = time.time()
    # generate the response (regardless the model)
    with torch.inference_mode(): # optimize inference by disabling gradient calculations to save memory and speed up processing
        generation = model.generate(**inputs, max_new_tokens=1300, do_sample=False, # deterministic generation (not random)
            temperature = 0.1)

    end_time = time.time() 
    print(f"Time taken to generate response: {end_time - start_time:.2f} seconds") 
    print(f"CUDA memory: {torch.cuda.memory_allocated() / 1e9:.2f} GB")

    torch.cuda.empty_cache() # free unused memory

    # decode the response based on the ml being used
    if model_id in MULTIMODAL_MODELS:
        # Qwen requires trimming the input tokens before decoding
        generated_ids_trimmed = [out_ids[len(in_ids):] for in_ids, out_ids in zip(inputs.input_ids, generation)]
        response = processor.batch_decode(generated_ids_trimmed, skip_special_tokens=True, clean_up_tokenization_spaces=False)
        print(response) 
    else:
        response = processor.decode(generation[0][input_len:], skip_special_tokens=True) # for gemma and aya
        print(response) 

    return response


# for aya (different regex) - since it doesn't answer each question, check which labels are missing - NOT TRUE WITH UPDATED INSTRUCTIONS
model_id = TEXT_MODELS[1]
model, processor = initiate_transformers_model(model_id) # initiate the right model and processor

response = start_classification_trns(model, processor, model_id)
answer_dict = process_missing_output(response, expected_labels)
temp = get_final_dict_entry(answer_dict, ad_id)

# for Qwen
model_id = MULTIMODAL_MODELS[0]
model, processor = initiate_transformers_model(model_id) # initiate the right model and processor



## APIs:
response = start_classification_apis(openai_model_id, openai_api_key, openai_api_url)
answers = response.json()['choices'][0]['message']['content']
answer_dict = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in pattern_api.findall(answers)}
temp = get_final_dict_entry(answer_dict, ad_id)

response2 = start_classification_apis(mistral_model_id, mistral_api_key, mistral_api_url)
answers2 = response2.json()['choices'][0]['message']['content']
answer_dict2 = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in pattern_api.findall(answers2)}
temp2 = get_final_dict_entry(answer_dict2, ad_id)


# best models
# 1. Gemma3-4b (6 min), 12b (29 mins), 27b (30 mins)
# 2. Qwen2.5-7b (2 min), 72b very slow (71 minutes) but better output format
# 3. Aya Vision-8b (20 sec), 32b (12 mins)
# 4. MistralAI via API, 12b (14 sec)
# 5. GPT-4o (18 sec)

# Qwen example output: (quite fast, 5-6 seconds)
# ['*AD_TYPE_CLASSIFICATION*: No - The ad is for a restaurant/takeaway/delivery outlet, not a food/drink manufacturing company or brand.\n*MARKETING_STRATEGIES*: Yes - OWNED_CARTOON, CELEBRITY, DISCOUNT\n*PREMIUM_OFFERS*: No - The ad focuses on discounts, not premium offers.\n*TARGET_AGE_GROUP*: No - The ad is not specifically targeted at children or adolescents.\n*WHO_FOOD_CATEGORIES*: Yes - READYMADE_CONVENIENCE\n*SPECULATION_LEVEL*: 2 - The ad clearly promotes a pizza deal, but the specific type of pizza is not detailed, requiring some inference.']


# other models: OmniParser-v2.0
# designed to be able to convert unstructured screenshot image into structured list of elements including interactable regions location and captions of icons on its potential functionality.

