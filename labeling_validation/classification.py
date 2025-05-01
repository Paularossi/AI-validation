import requests
import json
import base64
import os
import time
import re
import random
import pandas as pd
import torch
from PIL import Image

from huggingface_hub import login
from transformers import AutoModelForImageTextToText, Gemma3ForConditionalGeneration, AutoProcessor, LlavaForConditionalGeneration, AutoModelForCausalLM, Qwen2_5_VLForConditionalGeneration
from qwen_vl_utils import process_vision_info

from persistent.AI_validation.labeling_validation.WHO_questions import *

# run this file in the terminal with `nohup python3 -m persistent.AI_validation.labeling_validation.classification`

# use the GPU if available
device = "cuda:0" if torch.cuda.is_available() else "cpu"
torch_dtype = torch.float16 if torch.cuda.is_available() else torch.float32

TEXT_MODELS = ["google/gemma-3-12b-it"]
MULTIMODAL_MODELS = "Qwen/Qwen2.5-VL-32B-Instruct"
API_MODELS = ["gpt-4o", "pixtral-12b-2409"]

AD_PATTERN = re.compile(r"(.+?)\.(png|jpeg|jpg)")

all_questions = [alcohol, type_ad, marketing_str, premium_offer, who_cat, target_age_group]
expected_labels = [label for section in all_questions for (label, _) in section] + ['SPECULATION_LEVEL'] # all question labels

image_folder = "persistent/AI_validation/data/1000_images" # all the images
images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]

# read the keys
with open('persistent/AI_validation/keys.txt') as f:
    json_data = json.load(f)

hugg_key = json_data["huggingface"]



# =============== APIs ===============

# === GPT-4o ===
#client = OpenAI()
openai_model_id = "gpt-4o"
openai_api_url = "https://api.openai.com/v1/chat/completions"
openai_api_key = json_data["openai"]

# === Mistral AI ===
mistral_model_id = "pixtral-12b-2409"
mistral_api_url = "https://api.mistral.ai/v1/chat/completions"
mistral_api_key = json_data["mistralai"]


def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")

# for mistral ai and gpt-4o
def start_classification_apis(model_id, api_key, api_url, image_path, ad_creative_bodies, page_name):
    print(f'======== Labeling image: {image_path}. ========\n')
    
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }

    base64_image = encode_image(image_path)
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
    response_time = end_time - start_time
    print(f"Time taken to generate response: {response_time:.2f} seconds") # 18 seconds with gpu
    print(f"CUDA memory: {torch.cuda.memory_allocated() / 1e9:.2f} GB")

    torch.cuda.empty_cache() # free unused memory
    print(response.json()) # for gpt-4o, this format doesn't work!!!! change the instructions?

    #print(f"Done classifying using model {model_id}!!! YAYYYY !!!!")

    return response, response_time



# =============== Transformers ===============
login(hugg_key) # log into hugging face (gated models)

def initiate_transformers_model(model_id):
    """Load the right model and processor based on model_id."""
    # available models
    MODEL_MAP = { # try bigger
        "google/gemma-3-12b-it": Gemma3ForConditionalGeneration, # Gemma3 - https://huggingface.co/google/gemma-3-12b-it
        "Qwen/Qwen2.5-VL-32B-Instruct": Qwen2_5_VLForConditionalGeneration, # Qwen - https://huggingface.co/Qwen/Qwen2.5-VL-32B-Instruct
        "CohereForAI/aya-vision-32b": AutoModelForImageTextToText # Aya Vision - https://huggingface.co/CohereForAI/aya-vision-32b
    }

    if model_id not in MODEL_MAP:
        print(f"Model {model_id} not available.")
        return None, None

    # initiate the right model
    model = MODEL_MAP[model_id].from_pretrained(model_id, device_map="auto", trust_remote_code=True).eval() # use .eval() to switch to evaluation (inference) mode
    processor = AutoProcessor.from_pretrained(model_id, trust_remote_code = True)
    
    print(f"Successfully loaded model and processor with id {model_id}.")
    return model, processor


def start_classification_trns(model, processor, model_id, image_path, ad_creative_bodies, page_name):

    print(f'======== Labeling image: {image_path}. ========\n')

    base64_image = encode_image(image_path) # for gemma
    # load the instructions and questions
    messages = [{"role": "system", "content": [{"type": "text", "text": instructions_new}]}]
    user_content = create_user_content()

    # add other user content (page name, caption)
    user_content.extend([
        {"type": "text", "text": f"Name of the page running the ad: {page_name}"},
        {"type": "text", "text": f"Ad caption: {ad_creative_bodies}"},
        #{"type": "image", "image": base64_image} # for qwen
        {"type": "image_url", "image_url": f"data:image/png;base64,{base64_image}"} # Gemma
            if model_id.startswith("google") else {"type": "image", "image": image_path} # aya (and qwen?)
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

    start_time = time.time()
    # generate the response (regardless the model)
    with torch.inference_mode(): # optimize inference by disabling gradient calculations to save memory and speed up processing
        generation = model.generate(**inputs, max_new_tokens=1300, do_sample=False, # deterministic generation (not random)
            temperature = 0.1)

    end_time = time.time()
    response_time = end_time - start_time
    print(f"Time taken to generate response: {response_time:.2f} seconds") 
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

    return response, response_time


def label_images(images, captions, model_id, api_key=None, api_url=None):
    model, processor = None, None # define them first, in case we don't need them (for apis)
    
    if model_id not in API_MODELS: # transformers models
        model, processor = initiate_transformers_model(model_id)
        if model is None:
            print(f"Error loading the model {model_id}. Quitting...")
            return None
    print(f"Starting classifying {len(images)} images with model {model_id}...")
    
    results = [] # for the labels
    responses = []
    n = 1 # just to count the images
    
    for image in images:
        image_path = os.path.join(image_folder, image)
        #base64_image = encode_image(image_path)
        #image_url = f"data:image/jpeg;base64,{base64_image}"

        ad_id = AD_PATTERN.findall(image)[0][0]
        ad_creative_bodies = captions[captions["img_id"] == ad_id]["ad_creative_bodies"].values
        page_name = captions[captions["img_id"] == ad_id]["page_name"].values[0]
        
        # check if ad_creative_bodies exists and is not NaN
        if len(ad_creative_bodies) == 0 or pd.isna(ad_creative_bodies):
            ad_creative_bodies = "AD TEXT NOT AVAILABLE"
        else:
            ad_creative_bodies = ad_creative_bodies[0]

        try:
            # check if we're using an API model
            if model_id in API_MODELS:
                response, response_time = start_classification_apis(model_id, api_key, api_url, image_path, ad_creative_bodies, page_name)
                response = response.json()['choices'][0]['message']['content']
                #answer_dict = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in pattern_api.findall(answers)}
            else:  # if it's a local transformer model
                response, response_time = start_classification_trns(model, processor, model_id, image_path, ad_creative_bodies, page_name)
            
            responses.append(response)

            answer_dict = process_missing_output(response, expected_labels)
            dict_entry = get_final_dict_entry(answer_dict, ad_id)
            dict_entry.update({"response_time": round(response_time, 2)})
        except Exception as e:
            print(f"Error processing image {image} due to: {e}.")
            dict_entry = {"img_id": ad_id}
    
        results.append(dict_entry)
        print(f"===== Image {n} out of {len(images)} classified! =====")
        n += 1

    try:
        labeling_outputs = pd.DataFrame(results)
        labeling_outputs['img_id'] = labeling_outputs['img_id'].astype(str)
    except Exception as e:
        print(results)
        print(f"Unable to convert the output to a dataframe. Returning the data as it is.")
        return results, responses

    print(f"DONEEEE classifying {len(images)} images using model {model_id} !!!")
    return labeling_outputs, responses


# start from here
# read the ads data
ads_data = pd.read_excel("persistent/AI_validation/validation results/digital_coding_clean.xlsx")
coded_ads2 = pd.read_excel("persistent/AI_validation/validation results/gpu/pixtral_20250501_130435.xlsx")
coded_ads = pd.read_excel("persistent/AI_validation/validation results/gpu/pixtral_all.xlsx")

#uncoded_images_sample = ((coded_ads[~coded_ads['img_id'].isin(uncoded_ads['img_id'])]['img_id']).astype(str) + ".png").tolist()
#len(uncoded_images_sample)
coded_images = (coded_ads["img_id"].astype(str) + ".png").tolist() # code the same 50 images
coded_images2 = (coded_ads2["img_id"].astype(str) + ".png").tolist()
all_coded_images = set(coded_images) | set(coded_images2) 

uncoded_images_sample = [img for img in images if img not in all_coded_images]
# randomly sample
uncoded_images = random.sample(uncoded_images_sample, 200)

img_names = [os.path.splitext(image)[0] for image in uncoded_images] # change all_image_ids to sampled_images
captions = ads_data[ads_data["img_id"].isin(img_names)][["img_id", "ad_creative_bodies", "page_name"]]

labeling_outputs, responses = label_images(uncoded_images, captions, model_id=mistral_model_id, api_key=mistral_api_key, api_url=mistral_api_url)
#labeling_outputs, responses = label_images(uncoded_images, captions, model_id=TEXT_MODELS[0])

from datetime import datetime
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S") # for the filename
filename = f"pixtral_{timestamp}.xlsx"
labeling_outputs.to_excel(f"persistent/AI_validation/validation results/gpu/{filename}", index=False)



# best models
# 1. GPT-4o (18 sec) 
# 2. Qwen2.5 32b (~30 mins), 72b (71 minutes)
# 3. MistralAI via API, 12b (14 sec)
# 4. Gemma3 12b (29 mins), 27b (30 mins)

# other models: OmniParser-v2.0
# designed to be able to convert unstructured screenshot image into structured list of elements including interactable regions location and captions of icons on its potential functionality.

