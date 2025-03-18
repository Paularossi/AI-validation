import requests
import json
import base64
import os
import time
import re
import random
import torch
from PIL import Image

import ollama
from huggingface_hub import login
from transformers import AutoModelForImageTextToText, Gemma3ForConditionalGeneration, AutoProcessor, LlavaForConditionalGeneration, AutoModelForCausalLM, Qwen2_5_VLForConditionalGeneration
from qwen_vl_utils import process_vision_info

from persistent.AI_validation.labeling_validation.WHO_questions import *


# use the GPU if available
device = "cuda:0" if torch.cuda.is_available() else "cpu"
torch_dtype = torch.float16 if torch.cuda.is_available() else torch.float32

TEXT_MODELS = ["google/gemma-3-12b-it", "llava-hf/llava-1.5-7b-hf"]
MULTIMODAL_MODELS = ["Qwen/Qwen2.5-VL-7B-Instruct"]

pattern2 = re.compile(r"\*{1,2}(.*?)\*{1,2}: ([^\n]+?) [–-] (.*?)(?=\n\*|$)", re.DOTALL)
all_questions = [alcohol, type_ad, marketing_str, premium_offer, who_cat, target_age_group]
image_folder = "persistent/AI_validation/data/unique_images"
images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]


def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")


ad_id = "123"
ad_creative_bodies = "'Shop slim, eet lekker op Black Friday🍕 Enkel vandaag: 20% korting op 1 pizza, 30% op 2 en 40% op 3 of meer🍕!" # "Try our new burger!"
page_name = "Domino's Pizza Belgium"

temp_image = "ad_2016607565369658_img.png" # "ad_186810294481985_img.png"
image_path = os.path.join(image_folder, temp_image)
base64_image = encode_image(image_path)


# =============== APIs ===============

# === add GPT-40 ===

# === Mistral AI ===
mistral_model_id = "pixtral-12b-2409"
mistral_api_url = "https://api.mistral.ai/v1/chat/completions"
mistral_api_key = os.environ["MISTRAL_API_KEY"]


# =============== Ollama ===============

ollama_url = "http://localhost:11434/api/chat"
llava_model_id = "llava:7b"
llama_model_id = "llama3.2-vision:11b"


# =============== Transformers ===============

# Florence - https://huggingface.co/microsoft/Florence-2-large
#florence_model = AutoModelForCausalLM.from_pretrained("microsoft/Florence-2-large", torch_dtype=torch_dtype, trust_remote_code=True).to(device)
#florence_processor = AutoProcessor.from_pretrained("microsoft/Florence-2-large", trust_remote_code=True)

def initiate_transformers_model(model_id):
    """Load the right model and processor based on model_id."""
    # available models
    MODEL_MAP = {
        "google/gemma-3-12b-it": Gemma3ForConditionalGeneration, # Gemma3 - https://huggingface.co/google/gemma-3-12b-it
        "llava-hf/llava-1.5-7b-hf": LlavaForConditionalGeneration, # Llava - https://huggingface.co/llava-hf/llava-1.5-7b-hf 
        "Qwen/Qwen2.5-VL-7B-Instruct": Qwen2_5_VLForConditionalGeneration # Qwen - https://huggingface.co/Qwen/Qwen2.5-VL-7B-Instruct
    }

    if model_id not in MODEL_MAP:
        print(f"Model {model_id} not available.")
        return None, None

    # initiate the right model
    model = MODEL_MAP[model_id].from_pretrained(model_id, device_map="auto").eval() # use .eval() to switch to evaluation (inference) mode
    processor = AutoProcessor.from_pretrained(model_id)
    
    print(f"Successfully loaded model and processor with id {model_id}.")
    return model, processor


# so far works for gemma, llava (?), qwen
def start_classification_trns(model_id):
    #login()

    # initiate the right model and processor
    model, processor = initiate_transformers_model(model_id)
    if model is None:
        return None
    
    # load the instructions and questions
    messages = [{"role": "system", "content": [{"type": "text", "text": instructions_new}]}]
    user_content = create_user_content()

    # add other user content (page name, caption)
    user_content.extend([
        {"type": "text", "text": f"Name of the page running the ad: {page_name}"},
        {"type": "text", "text": f"Ad caption: {ad_creative_bodies}"},
        #{"type": "image", "image": base64_image}
        {"type": "image", "image": image_path}
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

    # generate the response (regardless the model)
    with torch.inference_mode(): # optimize inference by disabling gradient calculations to save memory and speed up processing
        generation = model.generate(**inputs, max_new_tokens=3000, do_sample=False) # deterministic generation (not random)

    # decode the response based on the ml being used
    if model_id in MULTIMODAL_MODELS:
        # Qwen requires trimming the input tokens before decoding
        generated_ids_trimmed = [out_ids[len(in_ids):] for in_ids, out_ids in zip(inputs.input_ids, generation)]
        print(processor.batch_decode(generated_ids_trimmed, skip_special_tokens=True, clean_up_tokenization_spaces=False)) 
    else:
        print(processor.decode(generation[0][input_len:], skip_special_tokens=True)) # for gemma

    #return 0



model_id = TEXT_MODELS[1]

start_classification_trns(model_id)


