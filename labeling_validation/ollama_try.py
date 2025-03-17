import requests
import json
import base64
import os
import ollama
import time
import re

from persistent.AI_validation.labeling_validation.WHO_questions import *

# TO ENABLE THE GPU, log into oc from cmd, then type: 
# oc patch dc/ai-labeling-gpu --type=json -p="[{"op": "replace", "path": "/spec/template/spec/containers/0/resources", "value": {"requests": {"nvidia.com/gpu": 1}, "limits": {"nvidia.com/gpu": 1}}}]"

pattern2 = re.compile(r"\*{1,2}(.*?)\*{1,2}: ([^\n]+?) [–-] (.*?)(?=\n\*|$)", re.DOTALL)
all_questions = [alcohol, type_ad, marketing_str, premium_offer, who_cat, target_age_group]
image_folder = "persistent/AI_validation/data/unique_images"
images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]

def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")


##########################################################################
# ============== TEST GPU ==============
import torch
print("CUDA available:", torch.cuda.is_available())
print("Device count:", torch.cuda.device_count())
print("Using device:", torch.device("cuda" if torch.cuda.is_available() else "cpu"))



# add a branding question (for outdoor ads for sure), maybe besides
# meeting on Thursday 


##################################################################################

# == FIRST MODEL (BEST ONE SO FAR) ========== MISTRAL AI ========== 
# pixtral-12b-2409 - free model
# premium (paid) models: pixtral-large-latest, 
# rate limit: 1,000,000,000 tokens per month

# ========== using Requests ==========
import random
# api_key = os.environ["MISTRAL_API_KEY"]
api_key = input()
mistral_model = "pixtral-12b-2409"

headers = {
    "Authorization": f"Bearer {api_key}",
    "Content-Type": "application/json"
}
api_url = "https://api.mistral.ai/v1/chat/completions"

ad_id = "123"
ad_creative_bodies = "'Shop slim, eet lekker op Black Friday🍕 Enkel vandaag: 20% korting op 1 pizza, 30% op 2 en 40% op 3 of meer🍕!" # "Try our new burger!"
page_name = "Domino's Pizza Belgium"

temp_image = "ad_2016607565369658_img.png" # "ad_186810294481985_img.png"
image_path = os.path.join(image_folder, temp_image)
base64_image = encode_image(image_path)

### NEW REFORMATTED PROMPTING OF THE QUESTIONS (not one by one anymore)
user_content = []
messages = [{"role": "system", "content": instructions_new}]
user_content = create_user_content()

# add other user content (page name, caption)
user_content.append({"type": "text", "text": f"Name of the page running the ad: {page_name}"})
user_content.append({"type": "text", "text": f"Ad caption: {ad_creative_bodies}"})
user_content.append({"type": "image_url", "image_url": f"data:image/png;base64,{base64_image}"})

messages.append({"role": "user", "content": user_content})

###

payload = {
    "model": mistral_model,  # free model
    "messages": messages,
    "temperature": 0.01
}

start_time = time.time()
response = requests.post(api_url, headers = headers, json = payload)
end_time = time.time() 
print(f"Time taken to generate response: {end_time - start_time} seconds") # 18 seconds with gpu

print(response.json())
# in case of a 422 response, to print the error message run print(response.json()["detail"][0]["msg"])


answers_1 = response.json()['choices'][0]['message']['content']
#answer_dict = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in PATTERN_PAYLOAD_OUTPUT.findall(answers_1)}
answer_dict = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in pattern2.findall(answers_1)}
#responses.append(answers_1)

ad_type, ad_type_explanation = get_ad_type(answer_dict)
marketing_str, marketing_str_explanation = get_marketing_strategy(answer_dict)
prem_offer, prem_offer_explanation = get_premium_offer(answer_dict)
who_cat, who_cat_explanation, who_cat_text = get_who_cat(answer_dict)
processed_dict, processed_explanation_dict = get_processing_level_new(answer_dict, who_cat_text, who_cat) # unlike the rest, this is a dict
target_group, target_group_expl = get_target_group(answer_dict)
is_alcohol, is_alcohol_expl = get_alcohol(answer_dict)

dict_entry = {
    "img_id": ad_id,
    "type_ad": ad_type,
    "type_ad_expl": ad_type_explanation,
    "marketing_str": marketing_str,
    "marketing_str_expl": marketing_str_explanation,
    "prem_offer": prem_offer,
    "prem_offer_expl": prem_offer_explanation,
    "target_group": target_group,
    "target_group_expl": target_group_expl,
    "who_cat": who_cat,
    "who_cat_expl": who_cat_explanation,
    "is_alcohol": is_alcohol,
    "is_alcohol_expl": is_alcohol_expl,
    "speculation": answer_dict["SPECULATION_LEVEL"][0],  # added speculation
    "speculation_expl": answer_dict["SPECULATION_LEVEL"][1]
}

# update with the processed dictionary
dict_entry.update(processed_dict)
dict_entry.update(processed_explanation_dict)
print(dict_entry)
# works but needs some adjustments in processing, for example: 'target_group_expl': 'The ad is targeted at a broad adult audience.\n\n### WHO Food Categories'
##################################################################################



###### OLLAMA ############################################################################
# ========== USING Requests ==========

url = "http://localhost:11434/api/chat" # add the local base url
llava_model = "llava:7b"
llama_model = "llama3.2-vision:11b"

messages = [
    {"role": "system", "content": instructions_new},
    #{"role": "user", "content": f"Name of the page running the ad: {page_name}"},
    #{"role": "user", "content": f"Ad caption: {ad_creative_bodies}"},
    {"role": "user", "content": create_user_content_string(page_name, ad_creative_bodies), "images": [base64_image]}
]

payload = {
    "model": llama_model,
    "messages": messages,
    "stream": False,
    "options": {
        "num_gpu_layers": -1  # use all available GPU layers
    }
}

start_time = time.time()
response = requests.post(url, json = payload, stream = False) # use stream = True to print the response as it's generated
end_time = time.time() # took 436 seconds aka around 7-8 mins
print(f"Time taken to generate response: {end_time - start_time} seconds") 
print(response.json())
print(response.content) # completely ignored the instructions and outputs strange answers



# ========== USING Ollama LIBRARY ==========

# manually start ollama with `ollama serve &` (& runs it in the background)
# to check if it's running do `ps aux | grep ollama`

client = ollama.Client(host = "http://localhost:11434") # initialize
image_path = os.path.join(image_folder, images[0])
base64_image = encode_image(image_path)


# calculate the time to generate the response
start_time = time.time()
response = client.generate(model = llama_model, prompt = create_user_content_string(page_name, ad_creative_bodies), stream=False, format="json",
                           system=instructions_new, options={"temperature": 0.01}, images = [base64_image]) # remove the json format????
end_time = time.time()

print(f"Time taken to generate response: {end_time - start_time} seconds") # 188 seconds aka around 3 mins
print(f"Response from Ollama: \n {response.response}") # better output but still strange, missing answers







# ========== using Mistral library ========== (not needed)
# from mistralai import Mistral

# api_key = os.environ["MISTRAL_API_KEY"]
# model = "pixtral-12b-2409"

# client = Mistral(api_key=api_key)

# image_path = os.path.join(image_folder, images[0])
# base64_image = encode_image(image_path)

# chat_response = client.chat.complete(
#     model = model,
#     messages=[
#         {
#             "role": "system",
#             "content": "Answer in maximum 10 words.",
#         },
#         {
#             "role": "user",
#             "content": [
#                 {"type": "text", "text": "What is in this ad image?"},
#                 {"type": "image_url", "image_url": f"data:image/jpeg;base64,{base64_image}"}
#             ],
#         },
#     ],
# )

# print(chat_response.choices[0].message.content)



# =============== OLD CODE ===============
##################################################################################
# ========== USING Transformers ==========

from transformers import AutoProcessor, LlavaForConditionalGeneration, AutoModelForCausalLM 
import torch
from PIL import Image

# Llama is not accessible in the EU
# Llava
model = LlavaForConditionalGeneration.from_pretrained("llava-hf/llava-1.5-7b-hf", torch_dtype=torch.float16, device_map="auto")
processor = AutoProcessor.from_pretrained("llava-hf/llava-1.5-7b-hf")



conversation = [
    {
        "role": "system",
        "content": [
            {"type": "text", "text": instructions_new}
        ]
    },
    {
        "role": "user",
        "content": [
            {"type": "image", "url": f"data:image/jpeg;base64,{base64_image}"},
            {"type": "text", "text": "What is shown in this image?"},
        ],
    },
]

inputs = processor.apply_chat_template(
    conversation,
    add_generation_prompt=True,
    tokenize=True,
    return_dict=True,
    return_tensors="pt"
).to(model.device, torch.float16)

start_time = time.time()
generate_ids = model.generate(**inputs, max_new_tokens=70)

end_time = time.time()

print(f"Time taken to generate response: {end_time - start_time} seconds.\n") # 84 seconds
processor.batch_decode(generate_ids, skip_special_tokens=True)



# =========================================================================
# Gemma 3 (google) - WORKS!!!!!!!!!!!!!!!!!!
# for this model I need to be logged in, via the terminal type `huggingface-cli login` and input the access token
from huggingface_hub import login
login() # add the key to a text file or path variable?

from transformers import AutoModelForImageTextToText, Gemma3ForConditionalGeneration

model_id = "google/gemma-3-12b-it" # or 4b
# "it" stands for instruction-tuned model. alternatively can use "pt" (pre-trained), but "it" is better for this task

model = AutoModelForImageTextToText.from_pretrained(model_id, device_map="auto").eval()
processor = AutoProcessor.from_pretrained(model_id)


messages = [{"role": "system", "content": [{"type": "text", "text": instructions_new}]}]
user_content = create_user_content()

# add other user content (page name, caption)
user_content.append({"type": "text", "text": f"Name of the page running the ad: {page_name}"})
user_content.append({"type": "text", "text": f"Ad caption: {ad_creative_bodies}"})
user_content.append({"type": "image", "image": base64_image})

messages.append({"role": "user", "content": user_content})

inputs = processor.apply_chat_template(
    messages, add_generation_prompt=True, tokenize=True,
    return_dict=True, return_tensors="pt"
).to(model.device, dtype=torch.bfloat16)

input_len = inputs["input_ids"].shape[-1]

start_time = time.time()
with torch.inference_mode():
    generation = model.generate(**inputs, max_new_tokens=2000, do_sample=False)
    generation = generation[0][input_len:]

end_time = time.time()
print(f"Time taken to generate Gemma response: {end_time - start_time} seconds.\n") # 70 seconds

decoded = processor.decode(generation, skip_special_tokens=True)
print(decoded)


answer_dict = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in pattern2.findall(decoded)}

ad_type, ad_type_explanation = get_ad_type(answer_dict)
marketing_str, marketing_str_explanation = get_marketing_strategy(answer_dict)
prem_offer, prem_offer_explanation = get_premium_offer(answer_dict)
who_cat, who_cat_explanation, who_cat_text = get_who_cat(answer_dict)
#processed_dict, processed_explanation_dict = get_processing_level_new(answer_dict, who_cat_text, who_cat) # unlike the rest, this is a dict
target_group, target_group_expl = get_target_group(answer_dict)
is_alcohol, is_alcohol_expl = get_alcohol(answer_dict)

dict_entry = {
    "img_id": ad_id,
    "type_ad": ad_type,
    "type_ad_expl": ad_type_explanation,
    "marketing_str": marketing_str,
    "marketing_str_expl": marketing_str_explanation,
    "prem_offer": prem_offer,
    "prem_offer_expl": prem_offer_explanation,
    "target_group": target_group,
    "target_group_expl": target_group_expl,
    "who_cat": who_cat,
    "who_cat_expl": who_cat_explanation,
    "is_alcohol": is_alcohol,
    "is_alcohol_expl": is_alcohol_expl,
    "speculation": answer_dict["SPECULATION_LEVEL"][0],  # added speculation
    "speculation_expl": answer_dict["SPECULATION_LEVEL"][1]
}
print(dict_entry)