import requests
import json
import base64
import os
import ollama
import time
import re

from labeling_validation.WHO_questions import *

pattern2 = re.compile(r"\*{1,2}(.*?)\*{1,2}: ([^\n]+?) [–-] (.*?)(?=\n\*|$)", re.DOTALL)
all_questions = [alcohol, type_ad, marketing_str, premium_offer, who_cat, target_age_group]
url = "http://localhost:11434/api/generate" # add the local base url
image_folder = "data/unique_images"
images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]

def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")

# i downloaded LLaVA (llava:7b), lama
payload = {
    "model": "llava", # replace with the model i'm using
    "messages": [{"role": "user", "content": "What is Python in max 20 words?"}]
}

response = requests.post(url, json = payload, stream = True) # use stream = True to print the response as it's generated

if response.status_code == 200:
    print("Streaming response from Ollama:")
    for line in response.iter_lines(decode_unicode = True):
        if line:
            try:
                json_data = json.loads(line)
                
                # extract and print the message
                if "message" in json_data and "content" in json_data["message"]:
                    print(json_data["message"]["content"], end = "")
                
            except json.JSONDecodeError:
                print(f"\nFailed to parse line: {line}")




# ========== USING Ollama LIBRARY ==========

instructions_1 = (
    "You will be provided with a picture of an online advertisement delivered to Belgium/Netherlands, its corresponding text (which may be in English, French, or Dutch), and the name of the company running the ad. "
    "You will be given sets of questions of questions about various aspects of the advertisement along with definitions and examples. "
    "Please answer each question using the exact format: *QUESTION_LABEL*: Yes/No - Brief explanation. Do not include any extra text, greetings, or commentary. "
    "For example, your answers should look like: *CARTOON*: Yes/No - explanation; *CELEBRITY*: Yes/No - explanation; and so on. Ensure that the question label is between a set of stars. "
    "Ensure that each answer includes a brief explanation of the features in the image/text that led to your choice. Ensure that you answer all questions. "
)


client = ollama.Client(host = "http://localhost:11434") # initialize
model = "" # ollama run llava:7b, llava-phi3
prompt = "What is in this image?"
image_path = os.path.join(image_folder, images[0])
base64_image = encode_image(image_path)


# calculate the time to generate the response
start_time = time.time()
response = client.generate(model = model, prompt = prompt, stream=False, format="json",
                           system="Answer in maximum 10 words.", options={"temperature": 0.01}, images = [base64_image])
end_time = time.time()

print(f"Time taken to generate response: {end_time - start_time} seconds")

# raise ResponseError(e.response.text, e.response.status_code) from None
# ollama._types.ResponseError: model requires more system memory (4.8 GiB) than is available (3.8 GiB) (status code: 500)

response = client.generate(model = model, prompt = target_age_group, stream = False, format = "json", system = instructions_1,
                           options={"temperature": 0.01}, images = [base64_image])

print(f"Response from Ollama: \n {response.response}")



# ========== MISTRAL AI ==========
# pixtral-12b-2409 - free model
# premium (paid) models: pixtral-large-latest, 
# rate limit: 1,000,000,000 tokens per month
from mistralai import Mistral

api_key = os.environ["MISTRAL_API_KEY"]
model = "pixtral-12b-2409"

client = Mistral(api_key=api_key)

image_path = os.path.join(image_folder, images[0])
base64_image = encode_image(image_path)

chat_response = client.chat.complete(
    model = model,
    messages=[
        {
            "role": "system",
            "content": "Answer in maximum 10 words.",
        },
        {
            "role": "user",
            "content": [
                {"type": "text", "text": "What is in this ad image?"},
                {"type": "image_url", "image_url": f"data:image/jpeg;base64,{base64_image}"}
            ],
        },
    ],
)

print(chat_response.choices[0].message.content)


##################################################################################
# ========== using Requests ==========
import random
api_key = os.environ["MISTRAL_API_KEY"]
headers = {
    "Authorization": f"Bearer {api_key}",
    "Content-Type": "application/json"
}
api_url = "https://api.mistral.ai/v1/chat/completions"

ad_id = "123"
ad_creative_bodies = "Try our new burger!"
page_name = "McDonald's"

temp_image = "ad_186810294481985_img.png"
image_path = os.path.join(image_folder, temp_image)
base64_image = encode_image(image_path)

#all_questions = [type_ad]  # Add other categories here if needed
# all_shuffled_questions = random.sample(all_questions, len(all_questions))

# shuffled_questions = []
# for main_question in all_shuffled_questions:
#     shuffled_questions.extend(main_question)

# messages = [{"role": "system", "content": instructions_1}]
# user_content = [{"type": "text", "text": q[0] + ": " + q[1]} for q in shuffled_questions] 
# user_content.append({"type": "text", "text": "Name of the page running the ad: " + page_name})
# user_content.append({"type": "text", "text": "Ad caption: " + ad_creative_bodies}) 
# user_content.append({"type": "image_url", "image_url": f"data:image/png;base64,{base64_image}"})

# messages.append({"role": "user", "content": user_content})


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
    "model": "pixtral-12b-2409",  # free model
    "messages": messages,
    "temperature": 0.01
}

response = requests.post(api_url, headers = headers, json = payload)

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