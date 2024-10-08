from openai import OpenAI
import base64
import requests
import os
import re
import pandas as pd
import random
import json

from labeling_validation.WHO_questions_new import *

client = OpenAI()
api_key = os.environ["OPENAI_API_KEY"]
image_folder = "data/unique_images"
AD_PATTERN = re.compile(r"(.+?)\.(png|jpeg|jpg)")
PATTERN_PAYLOAD_OUTPUT = re.compile(r"\*(.*?)\*: ([^\n]+?) - (.*?)(?=\n\*|$)", re.DOTALL)
all_questions = [type_ad, marketing_str, premium_offer, who_cat, processed]

headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {api_key}"
}

# default OpenAI function for encoding an image
def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')
    

def label_images(images, captions):
    results = [] # for the labels
    responses = []
    
    for image in images:
        print(f'======== Labeling image: {image}. ========\n')
        image_path = os.path.join(image_folder, image)
        base64_image = encode_image(image_path)
        image_url = f"data:image/jpeg;base64,{base64_image}"

        ad_id = AD_PATTERN.findall(image)[0][0]
        ad_creative_bodies = captions[captions["img_id"] == ad_id]["ad_creative_bodies"].values[0]
            
        # shuffle the order of the questions
        all_shuffled_questions = random.sample(all_questions, len(all_questions))
            
        shuffled_questions = []
        for main_question in all_shuffled_questions:
            shuffled_questions.extend(main_question)
            
        messages = [{"role": "system", "content": instructions_1}]
        user_content = [{"type": "text", "text": q[0] + ": " + q[1]} for q in shuffled_questions] 
        user_content.append({"type": "image_url", "image_url": {"url": image_url}})
        user_content.append({"type": "text", "text": ad_creative_bodies})
        messages.append({"role": "user", "content": user_content})

        # define payload for the API call
        payload = {
            "model": "gpt-4o",
            "messages": messages,
            "temperature": 0.01
        }
        try:
            response_intro = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)
            print(response_intro.json())

            answers_1 = response_intro.json()['choices'][0]['message']['content']
            answer_dict = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in PATTERN_PAYLOAD_OUTPUT.findall(answers_1)}
            responses.append(answer_dict)

            ad_type, ad_type_explanation = get_ad_type(answer_dict)
            marketing_str, marketing_str_explanation = get_marketing_strategy(answer_dict)
            prem_offer, prem_offer_explanation = get_premium_offer(answer_dict)
            who_cat, who_cat_explanation = get_who_cat(answer_dict)
            processed, processed_explanation = get_processing_level(answer_dict)
                                
            dict_entry = {
                "img_id": ad_id,
                "type_ad": ad_type,
                "type_ad_expl": ad_type_explanation,
                "marketing_str": marketing_str,
                "marketing_str_expl": marketing_str_explanation,
                "prem_offer": prem_offer,
                "prem_offer_expl": prem_offer_explanation,
                "who_cat": who_cat,
                "who_cat_expl": who_cat_explanation,
                "processed": processed,
                "processed_expl": processed_explanation
            }
            print(dict_entry) 
        
        except Exception as e:
            print(f"Error processing the first prompt: {e}.")
            dict_entry = {"img_id": ad_id}
                
        results.append(dict_entry)
    
    labeling_outputs = pd.DataFrame(results)
    labeling_outputs['img_id'] = labeling_outputs['img_id'].astype(str)
    
    return labeling_outputs, responses

# start from here
images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]
# randomly sample 100 images for a little analysis
sampled_images = random.sample(images, 5)
ads_data = pd.read_excel("validation results/digital_coding_clean.xlsx")

img_names = [os.path.splitext(image)[0] for image in sampled_images]
ads_subset = ads_data[ads_data["img_id"].isin(img_names)][["img_id", "ad_creative_bodies"]]

labeling_outputs, responses = label_images(sampled_images, ads_subset)

# for ad_3673407462987001_img the coding seems wrong..?


# ad_1487281292054307_img