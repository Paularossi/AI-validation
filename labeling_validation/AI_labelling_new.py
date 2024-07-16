from openai import OpenAI
import base64
import requests
import os
import re
import pandas as pd
import random
import openpyxl
import json

# uncomment the lines below for debugging:
# import sys
# current_dir = os.path.dirname(os.path.abspath(__file__))
# parent_dir = os.path.dirname(current_dir)
# sys.path.append(parent_dir)

from labeling_validation.WHO_questions import *

### SETUP
client = OpenAI()
api_key = os.environ["OPENAI_API_KEY"]
image_folder = "data/validation sample"
AD_PATTERN = re.compile(r"(\d+)\.(png|jpeg|jpg)") 
PATTERN_PAYLOAD_OUTPUT = re.compile(r"\*(.*?)\*: (-?\d+|[A-Za-z0-9]+) - (.*?)(?=\n\*|$)", re.DOTALL)
#PATTERN_SECOND_PAYLOAD = re.compile(r"\*(.*?)\*:\s*(-?\d+[a-zA-Z]?|[A-Za-z0-9]+)\s-\s(.*?)(?=\n\*|\Z)", re.DOTALL)

all_questions = [sub_questions_q1, sub_questions_q2, sub_questions_q3, sub_questions_q4, sub_questions_q5, sub_questions_q6]
all_categories = type_ad + "\n\n" + promo_character + "\n\n" + premium_offer + "\n\n" + who_cat + "\n\n" + processed + "\n\n" + healthy_living

headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {api_key}"
}

# default OpenAI function for encoding an image
def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')
  
  
# first payload - the image + the decision tree questions
def create_first_payload(image, all_questions):
    image_path = os.path.join(image_folder, image)
    base64_image = encode_image(image_path)
    image_url = f"data:image/jpeg;base64,{base64_image}"
    
    # shuffle the order of the questions
    all_shuffled_questions = random.sample(all_questions, len(all_questions))
    
    shuffled_questions = []
    for main_question in all_shuffled_questions:
        shuffled_questions.extend(main_question)
        
    messages = [{"role": "system", "content": instructions_1}]
    user_content = [{"type": "text", "text": q[0] + ": " + q[1]} for q in shuffled_questions] 
    user_content.append({"type": "image_url", "image_url": {"url": image_url}})
    messages.append({"role": "user", "content": user_content})

    # Define payload for the API call
    payload = {
        "model": "gpt-4o",
        "messages": messages,
        "temperature": 0.01
    }
    
    return payload


# second payload - output from the first prompt + original questions
def create_second_payload(first_prompt_answers):
    
    messages = [
        {"role": "system", "content": instructions_2},
        {"role": "user", "content": first_prompt_answers},
        {"role": "user", "content": all_categories}
    ]
    
    second_payload = {
        "model": "gpt-4o",
        "messages": messages,
        "temperature": 0.01
    }

    return second_payload


def process_images(images):
    results_tree = [] # for the coding using the decision tree deduction
    results_two = [] # for the coding using two AI payloads
    responses = []

    for image in images:
        print(f'======== Labeling image: {image}. ========\n')
        first_payload = create_first_payload(image, all_questions)
        response_1 = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=first_payload)
        print(response_1.json())

        if response_1.status_code == 200:
            answers_1 = response_1.json()['choices'][0]['message']['content']
            answer_dict_1 = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in PATTERN_PAYLOAD_OUTPUT.findall(answers_1)}
            
            responses.append(answers_1)

            # transform the answers from the first payload into input for the second one
            first_prompt_answers = "; ".join([f"*{key}*: {value[0]} - {value[1]}" for key, value in answer_dict_1.items()])
            
            second_payload = create_second_payload(first_prompt_answers)
            response_2 = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=second_payload)
            print(response_2.json())
            
            if response_2.status_code == 200:
                answers_2 = response_2.json()['choices'][0]['message']['content']
                answer_dict_2 = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in PATTERN_PAYLOAD_OUTPUT.findall(answers_2)}
            
            ad_id = AD_PATTERN.findall(image)[0][0]
            
            # get the answers
            # first, the decision tree deduction from the first prompt
            q1_answer, q1_explanation = get_q1_answer(answer_dict_1)
            q2_answer, q2_explanation = get_q2_answer(answer_dict_1)
            q3_answer, q3_explanation = get_q3_answer(answer_dict_1)
            q4_answer, q4_explanation = get_q4_answer(answer_dict_1)
            q5_answer, q5_explanation = get_q5_answer(answer_dict_1)
            q6_answer, q6_explanation = get_q6_answer(answer_dict_1)
            
            dict_entry_tree = {
                "img_id": ad_id,
                "type_ad": q1_answer,
                "type_ad_expl": q1_explanation,
                "marketing_str": q2_answer,
                "marketing_str_expl": q2_explanation,
                "prem_offer": q3_answer,
                "prem_offer_expl": q3_explanation,
                "who_cat": q4_answer,
                "who_cat_expl": q4_explanation,
                "processed": q5_answer,
                "processed_expl": q5_explanation,
                "healthy_living": q6_answer,
                "healthy_living_expl": q6_explanation
            }
            
            # second, get the coding from the second prompt
            type_ad, type_ad_expl = answer_dict_2.get("TYPE AD", ("-1", "Answer missing"))
            marketing_str, marketing_str_expl = answer_dict_2.get("MARKETING STRATEGY", ("-1", "Answer missing"))
            premium_offer, premium_offer_expl = answer_dict_2.get("PREMIUM OFFER", ("-1", "Answer missing"))
            who_cat, who_cat_expl = answer_dict_2.get("WHO CATEGORY", ("-1", "Answer missing"))
            processing, processing_expl = answer_dict_2.get("PROCESSING", ("-1", "Answer missing"))
            healthy_living, healthy_living_expl = answer_dict_2.get("HEALTHY LIVING", ("-1", "Answer missing"))

            dict_entry_two = {
                "img_id": ad_id,
                "type_ad": type_ad,
                "type_ad_expl": type_ad_expl,
                "marketing_str": marketing_str,
                "marketing_str_expl": marketing_str_expl,
                "prem_offer": premium_offer,
                "prem_offer_expl": premium_offer_expl,
                "who_cat": who_cat,
                "who_cat_expl": who_cat_expl,
                "processed": processing,
                "processed_expl": processing_expl,
                "healthy_living": healthy_living,
                "healthy_living_expl": healthy_living_expl
            }
            
        else:
            print(f"Error processing {image}: {response_1.status_code}")
            dict_entry_tree = {"img_id": ad_id}
            dict_entry_two = {"img_id": ad_id}
            
        results_tree.append(dict_entry_tree)
        results_two.append(dict_entry_two)

    return results_tree, results_two, responses



images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]
#images = images[0:5]
results_tree, results_two, responses = process_images(images)
with open('data/validation sample/validation results/responses.json', 'w') as f:
    for item in responses:
        json.dump(item, f)
        f.write('\n')

labeling_outputs_tree = pd.DataFrame(results_tree)
labeling_outputs_tree.to_excel('data/validation sample/validation results/temp_tree.xlsx', index=False)
labeling_outputs_tree.to_csv('data/validation sample/validation results/temp_tree_csv.csv', index=False)

labeling_outputs_two = pd.DataFrame(results_two)
labeling_outputs_two.to_excel('data/validation sample/validation results/temp_two.xlsx', index=False)
labeling_outputs_two.to_csv('data/validation sample/validation results/temp_two_csv.csv', index=False)

# still lots of missing answers....