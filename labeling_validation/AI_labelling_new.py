from openai import OpenAI
import base64
import requests
import os
import re
import pandas as pd
import random
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
image_folder = "data/outside ads"
AD_PATTERN = re.compile(r"(\d+)\.(png|jpeg|jpg)") 
#PATTERN_PAYLOAD_OUTPUT = re.compile(r"\*(.*?)\*: (-?\d+|[A-Za-z0-9]+) - (.*?)(?=\n\*|$)", re.DOTALL)
PATTERN_PAYLOAD_OUTPUT = re.compile(r"\*(.*?)\*: ([^\n]+?) - (.*?)(?=\n\*|$)", re.DOTALL)
#PATTERN_SECOND_PAYLOAD = re.compile(r"\*(.*?)\*:\s*(-?\d+[a-zA-Z]?|[A-Za-z0-9]+)\s-\s(.*?)(?=\n\*|\Z)", re.DOTALL)

all_questions = [sub_questions_q1, sub_questions_q2, sub_questions_q3, sub_questions_q4, sub_questions_q5]
all_categories = [type_ad, marketing_str, premium_offer, who_cat, processed]

headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {api_key}"
}

# default OpenAI function for encoding an image
def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')
  
  
# intro payload - the image and sanity check whether it contains an ad
def create_intro_check(image, question_intro):
    image_path = os.path.join(image_folder, image)
    base64_image = encode_image(image_path)
    image_url = f"data:image/jpeg;base64,{base64_image}"
    
    messages = [{"role": "system", "content": instructions_intro}]
    user_content = [{"type": "text", "text": q[0] + ": " + q[1]} for q in questions_intro] 
    user_content.append({"type": "image_url", "image_url": {"url": image_url}})
    messages.append({"role": "user", "content": user_content})
    
    payload = {
        "model": "gpt-4o",
        "messages": messages,
        "temperature": 0.01
    }
    
    return payload


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

    # define payload for the API call
    payload = {
        "model": "gpt-4o",
        "messages": messages,
        "temperature": 0.01
    }
    
    return payload


# second payload - output from the first prompt + original questions
def create_second_payload(first_prompt_answers):
    random.shuffle(all_categories)
    shuffled_categories = "\n\n".join(all_categories)
    
    messages = [
        {"role": "system", "content": instructions_2},
        {"role": "user", "content": first_prompt_answers},
        {"role": "user", "content": shuffled_categories}
    ]
    
    second_payload = {
        "model": "gpt-4o",
        "messages": messages,
        "temperature": 0.01
    }

    return second_payload


def process_second_payload(first_prompt_answers):
    second_payload = create_second_payload(first_prompt_answers)
    response_2 = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=second_payload)
    print(response_2.json())
            
    if response_2.status_code == 200:
        answers_2 = response_2.json()['choices'][0]['message']['content']
        answer_dict_2 = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in PATTERN_PAYLOAD_OUTPUT.findall(answers_2)}
            
    # second, get the coding from the second prompt
    type_ad, type_ad_expl = answer_dict_2.get("TYPE AD", ("-1", "Answer missing"))
    marketing_str, marketing_str_expl = answer_dict_2.get("MARKETING STRATEGY", ("-1", "Answer missing"))
    premium_offer, premium_offer_expl = answer_dict_2.get("PREMIUM OFFER", ("-1", "Answer missing"))
    who_cat, who_cat_expl = answer_dict_2.get("WHO CATEGORY", ("-1", "Answer missing"))
    processing, processing_expl = answer_dict_2.get("PROCESSING", ("-1", "Answer missing"))
    #healthy_living, healthy_living_expl = answer_dict_2.get("HEALTHY LIVING", ("-1", "Answer missing"))

    # if there are missing values, just rerun the second prompt
    if "-1" in [type_ad, marketing_str, premium_offer, who_cat, processing]: # , healthy_living
        print("Missing answers. Rerunning the second prompt...\n")
        answer_dict_2 = process_second_payload(first_prompt_answers)
    
    return answer_dict_2


def process_images(images):
    results_intro = [] # for sanity check and brand retrieval
    results_tree = [] # for the coding using the decision tree deduction
    results_two = [] # for the coding using two AI payloads
    responses_intro = []
    responses_first_prompt = []
    responses_second_prompt = []

    for image in images:
        print(f'======== Labeling image: {image}. ========\n')
        ad_id = AD_PATTERN.findall(image)[0][0]
        
        # create intro payload to check if it's an ad
        intro_payload = create_intro_check(image, questions_intro)
        
        try:
            response_intro = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=intro_payload)
            print(response_intro.json())
            
            if response_intro.status_code == 200:
                answers_1 = response_intro.json()['choices'][0]['message']['content']
                answer_dict_1 = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in PATTERN_PAYLOAD_OUTPUT.findall(answers_1)}
                
                responses_intro.append(answer_dict_1)
                
                is_ad_answer, is_ad_expl = get_intro_answer(answer_dict_1)
                brand, brand_expl = answer_dict_1.get("BRAND", ("-1", "Answer missing"))
                
                dict_entry_intro = {
                    "img_id": ad_id,
                    "is_ad": is_ad_answer,
                    "is_ad_expl": is_ad_expl,
                    "brand": brand,
                    "brand_expl": brand_expl
                }
            
        except Exception as e:
            print(f"Error processing the intro prompt: {e}.")
            dict_entry_intro = {"img_id": ad_id}
        
        results_intro.append(dict_entry_intro)
        
        # run the other prompts only if it's an ad
        if "is_ad" in dict_entry_intro and dict_entry_intro["is_ad"] == '1':
            print("The image contains an advertisement. Starting further analysis...")
            
            first_payload = create_first_payload(image, all_questions)
            try:
                response_1 = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=first_payload)
                print(response_1.json())

                if response_1.status_code == 200:
                    answers_1 = response_1.json()['choices'][0]['message']['content']
                    answer_dict_1 = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in PATTERN_PAYLOAD_OUTPUT.findall(answers_1)}
                    
                    responses_first_prompt.append(answer_dict_1)
                    
                    # get the answers
                    # first, the decision tree deduction from the first prompt
                    q1_answer, q1_explanation = get_q1_answer(answer_dict_1)
                    q2_answer, q2_explanation = get_q2_answer(answer_dict_1)
                    q3_answer, q3_explanation = get_q3_answer(answer_dict_1)
                    q4_answer, q4_explanation = get_q4_answer(answer_dict_1)
                    q5_answer, q5_explanation = get_q5_answer(answer_dict_1)
                    #q6_answer, q6_explanation = get_q6_answer(answer_dict_1)
                    
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
                        "processed_expl": q5_explanation
                        #"healthy_living": q6_answer,
                        #"healthy_living_expl": q6_explanation
                    }
                    
                    # transform the answers from the first payload into input for the second one
                    first_prompt_answers = "; ".join([f"*{key}*: {value[0]} - {value[1]}" for key, value in answer_dict_1.items()])
                    try: 
                        answer_dict_2 = process_second_payload(first_prompt_answers)
                        responses_second_prompt.append(answer_dict_2)
                        
                        type_ad, type_ad_expl = answer_dict_2.get("TYPE AD", ("-1", "Answer missing"))
                        marketing_str, marketing_str_expl = answer_dict_2.get("MARKETING STRATEGY", ("-1", "Answer missing"))
                        premium_offer, premium_offer_expl = answer_dict_2.get("PREMIUM OFFER", ("-1", "Answer missing"))
                        who_cat, who_cat_expl = answer_dict_2.get("WHO CATEGORY", ("-1", "Answer missing"))
                        processing, processing_expl = answer_dict_2.get("PROCESSING", ("-1", "Answer missing"))
                        #healthy_living, healthy_living_expl = answer_dict_2.get("HEALTHY LIVING", ("-1", "Answer missing"))
                            
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
                            #"healthy_living": healthy_living,
                            #"healthy_living_expl": healthy_living_expl
                        }
                        
                    except Exception as e:
                        print(f"Error processing the second prompt: {e}.")
                        dict_entry_two = {"img_id": ad_id}
                else:
                    print(f"Error processing {image}: {response_1.status_code}")
                    dict_entry_tree = {"img_id": ad_id}
                    dict_entry_two = {"img_id": ad_id}
            except Exception as e:
                print(f"Error processing the first prompt: {e}.")
                dict_entry_tree = {"img_id": ad_id}
                dict_entry_two = {"img_id": ad_id}
                
            results_tree.append(dict_entry_tree)
            results_two.append(dict_entry_two)
        
        else:
            print("The image does not contain an advertisement.")
    
    
    labeling_outputs_intro = pd.DataFrame(results_intro)
    labeling_outputs_intro['img_id'] = labeling_outputs_intro['img_id'].astype(str)
    
    labeling_outputs_tree = pd.DataFrame(results_tree)
    labeling_outputs_tree['img_id'] = labeling_outputs_tree['img_id'].astype(str)
        
    labeling_outputs_two = pd.DataFrame(results_two)
    labeling_outputs_two['img_id'] = labeling_outputs_two['img_id'].astype(str)

    return labeling_outputs_intro, responses_intro, labeling_outputs_tree, labeling_outputs_two, responses_first_prompt, responses_second_prompt



images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]
# randomly sample 100 images for a little analysis
sampled_images = random.sample(images, 10)

labeling_outputs_intro, responses_intro, labeling_outputs_tree, labeling_outputs_two, responses_first_prompt, responses_second_prompt = process_images(sampled_images)

merged_intro_tree = labeling_outputs_intro.merge(labeling_outputs_tree, on='img_id', how='left')
merged_intro_two = labeling_outputs_intro.merge(labeling_outputs_two, on='img_id', how='left')

# save the merged dataframes
merged_intro_tree.to_excel('validation results/temp_intro_tree3.xlsx', index=False)
merged_intro_two.to_excel('validation results/temp_intro_two3.xlsx', index=False)


image=sampled_images[9]




######## DO THIS ONLY ONCE
""" # load all outside ads
import shutil
outside_path = "data/outside ads all"
all_path = "data/outside ads"
folders = os.listdir(outside_path)

# extract images from each folder
for folder_name in os.listdir(outside_path):
    folder_path = os.path.join(outside_path, folder_name)
    if os.path.isdir(folder_path):
        # loop over each file in the folder
        for file_name in os.listdir(folder_path):
            file_path = os.path.join(folder_path, file_name)
            
            if file_name.lower().endswith(('.png', '.jpg', '.jpeg', '.gif', '.bmp', '.tiff')):
                shutil.copy(file_path, all_path)
                print(f"Copied {file_name} to {all_path}")

print(f"Done copying {len(os.listdir(all_path))} images.")

# keep only the images for the cleaned coding data
import os
import pandas as pd

img_ids = pd.read_excel("validation results/original_coding.xlsx")['img_id'].astype(str)
img_id_set = set(img_ids)

image_folder = 'data/outside ads'

# cleanup images in the folder
for image_file in os.listdir(image_folder):
    img_id_from_file = os.path.splitext(image_file)[0]
    
    # if the image file name is not in the img_id_set remove it
    if img_id_from_file not in img_id_set:
        os.remove(os.path.join(image_folder, image_file))
        print(f"Removed: {image_file}")

print("Folder cleanup complete.") """
###########################