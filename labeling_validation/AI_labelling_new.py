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
PATTERN_PAYLOAD_OUTPUT = re.compile(r"\*{1,2}(.*?)\*{1,2}: ([^\n]+?) - (.*?)(?=\n\*|$)", re.DOTALL) # for 2 sets of stars
all_questions = [alcohol, type_ad, marketing_str, premium_offer, who_cat, target_age_group]


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
    #image = all_image_ids[1] # remove this
    
    for image in images:
        print(f'======== Labeling image: {image}. ========\n')
        image_path = os.path.join(image_folder, image)
        base64_image = encode_image(image_path)
        image_url = f"data:image/jpeg;base64,{base64_image}"

        ad_id = AD_PATTERN.findall(image)[0][0]
        ad_creative_bodies = captions[captions["img_id"] == ad_id]["ad_creative_bodies"].values[0]
        page_name = captions[captions["img_id"] == ad_id]["page_name"].values[0]
        
        # check if ad_creative_bodies exists and is not NaN
        if len(ad_creative_bodies) == 0 or pd.isna(ad_creative_bodies):
            ad_creative_bodies = "AD TEXT NOT AVAILABLE"
        else:
            ad_creative_bodies = ad_creative_bodies[0]
    
        # shuffle the order of the questions
        all_shuffled_questions = random.sample(all_questions, len(all_questions))

        shuffled_questions = []
        for main_question in all_shuffled_questions:
            shuffled_questions.extend(main_question)
            
        # add UPF
        #shuffled_questions.extend(processed)
        # add speculation
        shuffled_questions.extend(speculation)
            
        messages = [{"role": "system", "content": instructions_1}]
        user_content = [{"type": "text", "text": q[0] + ": " + q[1]} for q in shuffled_questions] 
        user_content.append({"type": "image_url", "image_url": {"url": image_url}})
        user_content.append({"type": "text", "text": "Page name: " + page_name})
        user_content.append({"type": "text", "text": "Ad caption: " + ad_creative_bodies})
        messages.append({"role": "user", "content": user_content})

        # define payload for the API call
        payload = {
            "model": "gpt-4o",
            "messages": messages,
            "temperature": 0.01
        }
        
        try:
            response_intro = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)
            if response_intro.status_code != 200:
                print(f"Error: Received status code {response_intro.status_code} for image {image}")
                print(f"Response text: {response_intro.text}")
                raise ValueError(f"API returned status code: {response_intro.status_code}")
            
            print(response_intro.json())

            answers_1 = response_intro.json()['choices'][0]['message']['content']
            answer_dict = {match[0].strip(): (match[1].strip(), match[2].strip()) for match in PATTERN_PAYLOAD_OUTPUT.findall(answers_1)}
            responses.append(answer_dict)

            ad_type, ad_type_explanation = get_ad_type(answer_dict)
            marketing_str, marketing_str_explanation = get_marketing_strategy(answer_dict)
            prem_offer, prem_offer_explanation = get_premium_offer(answer_dict)
            who_cat, who_cat_explanation, who_cat_text = get_who_cat(answer_dict)
            processed_dict, processed_explanation_dict = get_processing_level_new(answer_dict, who_cat_text, who_cat)
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
                #"processed": processed,
                #"processed_expl": processed_explanation,
                "is_alcohol": is_alcohol,
                "is_alcohol_expl": is_alcohol_expl,
                "speculation": answer_dict["SPECULATION_LEVEL"][0], # added speculation
                "speculation_expl": answer_dict["SPECULATION_LEVEL"][1]
            }
            
            # update with the processed dictionary
            dict_entry.update(processed_dict)
            dict_entry.update(processed_explanation_dict)
            print(dict_entry) 
        
        except Exception as e:
            print(f"Error processing the first prompt: {e}.")
            dict_entry = {"img_id": ad_id}
                
        results.append(dict_entry)
    
    try:
        labeling_outputs = pd.DataFrame(results)
        labeling_outputs['img_id'] = labeling_outputs['img_id'].astype(str)
    except Exception as e:
        print(results)
        print(f"Unable to convert the output to a dataframe. Returning the data as it is.")
        return results, responses
    
    return labeling_outputs, responses


# start from here
images = [file for file in os.listdir(image_folder) if file.lower().endswith(('.jpg', '.jpeg', '.png'))]

# read the ads data
ads_data = pd.read_excel("validation results/digital_coding_clean.xlsx")
coded_ads = pd.read_excel("validation results/labeling_outputs.xlsx")
ads = coded_ads["img_id"].tolist() # to exclude these images

uncoded_images = [image for image in images if os.path.splitext(image)[0] not in ads]
# randomly sample 50 images for analysis
sampled_images = random.sample(uncoded_images, 10)

sampled_images = ['ad_365774349852854_img.png', 'ad_1117594759672868_4_img.png', 'ad_1351354515801982_img.png', 'ad_724883825637062_img.png', 'ad_1357155365155188_img.png']

# my own sample of coded ads:
my_ads = pd.read_csv("data/validation sample/survey-ai/myads.csv")
image_columns = [f'Image{i}_ID' for i in range(1, 6)]
all_image_ids = my_ads[image_columns].values.flatten().tolist()

# convert the dataframe to a long list 
long_list = []

for i in range(1, 6):
    # create a dictionary mapping original column names to new names
    col_map = {
        f'Image{i}_ID': 'image_id',
        f'alcohol_image{i}': 'alcohol',
        f'non_alcohol_image{i}': 'non_alcohol',
        f'prem_offer_image{i}': 'prem_offer',
        f'target_group_image{i}': 'target_group',
        f'ad_type_image{i}': 'ad_type',
        f'marketing_str_image{i}': 'marketing_str',
        f'who_cat_image{i}': 'who_cat'
    }
    
    # Select only the columns we need for image i and rename them
    temp_df = my_ads[list(col_map.keys())].rename(columns=col_map)
    
    temp_df['image_number'] = i
    
    long_list.append(temp_df)

df_long = pd.concat(long_list, ignore_index=True)

img_names = [os.path.splitext(image)[0] for image in all_image_ids] # change to in sampled_images
ads_subset = ads_data[ads_data["img_id"].isin(img_names)][["img_id", "ad_creative_bodies", "page_name"]]

labeling_outputs, responses = label_images(sampled_images, ads_subset)
labeling_outputs.to_excel('validation results/temppppp.xlsx', index=False)







# save the images from the analysis to a separate folder (for a manual check):
import shutil
dest = "validation results/miscl/marketing/predicted 7 actual 0"
folder = "validation results/images"
#dest = "validation results/images"
images_to_copy = pd.read_excel(os.path.join(dest, "pred_7_act_0_markt.xlsx"))
image_ids = images_to_copy['img_id'].astype(str) + ".png"
len(image_ids)

for image_id in image_ids:
    source_image_path = os.path.join(folder, image_id)
    
    if os.path.exists(source_image_path):
        destination_image_path = os.path.join(dest, image_id)
        if not os.path.exists(destination_image_path):
            shutil.copy(source_image_path, destination_image_path) # change to copy if needed
            
            print(f"Copied: {source_image_path} -> {destination_image_path}")
        else:
            print("=========IMAGE WAS ALREADY COPIED========")
    else:
        print(f"Image not found: {source_image_path}")
