import requests
import os
import pandas as pd
import json

with open('keys.txt') as f:
    json_data = json.load(f)
    
gpt_api_key = os.environ["OPENAI_API_KEY"]
gpt_api_url = "https://api.openai.com/v1/chat/completions"
gpt_model_id = "gpt-4o"

mistral_model_id = "mistral-small-latest"
mistral_api_url = "https://api.mistral.ai/v1/chat/completions"
mistral_api_key = json_data["mistralai"]

# load the data (for the text)
ads_data = pd.read_excel("validation results/digital_coding_clean.xlsx")
captions = ads_data[["img_id", "ad_creative_bodies"]]


def flag_language(api_model_id, api_url, api_key, captions, n = len(captions)):
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }

    instructions = (
        "Flag the language in the text in the following way: if it's French - type FR, if it's Dutch - type NL, if it's both - type BOTH, if it's English - type EN. Answer only with the language code. "
    )

    results = []

    for i in range(n):
        
        img_id = captions.iloc[i]["img_id"]
        ad_text = captions.iloc[i]["ad_creative_bodies"]
        
        print(f"Classifying ad {i} out of {n}...\n")
        
        if len(ad_text) == 0 or pd.isna(ad_text):
            dict_entry = {
                "ad_id": img_id,
                "language": "NONE"
            }
            continue

        messages = [{"role": "system", "content": instructions}]
        user_content = [{"type": "text", "text": "Ad caption: " + ad_text}] 
        messages.append({"role": "user", "content": user_content})

        # define payload for the API call
        payload = {
            "model": api_model_id,
            "messages": messages,
            "temperature": 0.01
        }

        response = requests.post(api_url, headers=headers, json=payload)
        #print(response.json())
        language_answer = response.json()['choices'][0]['message']['content']
        
        dict_entry = {
            "ad_id": img_id,
            "ad_text": ad_text,
            "language": language_answer
        }
        
        results.append(dict_entry)
        
    languages = pd.DataFrame(results)
    return languages


languages = flag_language(gpt_model_id, gpt_api_url, gpt_api_key, captions)
languages.to_excel('data/language_flagging.xlsx', index=False)


############ SPLIT DATASET BY LANGUAGES
data_old = pd.read_csv('data/images.csv')
data_old.head

french = data_old[data_old['language'] == "FR"]
dutch = data_old[data_old['language'] == "NL"]
eng = data_old[data_old['language'] == "EN"]
both = data_old[data_old['language'] == "BOTH"]
none = data_old[data_old['language'] == "NONE"]

#none.reset_index(drop=True, inplace=True)
split = len(both) // 2
both_shuffled = both.sample(frac=1, random_state=666).reset_index(drop=True)  # Shuffle with a fixed random state for reproducibility

both_half1 = both_shuffled.iloc[:split].reset_index(drop=True)
both_half2 = both_shuffled.iloc[split:].reset_index(drop=True)

french_combined = pd.concat([french, both_half1], ignore_index=True)
dutch_combined = pd.concat([dutch, both_half2], ignore_index=True)
eng_combined = pd.concat([eng, none], ignore_index=True)

french_combined.to_csv('data/images_fr.csv', index=False)
dutch_combined.to_csv('data/images_nl.csv', index=False)
eng_combined.to_csv('data/images_en.csv', index=False)