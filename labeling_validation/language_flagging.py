from openai import OpenAI
import requests
import os
import pandas as pd

client = OpenAI()
api_key = os.environ["OPENAI_API_KEY"]

headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {api_key}"
}

# load the data (for the text)
ads_data = pd.read_excel("validation results/digital_coding_clean.xlsx")
captions = ads_data[["img_id", "ad_creative_bodies"]]

instructions = (
    "Flag the language in the text in the following way: if it's French, type FR, if it's Dutch type NL, if it's both type BOTH, if it's English type EN."
)

results = []


for i in range(100, len(captions)):
    
    img_id = captions.iloc[i]["img_id"]
    ad_text = captions.iloc[i]["ad_creative_bodies"]
    
    print(f"Classifying ad {i}...\n")
    
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
        "model": "gpt-4o",
        "messages": messages,
        "temperature": 0.01
    }

    response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)
    print(response.json())
    language_answer = response.json()['choices'][0]['message']['content']
    
    dict_entry = {
        "ad_id": img_id,
        "ad_text": ad_text,
        "language": language_answer
    }
    
    results.append(dict_entry)
    
languages = pd.DataFrame(results)
languages.to_excel('validation results/language_flagging.xlsx', index=False)


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