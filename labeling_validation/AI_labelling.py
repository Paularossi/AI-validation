from openai import OpenAI
import base64
import requests
import os
import re
import pandas as pd
import random
import time

### SETUP
client = OpenAI()
api_key = os.environ["OPENAI_API_KEY"]
image_folder = "data/validation sample"
AD_PATTERN = re.compile(r"(\d+)\.(png|jpeg|jpg)")
ANSWER_PATTERN = re.compile(r"Q\d+:\s*([^\s;]+)(?=\s*(?:;|\n|$))")
#ANSWER_PATTERN = re.compile(r'\*(Q\d+)\*: ([0-9A-E]+)') 

headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {api_key}"
}


### QUESTIONS
system_intro = (
    "You will be provided with a picture of an ad, and you will answer to the questions by just providing one number/letter (only one answer) per question, and a short explanation for the answer. "
    "Write your answers like this: *Q1*: ...; *Q2*: ...; *Q3*... and so on."
) # change the instructions here and add symbolic binding to the other questions
# question = ("Is the ad related to a food or drink product or company? \nA1 = Yes \nA2 = No")

instructions = (
    "You will be provided with a picture of an advertisement. "
    "You need to answer the questions by providing a single number/letter (only one answer) per question, along with a short explanation for the answer. "
    "Write your answers like this: *Q1*: [answer]; *Q2*: [answer]; *Q3*: [answer]; and so on. "
    "Ensure that each answer includes a brief explanation of the features in the image that led to your choice."
)

# add about the working class

# general question to check if the image is fitting for analysis
first_check = ("Can you see advertising or brand related material in this image? If yes, describe the brand.")

sub_questions_q1 = [
    ("FOOD OR DRINK", "Is the ad related to a food or drink product? \n1 = Yes \n2 = No"),
    ("TYPE OF PRODUCT SOURCE", "If yes, is the product from a food company/brand, retailer, or restaurant? \n1 = Food company/brand \n2 = Retailer \n3 = Restaurant or takeaway"),
    ("FOOD COMPANY PROMOTION", "If food company/brand, is it promoting a specific product or just the brand? \n1 = Specific product \n2 = Just the brand"),
    ("RETAILER TYPE", "If retailer, is it a supermarket/convenience store or another type of retailer? \n1 = Supermarket or convenience store \n2 = Other type of retailer"),
    ("SUPERMARKET PROMOTION", "If supermarket/convenience store, is it promoting a specific product or just the brand? \n1 = Specific product \n2 = Just the brand"),
    ("RESTAURANT PROMOTION", "If restaurant or takeaway, is it promoting a specific product or just the brand? \n1 = Specific product \n2 = Just the brand"),
    ("NON-FOOD OR DRINK", "If no, is the ad related to a non-food or drink product, alcohol, or infant formula? \n1 = Non-food or drink product \n2 = Alcohol \n3 = Infant formula")
]

sub_questions_q4 = [
    ("Q4a", "Is the product a type of confectionery, baked good, or snack? \n1 = Chocolate and sugar confectionery \n2 = Cakes and pastries \n3a = Savoury snacks (including salted nuts) \n3b = Unsalted nuts \nNo = Next"),
    ("Q4b", "Is the product a type of beverage or dairy product? \n4a = Juices \n4b = Milk drinks \n4c = Energy drinks \n4d = Other beverages (Soft drinks, sweetened beverages) \n4e = Waters, tea and coffee (unsweetened) \n5 = Edible ices \n7 = Yoghurts, sour milk, cream, etc. \n8 = Cheese \nNo = Next"),
    ("Q4c", "Is the product a type of grain, meat, or vegetable? \n6 = Breakfast cereals \n10 = Butter and other fats and oils \n11 = Bread, bread products and crisp breads \n12 = Fresh or dried pasta, rice and grains \n13 = Fresh and frozen meat, poultry, fish and similar +eggs \n14 = Processed meat, poultry, fish and similar \n15 = Fresh and frozen fruit, vegetables and legumes \n16 = Processed fruit, vegetables and legumes \n17 = Sauces, dips and dressings \nNo = Next"),
    ("Q4d", "Is the product alcoholic or non-specified? \nA = Alcohol \nNA = Non applicable company brand with no foods and drinks \nNS = Non-specified")
]

sub_questions_q6 = [
    ("Q6a", "Is the product a type of drink or dairy product? \n1 = Drinks \n10 = Milk and alternatives \n9 = Cheese \nNo = Next"),
    ("Q6b", "Is the product a type of vegetable, fruit, or legume? \n2 = Vegetables \n3 = Fruits \n6 = Legumes \nNo = Next"),
    ("Q6c", "Is the product a type of grain, nut, or meat? \n4 = Bread, whole grain products and potatoes \n5 = Nuts and seeds \n8 = Eggs \n11 = Fish \n12 = Meat \nNo = Next"),
    ("Q6d", "Is the product outside the healthy living triangle or non-applicable? \n13 = Nutrition outside the triangle \nA = Alcohol \nNA = Non applicable company brand with no foods and drinks")
]

# send the questions that need revision
general_questions = [
    ("Q2", "Choose just the number of the marketing strategy used in this ad: "
            "0 = No marketing strategy used "
            "1 = Cartoon/Company owned character e.g. M&Ms "
            "2 = Licensed character e.g. Dora the explorer "
            "3 = Amateur sportsperson e.g. person playing a sport "
            "4 = Celebrity (non-sports) e.g. Jamie Oliver "
            "5 = Movie/cartoon character e.g. Shrek "
            "6 = Famous sportsperson/team "
            "7 = Non-sports/historical events/festivals e.g. Christmas "
            "8a = ‘For kids’ e.g. image of a child, ‘great for school lunches’ "
            "8b = ‘For students’ "
            "9 = Awards e.g. Best Food Award 2014 "
            "10 = Sports event "),
    ("Q3", "Choose just the number of the premium offer type in this ad: "
          "0 = No premium offer used"
          "1 = Game and app downloads "
          "2 = Contests "
          "3 = Pay 2 take 3, or similar "
          "4 = 20%% extra, or similar "
          "5 = Limited edition "
          "6 = Social charity "
          "7 = Gift or collectable "
          "8 = Price discount "
          "9 = Loyalty programs "),
    ("Q5", "How processed is the food in the image? "
            "1 = unprocessed or minimally processed food "
            "2 = processed culinary ingredients "
            "3 = processed food "
            "4 = ultra-processed food "
            "5 = alcohol "
            "NA = Non applicable"),
    ("Q7", "Act as an expert in nutritional science. Try to evaluate the healthiness of this food from 1-10 where 10 is the healthiest."),
    ("Q8", "Act as an expert in nutritional science. Try to evaluate the nutri-score of this food from A to E, where A is the best."),
    ("Q9", "Evaluate the general persuasiveness of this ad from 1-10 where 10 is the most persuasive."),
    ("Q10", "Evaluate the persuasiveness of this ad, from the perspective of children/teenagers, from 1-10 where 10 is the most persuasive.")
]

brand_question = ("Can you see a brand logo in the picture?")
instructions = ("I will now show you a number of categories to which this user may belong to."
                "The categories are preceded by an identifier (e.g. 'A1', 'A2' or 'E1' etc.)."
                "Please select, for each header, the most likely category to which this user belongs to."
                "In your answer present, for each header, the selected identifier."
                "For each header, please indicate what features of the data contributed to your choice.")


# function to encode the image (from https://platform.openai.com/docs/guides/vision)
def encode_image(image_path):
    with open(image_path, "rb") as image_file:
      return base64.b64encode(image_file.read()).decode('utf-8')
    
def get_q1_answer(answer_dict):
    # default values
    type_ad_answer = ""
    type_ad_expl = "Explanation not provided"
    
    # logic to determine the final answer for type_ad based on subquestions
    if answer_dict.get("Q1a", ("", ""))[0] == "1":
        if answer_dict.get("Q1b", ("", ""))[0] == "1":
            if answer_dict.get("Q1c", ("", ""))[0] == "1":
                type_ad_answer = "1"
                type_ad_expl = answer_dict.get("Q1c", ("", ""))[1]
            else:
                type_ad_answer = "3"
                type_ad_expl = answer_dict.get("Q1c", ("", ""))[1]
        elif answer_dict.get("Q1b", ("", ""))[0] == "2":
            if answer_dict.get("Q1d", ("", ""))[0] == "1":
                if answer_dict.get("Q1e", ("", ""))[0] == "1":
                    type_ad_answer = "4"
                    type_ad_expl = answer_dict.get("Q1e", ("", ""))[1]
                else:
                    type_ad_answer = "5"
                    type_ad_expl = answer_dict.get("Q1e", ("", ""))[1]
            else:
                type_ad_answer = ""
                type_ad_expl = "Other type of retailer"
        elif answer_dict.get("Q1b", ("", ""))[0] == "3":
            if answer_dict.get("Q1f", ("", ""))[0] == "1":
                type_ad_answer = "6"
                type_ad_expl = answer_dict.get("Q1f", ("", ""))[1]
            else:
                type_ad_answer = "7"
                type_ad_expl = answer_dict.get("Q1f", ("", ""))[1]
    else:
        if answer_dict.get("Q1g", ("", ""))[0] == "1":
            type_ad_answer = "8"
            type_ad_expl = answer_dict.get("Q1g", ("", ""))[1]
        elif answer_dict.get("Q1g", ("", ""))[0] == "2":
            type_ad_answer = "9"
            type_ad_expl = answer_dict.get("Q1g", ("", ""))[1]
        else:
            type_ad_answer = "10"
            type_ad_expl = answer_dict.get("Q1g", ("", ""))[1]

    return type_ad_answer, type_ad_expl

def get_q4_answer(answer_dict):
    # logic to determine the final answer for Q4 based on subquestions
    if answer_dict.get("Q4a", ("", ""))[0] in ["1", "2", "3a", "3b"]:
        return answer_dict.get("Q4a", ("", ""))[0], answer_dict.get("Q4a", ("", ""))[1]
    elif answer_dict.get("Q4b", ("", ""))[0] in ["4a", "4b", "4c", "4d", "4e", "5", "7", "8"]:
        return answer_dict.get("Q4b", ("", ""))[0], answer_dict.get("Q4b", ("", ""))[1]
    elif answer_dict.get("Q4c", ("", ""))[0] in ["6", "10", "11", "12", "13", "14", "15", "16", "17"]:
        return answer_dict.get("Q4c", ("", ""))[0], answer_dict.get("Q4c", ("", ""))[1]
    elif answer_dict.get("Q4d", ("", ""))[0] in ["A", "NA", "NS"]:
        return answer_dict.get("Q4d", ("", ""))[0], answer_dict.get("Q4d", ("", ""))[1]
    else:
        return "", "No valid answer"

def get_q6_answer(answer_dict):
    # logic to determine the final answer for Q6 based on subquestions
    if answer_dict.get("Q6a", ("", ""))[0] in ["1", "10", "9"]:
        return answer_dict.get("Q6a", ("", ""))[0], answer_dict.get("Q6a", ("", ""))[1]
    elif answer_dict.get("Q6b", ("", ""))[0] in ["2", "3", "6"]:
        return answer_dict.get("Q6b", ("", ""))[0], answer_dict.get("Q6b", ("", ""))[1]
    elif answer_dict.get("Q6c", ("", ""))[0] in ["4", "5", "8", "11", "12"]:
        return answer_dict.get("Q6c", ("", ""))[0], answer_dict.get("Q6c", ("", ""))[1]
    elif answer_dict.get("Q6d", ("", ""))[0] in ["13", "A", "NA"]:
        return answer_dict.get("Q6d", ("", ""))[0], answer_dict.get("Q6d", ("", ""))[1]
    else:
        return "", "No valid answer"
  

def label_image(image_path):
  
    print(f'Labeling image: {image_path}.\n')
    base64_image = encode_image(image_path)
    image_url = f"data:image/jpeg;base64,{base64_image}"

    shuffled_general_questions = random.sample(general_questions, len(general_questions)) # shuffle the questions
    
    insert_position_q1 = random.randint(0, len(shuffled_general_questions))
    shuffled_questions = shuffled_general_questions[:insert_position_q1] + sub_questions_q1 + shuffled_general_questions[insert_position_q1:]

    insert_position_q4 = random.randint(0, len(shuffled_questions))
    shuffled_questions = shuffled_questions[:insert_position_q4] + sub_questions_q4 + shuffled_questions[insert_position_q4:]

    insert_position_q6 = random.randint(0, len(shuffled_questions))
    shuffled_questions = shuffled_questions[:insert_position_q6] + sub_questions_q6 + shuffled_questions[insert_position_q6:]

    messages = [{"role": "system", "content": system_intro}]
    user_content = [{"type": "text", "text": q[0] + ": " + q[1]} for q in shuffled_questions]  # include question number
    user_content.append({"type": "image_url", "image_url": {"url": image_url}})
    messages.append({"role": "user", "content": user_content})
    
    # gpt-4o is the fastest and most affordable model so far published on may 13, 2024
    payload = {
        "model": "gpt-4o",
        "messages": messages,
        "temperature": 0
    }

    response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)

    print(response.json())

    ad_id = AD_PATTERN.findall(image_path)[0][0]

    # get the responses
    try:
      # adjust regex to capture question number, answer, and explanation
      ANSWER_PATTERN = re.compile(r'\*(Q\d+[a-g]?)\*: ([0-9A-E]+); (.*?)(?=\n\*Q\d+[a-g]?\*|\n?$)', re.DOTALL)  # Adjust regex to capture question number, answer, and explanation
      
      #ANSWER_PATTERN = re.compile(r'\*(Q\d+)\*: ([0-9A-E]+); (.*?)(?:\n|$)', re.DOTALL)
      answers = response.json()['choices'][0]['message']['content']
      answer_dict = {match[0]: (match[1], match[2].strip()) for match in ANSWER_PATTERN.findall(answers)}
      
      # get the answer for type_ad
      type_ad_answer, type_ad_expl = get_q1_answer(answer_dict)
      who_cat_answer, who_cat_expl = get_q4_answer(answer_dict)
      healthy_living_answer, healthy_living_expl = get_q6_answer(answer_dict)

      dict_entry = {
            "img_id": ad_id,
            "type_ad": type_ad_answer,
            "type_ad_expl": type_ad_expl,
            "type_ad_expl": answer_dict.get("Q1", ("", ""))[1],
            "promo_char": answer_dict.get("Q2", ("", ""))[0],
            "promo_char_expl": answer_dict.get("Q2", ("", ""))[1],
            "prem_offer": answer_dict.get("Q3", ("", ""))[0],
            "prem_offer_expl": answer_dict.get("Q3", ("", ""))[1],
            "who_cat": who_cat_answer,
            "who_cat_expl": who_cat_expl,
            "processed": answer_dict.get("Q5", ("", ""))[0],
            "processed_expl": answer_dict.get("Q5", ("", ""))[1],
            "healthy_living": healthy_living_answer,
            "healthy_living_expl": healthy_living_expl,
            "healthiness_lvl": answer_dict.get("Q7", ("", ""))[0],
            "healthiness_lvl_expl": answer_dict.get("Q7", ("", ""))[1],
            "nutri_score": answer_dict.get("Q8", ("", ""))[0],
            "nutri_score_expl": answer_dict.get("Q8", ("", ""))[1],
            "persuasion_all": answer_dict.get("Q9", ("", ""))[0],
            "persuasion_all_expl": answer_dict.get("Q9", ("", ""))[1],
            "persuasion_kids": answer_dict.get("Q10", ("", ""))[0],
            "persuasion_kids_expl": answer_dict.get("Q10", ("", ""))[1]
        }
      return dict_entry
    except Exception as e:
       print(f'Exception occured: {e}. Retrying labeling for image {image}.')
       return label_image(image_path)
    

images = os.listdir(image_folder)
row_list = []

for image in images:
    image_path = os.path.join(image_folder, images[14])
    try:
        entry = label_image(image_path)
        row_list.append(entry)
    except Exception as e:
        print(f'Error {e} occured. Retrying after 5 minutes...')
        time.sleep(300) # wait 5 minutes before retrying
        entry = label_image(image_path)
        row_list.append(entry)
        

labeling_outputs = pd.DataFrame(row_list)
labeling_outputs.to_excel(f'data/validation sample/validation results/labelingnew.xlsx', index=False)


entry = label_image(images[0])
print(entry)





### OLD STUFF
type_ad = (
    "Q1: Choose the number that best represents the type of ad in the image:"
    "1 = food or drink product- food company/brand "
    "2 = food or drink product- promoted in advertisement by non-food brand/company/retailer /service/event " 
    "3 = food or drink company or brand (no retailer) without food or drink product "
    "4 = food or drink retailer (supermarket or convenience store) with food or drink product "
    "5 = food or drink retailer (supermarket or convenience store) without food or drink product "
    "6 = food or drink retailer (restaurant or takeaway or fast food) with food or drink product "
    "7 = food or drink retailer (restaurant or takeaway or fast food) without food or drink product "
    "8 = non-food or drink product "
    "9 = alcohol "
    "10 = infant formula, follow-up and growing up milks"
)

promo_character = (
    "Q2: Choose just the number of the marketing strategy used in this ad:"
    "0 = No marketing strategy used "
    "1 = Cartoon/Company owned character e.g. M&Ms "
    "2 = Licensed character e.g. Dora the explorer  "
    "3 = Amateur sportsperson e.g. person playing a sport "
    "4 = Celebrity (non-sports) e.g. Jamie Oliver "
    "5 = Movie/cartoon character e.g. Shrek "
    "6 = Famous sportsperson/team "
    "7 = Non-sports/historical events/festivals e.g. Christmas "
    "8a = ‘For kids’ e.g. image of a child, ‘great for school lunches’ "
    "8b = 'For students' "
    "9 = Awards e.g. Best Food Award 2014 "
    "10 = Sports event "
    "11 = Other "
)

# premium offers
premium_offer = ( 
    "Q3: Choose just the number of the premium offer type in this ad:"
    "0 = No premium offer used"
    "1 = Game and app downloads "
    "2 = Contests "
    "3 = Pay 2 take 3, or similar "
    "4 = 20%% extra, or similar "
    "5 = Limited edition "
    "6 = Social charity "
    "7 = Gift or collectable "
    "8 = Price discount "
    "9 = Loyalty programs "
    "10 = Other "
)

# food product category - not very useful for fast-food, more for snacks
who_cat = (
    "Q4: Categorize the ad into one of these product categories: "
    "1 = Chocolate and sugar confectionery "
    "2 = Cakes and pastries "
    "3a = Savoury snacks (including salted nuts) "
    "3b = Unsalted nuts "
    "4a = Juices "
    "4b = Milk drinks "
    "4c = Energy drinks "
    "4d = Other beverages (Soft drinks, sweetened beverages) "
    "4e = Waters, tea and coffee (unsweetened) "
    "5 = Edible ices "
    "6 = Breakfast cereals "	
    "7 = Yoghurts, sour milk, cream, etc. "
    "8 = Cheese "
    "9 = Ready-made and convenience foods and composite dishes "
    "10 = Butter and other fats and oils "
    "11 = Bread, bread products and crisp breads "
    "12 = Fresh or dried pasta, rice and grains "
    "13 = Fresh and frozen meat, poultry, fish and similar +eggs "
    "14 = Processed meat, poultry, fish and similar "
    "15 = Fresh and frozen fruit, vegetables and legumes "
    "16 = Processed fruit, vegetables and legumes "
    "17 = Sauces, dips and dressings "
    "A = Alcohol "
    "NA. Non applicable company brand with no foods and drinks "
    "NS. Non-specified "
)

processed = (
    "Q5: How processed is the food in the image?"
    "1 = unprocessed or minimally processed food "
    "2 = processed culinary ingredients "
    "3 = processed food "
    "4 = ultra-processed food "
    "5 = alcohol "
    "NA = Non applicable"
)

healthy_living = (
    "Q6: Categorize the ad into one of these healthy living categories: "
    "1 = Drinks "
    "2 = Vegetables "
    "3 = Fruits "
    "4 = Bread, whole grain products and potatoes "
    "5 = Nuts and seeds "
    "6 = Legumes "	
    "7 = Oils and fats "
    "8 = Eggs "
    "9 = Cheese "
    "10 = Milk and alternatives "
    "11 = Fish "
    "12 = Meat "
    "13 = Nutrition outside the triangle "
    "A = Alcohol "
    "NA. Non applicable company brand with no foods and drinks "
)

healthiness_lvl = (
    "Q7: Act as an expert in nutritional science. Try to evaluate the healthiness of this food from 1-10 where 10 is the healthiest."
)

nutri_score = (
    "Q8: Act as an expert in nutritional science. Try to evaluate the nutri-score of this food from A to E, where A is the best."
)

persuasion = (
    "Q9: Evaluate the general persuasiveness of this ad from 1-10 where 10 is the most persuasive."
)

persuasion_kids = (
    "Q10: Evaluate the persuasiveness of this ad, from the perspective of children/teenagers, from 1-10 where 10 is the most persuasive."
)
# play around with this type of questions


#base64_image = encode_image(os.path.join(image_folder, images[0]))
base64_image = encode_image(os.path.join(image_folder, "1666263595546.jpg"))

# gpt-4o is the fastest and most affordable model so far published on may 13, 2024
payload = {
      "model": "gpt-4o",
      "messages": [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "What population group does this ad target? Choose from one of these age ranges: 13-17, 18-24, 25-34, 35-44, 45-54, 55-64, 65+"
            },            
            {
              "type": "image_url",
              "image_url": {
                "url": f"data:image/jpeg;base64,{base64_image}"
              }
            }
          ]
        }
      ],
      "max_tokens": 200,
      "temperature": 0
    }

response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)

print(response.json())