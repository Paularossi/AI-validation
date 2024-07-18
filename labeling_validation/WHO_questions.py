instructions_1 = (
    "You will be provided with a picture of an advertisement. "
    "You need to answer the questions by providing a single number/letter (only one answer) per question, along with a short explanation for the answer. "
    "Write your answers like this: *FOOD OR DRINK*: [answer] - [explanation]; *TYPE OF PRODUCT SOURCE*: [answer] - [explanation]; and so on. "
    "If a question is not relevant based on a previous answer, you should write -1 for the answer and 'NOT RELEVANT' for the explanation, but every question must be answered. "
    "Ensure that each answer includes a brief explanation of the features in the image that led to your choice. "
    "If the questions is not relevant, do not provide an explanation, just write the answer like this: *TYPE OF STRATEGY*: -1 - NOT RELEVANT."
)

instructions_2 = (
    "You will be provided with detailed answers from a previous set of questions regarding an ad image. "
    "Based on this information, categorize the ad into one of the specified categories by providing a single number/letter (only one answer) per question. "
    "Use the answers provided for each subquestion to make your decision, and make sure you answer every question. "
    "Write your final answer like this: *TYPE AD*: [answer] - [explanation]. Every question must be answered."
)

# general question to check if the image is fitting for analysis
#first_check = ("Can you see advertising or brand related material in this image? If yes, describe the brand.")

sub_questions_q1 = [
    ("FOOD OR DRINK", "Is the ad related to a food or drink product? \n1 = Yes \n2 = No"),
    ("TYPE OF PRODUCT SOURCE", "If yes, is the product from a food company/brand, retailer, or restaurant? \n1 = Food company/brand \n2 = Retailer \n3 = Restaurant or takeaway"),
    ("FOOD COMPANY PROMOTION", "If food company/brand, is it promoting a specific product or just the brand? \n1 = Specific product \n2 = Just the brand"),
    ("RETAILER TYPE", "If retailer, is it a supermarket/convenience store or another type of retailer? \n1 = Supermarket or convenience store \n2 = Other type of retailer"),
    ("SUPERMARKET PROMOTION", "If supermarket/convenience store, is it promoting a specific product or just the brand? \n1 = Specific product \n2 = Just the brand"),
    ("RESTAURANT PROMOTION", "If restaurant or takeaway, is it promoting a specific product or just the brand? \n1 = Specific product \n2 = Just the brand"),
    ("NON-FOOD OR DRINK", "If no, is the ad related to a non-food or drink product, alcohol, or infant formula? \n1 = Non-food or drink product \n2 = Alcohol \n3 = Infant formula")
]

sub_questions_q2 = [
    ("MARKETING STRATEGY USED", "Is there a marketing strategy used in this ad? \n0 = No marketing strategy used \n1 = Yes, there is a marketing strategy used"),
    ("TYPE OF STRATEGY", "What type of marketing strategy is used? \n1 = Cartoon/Company owned character (e.g., Shrek) \n2 = Licensed character (e.g., Dora the Explorer) \n3 = Person (sportsperson, celebrity, etc.) \n4 = Event or historical/festival \n5 = For specific groups (kids, students) \n6 = Awards \n7 = Other"),
    ("CARTOON OR COMPANY OWNED", "Specify the type: \n1 = Cartoon/Company owned character (e.g., M&Ms) \n2 = Movie/cartoon character "),
    ("PERSON STRATEGY", "Specify the type: \n1 = Amateur sportsperson \n2 = Celebrity (non-sports) \n3 = Famous sportsperson/team"),
    ("EVENT STRATEGY", "Specify the type: \n1 = Non-sports/historical events/festivals (e.g., Christmas) \n2 = Sports event"),
    ("SPECIFIC GROUPS", "Specify the type: \n1 = ‘For kids’ (e.g., image of a child, ‘great for school lunches’) \n2 = ‘For students’")
]

sub_questions_q3 = [
    ("PREMIUM OFFER USED", "Is there a premium offer used in this ad? \n0 = No premium offer used \n1 = Yes, there is a premium offer used"),
    ("TYPE OF OFFER", "What type of premium offer is used? \n1 = Digital Offers \n2 = Promotions \n3 = Special Editions \n4 = Social Charity \n5 = Gifts or Collectables \n6 = Price or Loyalty Incentives \7 = Other"),
    ("DIGITAL OFFERS", "Specify the type: \n1 = Game and app downloads \n2 = Contests"),
    ("PROMOTIONS", "Specify the type: \n1 = Pay 2 take 3, or similar \n2 = 20%% extra, or similar"),
    ("PRICE OR LOYALTY INCENTIVES", "Specify the type: \n1 = Price discount \n2 = Loyalty programs")
]

sub_questions_q4 = [
    ("INITIAL WHO CATEGORY", "Is the product a type of: \n1 = Confectionery, Baked Goods, or Snack \n2 = Drink or Dairy Product \n3 = Grain, Meat, or Vegetable \n4 = Ready-made and convenience foods \n5 = Alcoholic, Non-Applicable or Non-Specified"),
    ("CONFECTIONERY BAKED SNACK", "Specify the type: \n1 = Chocolate and sugar confectionery \n2 = Cakes and pastries \n3 = Savoury snacks (including salted nuts) \n4 = Unsalted nuts"),
    ("BEVERAGE DAIRY", "Specify the type: \n1 = Juices \n2 = Milk drinks \n3 = Energy drinks \n4 = Other beverages (Soft drinks, sweetened beverages) \n5 = Waters, tea, and coffee (unsweetened) \n6 = Edible ices \n7 = Yoghurts, sour milk, cream, etc. \n8 = Cheese"),
    ("GRAIN MEAT VEGETABLE", "Specify the type: \n1 = Breakfast cereals \n2 = Butter and other fats and oils \n3 = Bread, bread products, and crisp breads \n4 = Fresh or dried pasta, rice, and grains \n5 = Fresh and frozen meat, poultry, fish, and similar +eggs \n6 = Processed meat, poultry, fish, and similar \n7 = Fresh and frozen fruit, vegetables, and legumes \n8 = Processed fruit, vegetables, and legumes \n9 = Sauces, dips, and dressings"),
    ("ALCOHOL NONSPECIFIED", "Specify the type: \n1 = Alcohol \n2 = Non applicable company brand with no foods and drinks")
]

sub_questions_q5 = [
    ("PROCESSING LEVEL", "How processed is the food in the image? \n1 = Unprocessed or minimally processed \n2 = Processed \n3 = Alcohol \n4 = Non applicable"),
    ("TYPE OF PROCESSING", "Specify the level of processing: \n1 = Processed culinary ingredients \n2 = Processed food \n3 = Ultra-processed food")
]

sub_questions_q6 = [
    ("INITIAL CATEGORY", "Categorize the product broadly: \n1 = Drink or Dairy Product \n2 = Vegetable, Fruit, or Legume \n3 = Grain, Nut, or Meat \n4 = Outside the Healthy Living Triangle or Non-applicable"),
    ("DRINK DAIRY", "Specify the type: \n1 = Drinks \n2 = Milk and alternatives \n3 = Cheese"),
    ("VEGETABLE FRUIT LEGUME", "Specify the type: \n1 = Vegetables \n2 = Fruits \n3 = Legumes"),
    ("GRAIN NUT MEAT", "Specify the type: \n1 = Bread, whole grain products and potatoes \n2 = Nuts and seeds \n3 = Eggs \n4 = Fish \n5 = Meat \n6 = Oils and fats"),
    ("OUTSIDE_TRIANGLE_NON_APPLICABLE", "Specify the type: \n1 = Nutrition outside the triangle \n2 = Alcohol \n3 = Non applicable company brand with no foods and drinks")
]
    

def get_q1_answer(answer_dict):
    
    # default "-1" for missing data
    food_or_drink, food_or_drink_expl = answer_dict.get("FOOD OR DRINK", ("-1", "Answer missing"))
    product_source, product_source_expl = answer_dict.get("TYPE OF PRODUCT SOURCE", ("-1", "Answer missing"))
    food_company_promotion, food_company_promotion_expl = answer_dict.get("FOOD COMPANY PROMOTION", ("-1", "Answer missing"))
    retailer_type, retailer_type_expl = answer_dict.get("RETAILER TYPE", ("-1", "Answer missing"))
    supermarket_promotion, supermarket_promotion_expl = answer_dict.get("SUPERMARKET PROMOTION", ("-1", "Answer missing"))
    restaurant_promotion, restaurant_promotion_expl = answer_dict.get("RESTAURANT PROMOTION", ("-1", "Answer missing"))
    non_food_or_drink, non_food_or_drink_expl = answer_dict.get("NON-FOOD OR DRINK", ("-1", "Answer missing"))


    explanation = "" # to bind the explanation together
    if food_or_drink == "1":  # Yes, it is related to food or drink
        explanation += food_or_drink_expl
        if product_source == "1":  # Food company/brand
            explanation += " " + product_source_expl
            if food_company_promotion == "1":  # Specific product
                type_ad_answer = "1"
                explanation += " " + food_company_promotion_expl
            elif food_company_promotion == "2":  # Just the brand
                type_ad_answer = "3"
                explanation += " " + food_company_promotion_expl
            else:
                type_ad_answer = "-1"
                explanation += " Answer missing or ambiguous."
        elif product_source == "2":  # Retailer
            explanation += " " + product_source_expl
            if retailer_type == "1":  # Supermarket or convenience store
                explanation += " " + retailer_type_expl
                if supermarket_promotion == "1":  # Specific product
                    type_ad_answer = "4"
                    explanation += " " + supermarket_promotion_expl
                elif supermarket_promotion == "2":  # Just the brand
                    type_ad_answer = "5"
                    explanation += " " + supermarket_promotion_expl
                else:
                    type_ad_answer = "-1"
                    explanation += " Answer missing or ambiguous."
            elif retailer_type == "2":  # Other type of retailer
                type_ad_answer = "2"
                explanation += " Food or drink product promoted by a non-food brand."
            else:
                type_ad_answer = "-1"
                explanation += " Answer missing or ambiguous."
        elif product_source == "3":  # Restaurant or takeaway
            explanation += " " + product_source_expl
            if restaurant_promotion == "1":  # Specific product
                type_ad_answer = "6"
                explanation += " " + restaurant_promotion_expl
            elif restaurant_promotion == "2":  # Just the brand
                type_ad_answer = "7"
                explanation += " " + restaurant_promotion_expl
            else:
                type_ad_answer = "-1"
                explanation += " Answer missing or ambiguous."
        else:
            type_ad_answer = "-1"
            explanation += " Answer missing or ambiguous."
    elif food_or_drink == "2":  # no, it is not related to food or drink
        explanation += " " + food_or_drink_expl
        if non_food_or_drink == "1":  # non-food or drink product
            type_ad_answer = "8"
            explanation += " " + non_food_or_drink_expl
        elif non_food_or_drink == "2":  # alcohol
            type_ad_answer = "9"
            explanation += " " + non_food_or_drink_expl
        elif non_food_or_drink == "3":  # infant formula
            type_ad_answer = "10"
            explanation += " " + non_food_or_drink_expl
        else:
            type_ad_answer = "5"
            explanation += " Answer missing - retailer (supermarket or convenience store) without a product."
    else:
        type_ad_answer = "-1"
        explanation += " Answer missing or ambiguous."
    
    return type_ad_answer, explanation

def get_q2_answer(answer_dict):
    marketing_strategy_used, marketing_strategy_used_expl = answer_dict.get("MARKETING STRATEGY USED", ("-1", "Answer missing"))
    explanation = marketing_strategy_used_expl

    if marketing_strategy_used == "1":  # Yes, there is a marketing strategy used
        type_of_strategy, type_of_strategy_expl = answer_dict.get("TYPE OF STRATEGY", ("-1", "Answer missing."))
        explanation += " " + type_of_strategy_expl

        if type_of_strategy == "1":  # Cartoon/Company owned character
            cartoon_or_company_owned, cartoon_or_company_owned_expl = answer_dict.get("CARTOON OR COMPANY OWNED", ("-1", "Answer missing."))
            explanation += " " + cartoon_or_company_owned_expl
            if cartoon_or_company_owned == "1": # Cartoon/Company owned character
                type_ad_answer = "1"
            elif cartoon_or_company_owned == "2": # Movie tie-in
                type_ad_answer = "5"
            else:
                type_ad_answer = "-1"
                explanation += " Answer missing."
        elif type_of_strategy == "2":  # Licensed character
            type_ad_answer = "2"
        elif type_of_strategy == "3":  # Person (sportsperson, celebrity, etc.)
            person_strategy, person_strategy_expl = answer_dict.get("PERSON STRATEGY", ("-1", "Answer missing."))
            explanation += " " + person_strategy_expl
            if person_strategy == "1": # Amateur sportsperson
                type_ad_answer = "3"
            elif person_strategy == "2": # Celebrity (non-sports)
                type_ad_answer = "4"
            elif person_strategy == "3": # Famous sportsperson/team
                type_ad_answer = "6"
            else:
                type_ad_answer = "11" # set as 'Other'
                explanation += " Other."
        elif type_of_strategy == "4":  # Event or historical/festival
            event_strategy, event_strategy_expl = answer_dict.get("EVENT STRATEGY", ("-1", "Answer missing."))
            explanation += " " + event_strategy_expl
            if event_strategy == "1": # Non-sports event (festival)
                type_ad_answer = "7"
            elif event_strategy == "2": # Sports event
                type_ad_answer = "10"
            else:
                type_ad_answer = "11" # set as 'Other'
                explanation += " Other."
        elif type_of_strategy == "5":  # For specific groups (kids, students)
            specific_groups, specific_groups_expl = answer_dict.get("SPECIFIC GROUPS", ("-1", "Answer missing."))
            explanation += " " + specific_groups_expl
            if specific_groups == "1": # Kids
                type_ad_answer = "8a"
            elif specific_groups == "2": # Students
                type_ad_answer = "8b"
            else:
                type_ad_answer = "11"
                explanation += " Other."
        elif type_of_strategy == "6":  # Awards
            type_ad_answer = "9"
            explanation += " Awards (e.g., Best Food Award 2014)."
        elif type_of_strategy == "7":  # Other
            type_ad_answer = "11"
        else:
            type_ad_answer = "-1"
            explanation += " Answer missing."
    else:
        type_ad_answer = 0

    return type_ad_answer, explanation


def get_q3_answer(answer_dict):
    premium_offer_used, premium_offer_used_expl = answer_dict.get("PREMIUM OFFER USED", ("-1", "Answer missing."))
    explanation = premium_offer_used_expl

    if premium_offer_used == "1":  # Yes, there is a premium offer used
        type_of_offer, type_of_offer_expl = answer_dict.get("TYPE OF OFFER", ("-1", "Answer missing."))
        explanation += " " + type_of_offer_expl

        if type_of_offer == "1":  # Digital Offers
            digital_offers, digital_offers_expl = answer_dict.get("DIGITAL OFFERS", ("-1", "Answer missing."))
            explanation += " " + digital_offers_expl
            if digital_offers == "1": # Game/App downloads
                type_ad_answer = "1"
            elif digital_offers == "2": # Contests
                type_ad_answer = "2"
            else:
                type_ad_answer = "10" # set as 'Other'
                explanation += " Other."
        elif type_of_offer == "2":  # Promotions
            promotions, promotions_expl = answer_dict.get("PROMOTIONS", ("-1", "Answer missing."))
            explanation += " " + promotions_expl
            if promotions == "1": # Pay 2 get 3
                type_ad_answer = "3"
            elif promotions == "2": # 20% off
                type_ad_answer = "4"
            else:
                type_ad_answer = "10"
                explanation += " Other."
        elif type_of_offer == "3":  # Special Editions
            type_ad_answer = "5"
        elif type_of_offer == "4":  # Social Charity
            type_ad_answer = "6"
        elif type_of_offer == "5":  # Gifts or Collectables
            type_ad_answer = "7"
        elif type_of_offer == "6":  # Price or Loyalty Incentives
            price_or_loyalty_incentives, price_or_loyalty_incentives_expl = answer_dict.get("PRICE OR LOYALTY INCENTIVES", ("-1", "Answer missing."))
            type_ad_answer = price_or_loyalty_incentives
            explanation += " " + price_or_loyalty_incentives_expl
            if price_or_loyalty_incentives == "1": # Price discount
                type_ad_answer = "8"
            elif price_or_loyalty_incentives == "2": # Loyalty programs
                type_ad_answer = "9"
            else:
                type_ad_answer = "10"
                explanation += " Other."
        elif type_of_offer == "7":  # Other
            type_ad_answer = "10"
        else:
            type_ad_answer = "-1"
            explanation += " Answer missing."
    else:
        type_ad_answer = "0"

    return type_ad_answer, explanation


def get_q4_answer(answer_dict):
    initial_category, initial_category_expl = answer_dict.get("INITIAL WHO CATEGORY", ("-1", "Answer missing"))
    explanation = initial_category_expl

    if initial_category == "1":  # Confectionery, Baked Goods, or Snack
        confectionery_baked_snack, confectionery_baked_snack_expl = answer_dict.get("CONFECTIONERY BAKED SNACK", ("-1", "Answer missing"))
        explanation += " " + confectionery_baked_snack_expl
        if confectionery_baked_snack == "1": # Chocolate/sugar
            type_ad_answer = "1"
        elif confectionery_baked_snack == "2": # Cakes
            type_ad_answer = "2"
        elif confectionery_baked_snack == "3": # Savoury snacks
            type_ad_answer = "3a"
        elif confectionery_baked_snack == "4": # Unsalted nuts
            type_ad_answer = "3b"
        else: 
            type_ad_answer = "-1"
            explanation += " Answer missing."
    elif initial_category == "2":  # Beverage or Dairy Product
        beverage_dairy, beverage_dairy_expl = answer_dict.get("BEVERAGE DAIRY", ("-1", "Answer missing"))
        explanation += " " + beverage_dairy_expl
        if beverage_dairy == "1": # Juices
            type_ad_answer = "4a"
        elif beverage_dairy == "2": # Milk
            type_ad_answer = "4b"
        elif beverage_dairy == "3": # Energy drinks
            type_ad_answer = "4c"
        elif beverage_dairy == "4": # Soft drinks
            type_ad_answer = "4d"
        elif beverage_dairy == "5": # Water/tea/coffee
            type_ad_answer = "4e"
        elif beverage_dairy == "6": # Ices
            type_ad_answer = "5"
        elif beverage_dairy == "7": # Yogurt
            type_ad_answer = "7"
        elif beverage_dairy == "8": # Cheese 
            type_ad_answer = "8"
        else: 
            type_ad_answer = "-1"
            explanation += " Answer missing."
    elif initial_category == "3":  # Grain, Meat, or Vegetable
        grain_meat_vegetable, grain_meat_vegetable_expl = answer_dict.get("GRAIN MEAT VEGETABLE", ("-1", "Answer missing"))
        explanation += " " + grain_meat_vegetable_expl
        if grain_meat_vegetable == "1": # Cereal
            type_ad_answer = "6"
        elif grain_meat_vegetable == "2": # Butter/oil
            type_ad_answer = "10"
        elif grain_meat_vegetable == "3": # Bread
            type_ad_answer = "11"
        elif grain_meat_vegetable == "4": # Pasta/rice/grains
            type_ad_answer = "12"
        elif grain_meat_vegetable == "5": # Fresh meat/fish
            type_ad_answer = "13"
        elif grain_meat_vegetable == "6": # Processed meat/fish
            type_ad_answer = "14"
        elif grain_meat_vegetable == "7": # Fresh fruit/veg
            type_ad_answer = "15"
        elif grain_meat_vegetable == "8": # Processed fruit/veg
            type_ad_answer = "16"
        elif grain_meat_vegetable == "9": # Sauces/dips
            type_ad_answer = "17"
        else: 
            type_ad_answer = "-1"
            explanation += " Answer missing."
    elif initial_category == "4": # Ready-made and convenience foods
        type_ad_answer = "9"
    elif initial_category == "5":  # Alcoholic or Non-Specified
        alcohol_nonspecified, alcohol_nonspecified_expl = answer_dict.get("ALCOHOL NONSPECIFIED", ("-1", "Answer missing"))
        explanation += " " + alcohol_nonspecified_expl
        if alcohol_nonspecified == "1": # Alcohol
            type_ad_answer = "A"
        elif alcohol_nonspecified == "2": # Non-Applicable 
            type_ad_answer = "NA"
        else: 
            type_ad_answer = "NA"
            explanation += " Answer missing - Non applicable."
    else:
        type_ad_answer = "-1"
        explanation += "Answer missing or ambiguous."

    return type_ad_answer, explanation


def get_q5_answer(answer_dict):
    initial_processing_level, initial_processing_level_expl = answer_dict.get("PROCESSING LEVEL", ("-1", "Answer missing."))
    explanation = initial_processing_level_expl

    if initial_processing_level == "1":  # Unprocessed or minimally processed food
        type_ad_answer = "1"
        #explanation += " Unprocessed or minimally processed food."
    elif initial_processing_level == "2":  # Processed
        type_of_processing, type_of_processing_expl = answer_dict.get("TYPE OF PROCESSING", ("-1", "Answer missing."))
        explanation += " " + type_of_processing_expl
        if type_of_processing == "1": # Culinary ingredients
            type_ad_answer = "2"
        elif type_of_processing == "2": # Processed food
            type_ad_answer = "3"
        elif type_of_processing == "3": # Ultra-processed
            type_ad_answer = "4"
        else: 
            type_ad_answer = "-1"
            explanation += " Answer missing."
    elif initial_processing_level == "3":  # Alcohol
        type_ad_answer = "5"
    elif initial_processing_level == "4":  # Non applicable
        type_ad_answer = "NA"
    else:
        type_ad_answer = "NA"
        explanation += " Answer missing."

    return type_ad_answer, explanation


def get_q6_answer(answer_dict):
    initial_category, initial_category_expl = answer_dict.get("INITIAL CATEGORY", ("-1", "Answer missing"))
    explanation = initial_category_expl

    if initial_category == "1":  # Drink or Dairy Product
        drink_dairy, drink_dairy_expl = answer_dict.get("DRINK DAIRY", ("-1", "Answer missing"))
        explanation += " " + drink_dairy_expl
        if drink_dairy == "1": # Drinks
            type_ad_answer = "1"
        elif drink_dairy == "2": # Milk and alternatives
            type_ad_answer = "10"
        elif drink_dairy == "3": # Cheese
            type_ad_answer = "9"
        else:
            type_ad_answer = "-1"
            explanation += " Answer missing."
    elif initial_category == "2":  # Vegetable, Fruit, or Legume
        vegetable_fruit_legume, vegetable_fruit_legume_expl = answer_dict.get("VEGETABLE FRUIT LEGUME", ("-1", "Answer missing"))
        explanation += " " + vegetable_fruit_legume_expl
        if vegetable_fruit_legume == "1": # Vegetable
            type_ad_answer = "2"
        elif vegetable_fruit_legume == "2": # Fruit
            type_ad_answer = "3"
        elif vegetable_fruit_legume == "3": # Legume
            type_ad_answer = "6"
        else:
            type_ad_answer = "-1"
            explanation += " Answer missing."
    elif initial_category == "3":  # Grain, Nut, or Meat
        grain_nut_meat, grain_nut_meat_expl = answer_dict.get("GRAIN NUT MEAT", ("-1", "Answer missing"))
        explanation += " " + grain_nut_meat_expl
        if grain_nut_meat == "1": # Bread
            type_ad_answer = "4"
        elif grain_nut_meat == "2": # Nuts
            type_ad_answer = "5"
        elif grain_nut_meat == "3": # Eggs
            type_ad_answer = "8"
        elif grain_nut_meat == "4": # Fish
            type_ad_answer = "11"
        elif grain_nut_meat == "5": # Meat
            type_ad_answer = "12"
        elif grain_nut_meat == "6": # Oils
            type_ad_answer = "7"
        else:
            type_ad_answer = "-1"
            explanation += " Answer missing."
    elif initial_category == "4":  # Outside the Healthy Living Triangle or Non-applicable
        outside_triangle_non_applicable, outside_triangle_non_applicable_expl = answer_dict.get("OUTSIDE_TRIANGLE_NON_APPLICABLE", ("-1", "Answer missing"))
        explanation += " " + outside_triangle_non_applicable_expl
        if outside_triangle_non_applicable == "1": # Outside the triangle
            type_ad_answer = "13"
        elif outside_triangle_non_applicable == "2": # Alcohol
            type_ad_answer = "A"
        elif outside_triangle_non_applicable == "3": # Non-applicable
            type_ad_answer = "NA"
        else:
            type_ad_answer = "NA"
            explanation += " Answer missing - Non applicable."
    else:
        type_ad_answer = "-1"
        explanation = "Answer missing or ambiguous."

    return type_ad_answer, explanation


# original questions:
type_ad = (
    "TYPE AD: Choose the number that best represents the type of ad in the image: "
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
    "MARKETING STRATEGY: Choose just the number of the marketing strategy used in this ad: "
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
    "PREMIUM OFFER: Choose just the number of the premium offer type in this ad: "
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
    "WHO CATEGORY: Categorize the ad into one of these product categories: "
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
    "PROCESSING: How processed is the food in the image? "
    "1 = unprocessed or minimally processed food "
    "2 = processed culinary ingredients "
    "3 = processed food "
    "4 = ultra-processed food "
    "5 = alcohol "
    "NA = Non applicable"
)

healthy_living = (
    "HEALTHY LIVING: Categorize the ad into one of these healthy living categories: "
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

