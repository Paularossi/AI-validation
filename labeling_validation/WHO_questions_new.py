
instructions_1 = (
    "You will be provided with a picture of an online advertisement delivered to Belgium/Netherlands, its corresponding text, either in English, French or Dutch, and the name of the company running the ad. "
    "You will be given sets of questions on different topics regarding the content of the advertisement, along with definitions and examples for each question. "
    "You need to answer all the questions with Yes/No, along with a short explanation for the answer. "
    "Preserve a strictly structured answer to ease parsing of the text, by only writing the label of the question, the Yes/No answer and the explanation. "
    "Write your answers like this: *CARTOON*: Yes/No - explanation; *CELEBRITY*: Yes/No - explanation; and so on. Ensure that the question label is between a set of stars. "
    "Ensure that each answer includes a brief explanation of the features in the image/text that led to your choice. Ensure that you answer all questions. "
)

# the questions need: (1) to be mutually exclusive, (2) to have a definition and (3) to give examples
# add a new question about the target audience and give explanations for each category (remove "for kids" from marketing strategies)

type_ad = [ # change here
    ("FOOD_PRODUCT_COMPANY", "Is the ad promoting a specific food or drink product from a food company/brand or manufacturer, which is visible in the image or text? (e.g. a Coca-cola bottle in someone's hand) "),
    ("FOOD_PRODUCT_NONFOOD_COMPANY", "Is the ad promoting a food or drink product but created by a non-food brand/company/retailer/service/event? (e.g. a bank sponsoring free coffee at an event) "),
    ("FOOD_COMPANY_NO_PRODUCT", "Is the ad promoting a food or drink company or brand without showing a specific food or drink product? (e.g. an ad for Nestlé as a brand but not for a specific product) "),
    ("RETAILER_FOOD_PRODUCT", "Is the ad from a food or drink retailer (supermarket or convenience store) featuring a specific food or drink product? (e.g. a supermarket ad showcasing discounts on fresh produce) "),
    ("RETAILER_NO_FOOD_PRODUCT", "Is the ad from a food or drink retailer (supermarket or convenience store) without featuring any specific food or drink product? (e.g. a store ad focusing on customer service without showcasing any products) "),
    ("RESTAURANT_FOOD_PRODUCT", "Is the ad from a food or drink retailer (restaurant, takeaway, or fast food) featuring a specific food or drink product? (e.g. a McDonald’s ad promoting their new burger) "),
    ("RESTAURANT_NO_FOOD_PRODUCT", "Is the ad from a food or drink retailer (restaurant, takeaway, or fast food) without showcasing any specific food or drink product? (e.g. an ad focusing on restaurant ambiance or service) "),
    ("NONFOOD_PRODUCT", "Is the ad promoting a non-food or drink product or service? (e.g. a mobile phone or car ad) "),
    ("ALCOHOL_PRODUCT", "Is the ad promoting an alcoholic product? (e.g. beer, wine, liquor) "),
    ("INFANT_FORMULA", "Is the ad promoting infant formula, follow-up, or growing-up milks? (e.g. an ad for Enfamil or Aptamil) ")
]

# licensed characters are owned by a specific company, change cartoons to mention "not licensed chars"
# remove age-targeted strategies and create a new question

marketing_str = [
    ("CARTOON", "This set of questions is about marketing strategies used in the ad. Answer with Yes/No to each question. Is there a cartoon character specifically created by or for a company to represent their brand, e.g. Tony the Tiger for Frosted Flakes or M&Ms characters? "),
    ("LICENSED_CHARACTER", "Is there a licensed character (well-known characters from TV shows or books)? e.g. Dora the explorer, Mickey Mouse "),
    ("MOVIE_TIE_IN", "Is there a movie tie-in? (Promotional strategies that align products or brands with popular films, utilizing themes, characters, or storylines from movies) "),
    ("FAMOUS_SPORTS", "Are there famous athletes or sports teams that are recognized on a national or international level? "),
    ("AMATEUR_SPORTS", "Are there amateur people playing sports (non-professional level)? "),
    ("CELEBRITY", "Are there non-sports celebrities? e.g. Jamie Oliver"),
    ("KIDS", "Are there marketing messages or products specifically designed for children? "),
    ("STUDENTS", "Are there marketing strategies focused on older children and young adults, particularly those in educational settings? "),
    ("AWARDS", "Are there strategies related to awards? (recognitions or accolades received by a product or brand that signify quality or excellence) "),
    ("EVENTS", "Are there strategies tied to non-sports, cultural events, historical anniversaries, or festivals? "),
    ("SPORT_EVENTS", "Are there strategies related to major sporting events? e.g. Olympic Games, Tour de France")
]

premium_offer = [
    ("GAMES", "This set of questions is about premium offers used in the ad. Are there offers that incentivize consumers to download mobile games or apps? "),
    ("CONTESTS", "Are there promotions/contests where consumers enter to win prizes by completing specific actions, often requiring purchase or engagement? "),
    ("2FOR3", "Are there offers that encourage bulk purchasing by providing extra products for a reduced price? e.g. buy 1 get 1 free, 3 for the price of 2 "),
    ("EXTRA", "Are there promotions offering additional product quantity for the same price? e.g. 20%% extra, 50 grams free "),
    ("LIMITED", "Are there special products offered for a short/limited time? e.g. limited edition, seasonal flavours "),
    ("CHARITY", "Are there offers where a portion of proceeds goes to a charitable cause? e.g. Every Purchase Helps Local Farmers "),
    ("GIFTS", "Are there promotions that include free gifts or collectible items with purchases? "),
    ("DISCOUNT", "Are there direct reductions in the selling price of products? e.g. €1 Off Any Product "),
    ("LOYALTY", "Are there programs that reward loyal customers for repeat purchases, often through points or discounts? e.g. Coffee shop loyalty stamps ")
]

who_cat = [
    ("CHOCOLATE_SUGAR", "Does the ad feature chocolate or sugar confectionery? (e.g. candy bars, chocolates, gummies) "),
    ("CAKES_PASTRIES", "Does the ad feature cakes or pastries? (e.g. cupcakes, doughnuts, croissants) "),
    ("SAVOURY_SNACKS", "Does the ad feature savoury snacks? (e.g. crisps, pretzels, salted nuts) "),
    ("JUICES", "Does the ad feature juices? (e.g. orange juice, apple juice) "),
    ("MILK_DRINKS", "Does the ad feature milk drinks? (e.g. flavoured milk, milkshakes) "),
    ("ENERGY_DRINKS", "Does the ad feature energy drinks? (e.g. Red Bull, Monster) "),
    ("OTHER_BEVERAGES", "Does the ad feature other beverages such as soft drinks or sweetened beverages? (e.g. soda, iced tea) "),
    ("WATERS_TEA_COFFEE", "Does the ad feature unsweetened waters, tea, or coffee? (e.g. bottled water, black tea, black coffee) "),
    ("EDIBLE_ICES", "Does the ad feature edible ices? (e.g. ice cream, popsicles) "),
    ("BREAKFAST_CEREALS", "Does the ad feature breakfast cereals? (e.g. cornflakes, muesli) "),
    ("YOGHURTS", "Does the ad feature yoghurts, sour milk, cream, etc.? (e.g. Greek yoghurt, sour cream) "),
    ("CHEESE", "Does the ad feature cheese? (e.g. cheddar, gouda, feta) "),
    ("READYMADE_CONVENIENCE", "Does the ad feature ready-made or convenience foods and composite dishes? (e.g. frozen meals, pizza) "),
    ("BUTTER_OILS", "Does the ad feature butter or other fats and oils? (e.g. butter, margarine, olive oil) "),
    ("BREAD_PRODUCTS", "Does the ad feature bread, bread products, or crispbreads? (e.g. sandwich bread, baguettes, rye crackers) "),
    ("PASTA_RICE_GRAINS", "Does the ad feature fresh or dried pasta, rice, or grains? (e.g. spaghetti, rice, quinoa) "),
    ("FRESH_MEAT_POULTRY_FISH", "Does the ad feature fresh or frozen meat, poultry, fish, or eggs? (e.g. chicken breasts, salmon fillets, eggs) "),
    ("PROCESSED_MEAT_POULTRY_FISH", "Does the ad feature processed meat, poultry, fish, or similar? (e.g. sausages, smoked fish) "),
    ("FRESH_FRUIT_VEG", "Does the ad feature fresh or frozen fruit, vegetables, or legumes? (e.g. apples, broccoli, lentils) "),
    ("PROCESSED_FRUIT_VEG", "Does the ad feature processed fruit, vegetables, or legumes? (e.g. tinned fruit, dried vegetables) "),
    ("SAUCES_DIPS_DRESSINGS", "Does the ad feature sauces, dips, or dressings? (e.g. ketchup, mayonnaise, guacamole) "),
    ("ALCOHOL", "Does the ad feature alcohol? (e.g. beer, wine, spirits) "),
    ("NA", "Is the ad for a non-applicable company or brand that does not feature any foods or drinks? "),
    ("NS", "Is the product category in the ad non-specified or unclear? ")
]

processed = [
    ("NA_PROCESSING", "This set of questions is about the level of food processing in the image. Answer each question with Yes/No. Does the image not depict any food or beverage and the processing level is therefore non-applicable?"),
    ("ALCOHOL", "Is there alcohol in the image? "),
    ("UNPROCESSED", "Does the food belong to unprocessed or minimally processed food? (natural foods that have undergone minimal alterations, such as cleaning, drying, or freezing, without significant changes to their nutritional content, e.g. fresh fruits, whole grains, eggs, fresh meat) "),
    ("PROCESSED", "Does the food belong to processed food? (Foods that have undergone processes such as canning, smoking, fermentation, or preservation, often with added ingredients to extend shelf life or enhance flavor, e.g. canned vegetables, cheeses, smoked meats, bread) "),
    ("ULTRA_PROCESSED", "Does the food belong to ultra-processed food? (formulations of industrial ingredients, resulting from a series of industrial processes such as frying, chemical modifications, application of additives. They typically contain little or no whole foods, e.g. chips, candy, instant noodles, soft drinks, fast-food) "),
    ("INGREDIENTS", "Does the food represent processed culinary ingredients? (substances extracted or refined from minimally processed foods, typically used in cooking or seasoning other food, e.g. sugar, vegetable oils, butter, salt) ")
]

# it contains an alcoholic product
alcohol = [
    ()
]


# Answer Logic:

def get_ad_type(answer_dict):
    
    # check each ad type in order of priority and return the first "Yes" answer
    if answer_dict["FOOD_PRODUCT_COMPANY"][0] == "Yes":
        return "1", answer_dict["FOOD_PRODUCT_COMPANY"][1]
    
    elif answer_dict["FOOD_PRODUCT_NONFOOD_COMPANY"][0] == "Yes":
        return "2", answer_dict["FOOD_PRODUCT_NONFOOD_COMPANY"][1]
    
    elif answer_dict["FOOD_COMPANY_NO_PRODUCT"][0] == "Yes":
        return "3", answer_dict["FOOD_COMPANY_NO_PRODUCT"][1]
    
    elif answer_dict["RETAILER_FOOD_PRODUCT"][0] == "Yes":
        return "4", answer_dict["RETAILER_FOOD_PRODUCT"][1]
    
    elif answer_dict["RETAILER_NO_FOOD_PRODUCT"][0] == "Yes":
        return "5", answer_dict["RETAILER_NO_FOOD_PRODUCT"][1]
    
    elif answer_dict["RESTAURANT_FOOD_PRODUCT"][0] == "Yes":
        return "6", answer_dict["RESTAURANT_FOOD_PRODUCT"][1]
    
    elif answer_dict["RESTAURANT_NO_FOOD_PRODUCT"][0] == "Yes":
        return "7", answer_dict["RESTAURANT_NO_FOOD_PRODUCT"][1]
    
    elif answer_dict["NONFOOD_PRODUCT"][0] == "Yes":
        return "8", answer_dict["NONFOOD_PRODUCT"][1]
    
    elif answer_dict["ALCOHOL_PRODUCT"][0] == "Yes":
        return "9", answer_dict["ALCOHOL_PRODUCT"][1]
    
    elif answer_dict["INFANT_FORMULA"][0] == "Yes":
        return "10", answer_dict["INFANT_FORMULA"][1]
    
    # if no matches are found, return "NA" with a default explanation
    return "-1", "No applicable ad type was found."


def get_marketing_strategy(answer_dict):
    strategies = []
    explanations = []

    # check each strategy and append to the list if it's present in the ad (Yes)
    if answer_dict["CARTOON"][0] == "Yes":
        strategies.append("1")  # Cartoon character
        explanations.append(answer_dict["CARTOON"][1])

    if answer_dict["LICENSED_CHARACTER"][0] == "Yes":
        strategies.append("2")  # Licensed character
        explanations.append(answer_dict["LICENSED_CHARACTER"][1])
    try:
        if answer_dict["MOVIE_TIE_IN"][0] == "Yes":
            strategies.append("5")  # Movie tie-in
            explanations.append(answer_dict["MOVIE_TIE_IN"][1])
    except:
        if answer_dict["MOVIE_TIE-IN"][0] == "Yes":
            strategies.append("5")  # Movie tie-in
            explanations.append(answer_dict["MOVIE_TIE-IN"][1])

    if answer_dict["FAMOUS_SPORTS"][0] == "Yes":
        strategies.append("6")  # Famous athletes or sports teams
        explanations.append(answer_dict["FAMOUS_SPORTS"][1])

    if answer_dict["AMATEUR_SPORTS"][0] == "Yes":
        strategies.append("3")  # Amateur sports
        explanations.append(answer_dict["AMATEUR_SPORTS"][1])

    if answer_dict["CELEBRITY"][0] == "Yes":
        strategies.append("4")  # Non-sports celebrities
        explanations.append(answer_dict["CELEBRITY"][1])

    if answer_dict["KIDS"][0] == "Yes":
        strategies.append("8a")  # Marketing aimed at children
        explanations.append(answer_dict["KIDS"][1])

    if answer_dict["STUDENTS"][0] == "Yes":
        strategies.append("8b")  # Marketing aimed at students
        explanations.append(answer_dict["STUDENTS"][1])

    if answer_dict["AWARDS"][0] == "Yes":
        strategies.append("9")  # Awards-related marketing
        explanations.append(answer_dict["AWARDS"][1])

    if answer_dict["EVENTS"][0] == "Yes":
        strategies.append("7")  # Cultural or historical events
        explanations.append(answer_dict["EVENTS"][1])

    if answer_dict["SPORT_EVENTS"][0] == "Yes":
        strategies.append("10")  # Sporting events
        explanations.append(answer_dict["SPORT_EVENTS"][1])

    # if no marketing strategies are found, return 0 with a default explanation
    if not strategies:
        return "0", "No marketing strategies found in the ad."

    strategies_string = ", ".join(strategies)
    explanations_string = " ".join(explanations)

    return strategies_string, explanations_string


def get_premium_offer(answer_dict):
    premium_offers = []
    explanations = []

    # check each offer type and append to the list if it's present in the ad (Yes).
    if answer_dict["GAMES"][0] == "Yes":
        premium_offers.append("1")  # Games/App offers
        explanations.append(answer_dict["GAMES"][1])

    if answer_dict["CONTESTS"][0] == "Yes":
        premium_offers.append("2")  # Contests/Promotions
        explanations.append(answer_dict["CONTESTS"][1])

    if answer_dict["2FOR3"][0] == "Yes":
        premium_offers.append("3")  # Bulk purchase offers
        explanations.append(answer_dict["2FOR3"][1])

    if answer_dict["EXTRA"][0] == "Yes":
        premium_offers.append("4")  # Extra product quantity offers
        explanations.append(answer_dict["EXTRA"][1])

    if answer_dict["LIMITED"][0] == "Yes":
        premium_offers.append("5")  # Limited edition or seasonal products
        explanations.append(answer_dict["LIMITED"][1])

    if answer_dict["CHARITY"][0] == "Yes":
        premium_offers.append("6")  # Charity offers
        explanations.append(answer_dict["CHARITY"][1])

    if answer_dict["GIFTS"][0] == "Yes":
        premium_offers.append("7")  # Free gifts or collectibles
        explanations.append(answer_dict["GIFTS"][1])

    if answer_dict["DISCOUNT"][0] == "Yes":
        premium_offers.append("8")  # Direct price discounts
        explanations.append(answer_dict["DISCOUNT"][1])

    if answer_dict["LOYALTY"][0] == "Yes":
        premium_offers.append("9")  # Loyalty programs
        explanations.append(answer_dict["LOYALTY"][1])

    # if no premium offers are found, return 0 with a default explanation.
    if not premium_offers:
        return "0", "No premium offers found in the ad."

    premium_offers_string = ", ".join(premium_offers)
    explanations_string = " ".join(explanations)

    return premium_offers_string, explanations_string


def get_who_cat(answer_dict):
    who_categories = []
    explanations = []

    if answer_dict["NA"][0] == "Yes":
        return "NA", answer_dict["NA"][1] # Non-applicable
        
    # check each WHO category and append to the list if it's present in the ad (Yes).
    if answer_dict["CHOCOLATE_SUGAR"][0] == "Yes":
        who_categories.append("1")  # Chocolate and sugar confectionery
        explanations.append(answer_dict["CHOCOLATE_SUGAR"][1])

    if answer_dict["CAKES_PASTRIES"][0] == "Yes":
        who_categories.append("2")  # Cakes and pastries
        explanations.append(answer_dict["CAKES_PASTRIES"][1])

    if answer_dict["SAVOURY_SNACKS"][0] == "Yes":
        who_categories.append("3")  # Savoury snacks
        explanations.append(answer_dict["SAVOURY_SNACKS"][1])

    if answer_dict["JUICES"][0] == "Yes":
        who_categories.append("4a")  # Juices
        explanations.append(answer_dict["JUICES"][1])

    if answer_dict["MILK_DRINKS"][0] == "Yes":
        who_categories.append("4b")  # Milk drinks
        explanations.append(answer_dict["MILK_DRINKS"][1])

    if answer_dict["ENERGY_DRINKS"][0] == "Yes":
        who_categories.append("4c")  # Energy drinks
        explanations.append(answer_dict["ENERGY_DRINKS"][1])

    if answer_dict["OTHER_BEVERAGES"][0] == "Yes":
        who_categories.append("4d")  # Other beverages (Soft drinks, sweetened beverages)
        explanations.append(answer_dict["OTHER_BEVERAGES"][1])

    if answer_dict["WATERS_TEA_COFFEE"][0] == "Yes":
        who_categories.append("4e")  # Waters, tea, and coffee (unsweetened)
        explanations.append(answer_dict["WATERS_TEA_COFFEE"][1])

    if answer_dict["EDIBLE_ICES"][0] == "Yes":
        who_categories.append("5")  # Edible ices
        explanations.append(answer_dict["EDIBLE_ICES"][1])

    if answer_dict["BREAKFAST_CEREALS"][0] == "Yes":
        who_categories.append("6")  # Breakfast cereals
        explanations.append(answer_dict["BREAKFAST_CEREALS"][1])

    if answer_dict["YOGHURTS"][0] == "Yes":
        who_categories.append("7")  # Yoghurts, sour milk, cream, etc.
        explanations.append(answer_dict["YOGHURTS"][1])

    if answer_dict["CHEESE"][0] == "Yes":
        who_categories.append("8")  # Cheese
        explanations.append(answer_dict["CHEESE"][1])

    if answer_dict["READYMADE_CONVENIENCE"][0] == "Yes":
        who_categories.append("9")  # Ready-made and convenience foods
        explanations.append(answer_dict["READYMADE_CONVENIENCE"][1])

    if answer_dict["BUTTER_OILS"][0] == "Yes":
        who_categories.append("10")  # Butter and other fats and oils
        explanations.append(answer_dict["BUTTER_OILS"][1])

    if answer_dict["BREAD_PRODUCTS"][0] == "Yes":
        who_categories.append("11")  # Bread, bread products, and crisp breads
        explanations.append(answer_dict["BREAD_PRODUCTS"][1])

    if answer_dict["PASTA_RICE_GRAINS"][0] == "Yes":
        who_categories.append("12")  # Fresh or dried pasta, rice, and grains
        explanations.append(answer_dict["PASTA_RICE_GRAINS"][1])

    if answer_dict["FRESH_MEAT_POULTRY_FISH"][0] == "Yes":
        who_categories.append("13")  # Fresh and frozen meat, poultry, fish, and eggs
        explanations.append(answer_dict["FRESH_MEAT_POULTRY_FISH"][1])

    if answer_dict["PROCESSED_MEAT_POULTRY_FISH"][0] == "Yes":
        who_categories.append("14")  # Processed meat, poultry, fish, and similar
        explanations.append(answer_dict["PROCESSED_MEAT_POULTRY_FISH"][1])

    if answer_dict["FRESH_FRUIT_VEG"][0] == "Yes":
        who_categories.append("15")  # Fresh and frozen fruit, vegetables, and legumes
        explanations.append(answer_dict["FRESH_FRUIT_VEG"][1])

    if answer_dict["PROCESSED_FRUIT_VEG"][0] == "Yes":
        who_categories.append("16")  # Processed fruit, vegetables, and legumes
        explanations.append(answer_dict["PROCESSED_FRUIT_VEG"][1])

    if answer_dict["SAUCES_DIPS_DRESSINGS"][0] == "Yes":
        who_categories.append("17")  # Sauces, dips, and dressings
        explanations.append(answer_dict["SAUCES_DIPS_DRESSINGS"][1])

    if answer_dict["ALCOHOL"][0] == "Yes":
        who_categories.append("A")  # Alcohol
        explanations.append(answer_dict["ALCOHOL"][1])

    if answer_dict["NS"][0] == "Yes":
        who_categories.append("NS")  # Non-specified
        explanations.append(answer_dict["NS"][1])

    # if no WHO categories are found, return "NA" with a default explanation
    if not who_categories:
        return "NA", "No WHO categories found in the ad."

    who_category_string = ", ".join(who_categories)
    explanations_string = " ".join(explanations)

    return who_category_string, explanations_string


def get_processing_level(answer_dict):
    if answer_dict["ALCOHOL"][0] == "Yes":
        return "A", answer_dict["ALCOHOL"][1]  # Alcohol
    
    elif answer_dict["NA_PROCESSING"][0] == "Yes":
        return "NA", answer_dict["NA_PROCESSING"][1]  # Non-applicable
        
    elif answer_dict["ULTRA_PROCESSED"][0] == "Yes":
        return "1", answer_dict["ULTRA_PROCESSED"][1]  # Ultra-processed food
    
    elif answer_dict["PROCESSED"][0] == "Yes":
        return "4", answer_dict["PROCESSED"][1]  # Processed food
    
    elif answer_dict["UNPROCESSED"][0] == "Yes":
        return "2", answer_dict["UNPROCESSED"][1]  # Unprocessed or minimally processed food
    
    elif answer_dict["INGREDIENTS"][0] == "Yes":
        return "3", answer_dict["INGREDIENTS"][1]  # Processed culinary ingredients
    
    
    # if none of the food processing levels apply, return "NA" with a default explanation
    return "NA", "No applicable food processing level was found in the ad."
