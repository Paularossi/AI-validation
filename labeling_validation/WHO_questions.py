
instructions_1 = (
    "You will be provided with a picture of an online advertisement delivered to Belgium/Netherlands, its corresponding text, either in English, French or Dutch, and the name of the company running the ad. "
    "You will be given sets of questions on different topics regarding the content of the advertisement, along with definitions and examples for each question. "
    "You need to answer all the questions with Yes/No, along with a short explanation for the answer. "
    "Preserve a strictly structured answer to ease parsing of the text, by only writing the label of the question, the Yes/No answer and the explanation. "
    "Write your answers like this: *CARTOON*: Yes/No - explanation; *CELEBRITY*: Yes/No - explanation; and so on. Ensure that the question label is between a set of stars. "
    "Ensure that each answer includes a brief explanation of the features in the image/text that led to your choice. Ensure that you answer all questions. "
)

type_ad = [ # change here
    ("FOOD_PRODUCT_COMPANY", "Is the ad promoting a specific food or drink product from a food/drink manufacturing company or brand (defined as a company or brand involved in producing and processing foods or beverages. Manufacturers focus on the creation and packaging of consumable goods rather than directly selling them to end consumers), which is visible in the image or text? (e.g. a Coca-cola bottle in someone's hand) "),
    ("FOOD_PRODUCT_NONFOOD_COMPANY", "Is the ad promoting a specific food or drink product but created by a non-food brand/company/retailer/service/event? (e.g. a bank sponsoring free coffee at an event) "),
    ("FOOD_COMPANY_NO_PRODUCT", "Is the ad promoting a food or drink company or brand without showing a specific food or drink product? (e.g. an ad for Nestlé as a brand but not for a specific product) "),
    ("RETAILER_FOOD_PRODUCT", "Is the ad from a food or drink retailer (supermarket or convenience store) featuring a specific food or drink product? (e.g. a supermarket ad showcasing discounts on fresh produce) "),
    ("RETAILER_NO_FOOD_PRODUCT", "Is the ad from a food or drink retailer (supermarket or convenience store) without featuring any specific food or drink product? (e.g. a store ad focusing on customer service without showcasing any products) "),
    ("RESTAURANT_FOOD_PRODUCT", "Is the ad from a food or drink retailer (restaurant, takeaway, or fast food) featuring a specific food or drink product? (e.g. a McDonald’s ad promoting their new burger) "),
    ("RESTAURANT_NO_FOOD_PRODUCT", "Is the ad from a food or drink retailer (restaurant, takeaway, or fast food) without showcasing any specific food or drink product? (e.g. an ad focusing on restaurant ambiance or service) "),
    ("NONFOOD_PRODUCT", "Is the ad promoting a non-food or drink product or service? (e.g. a mobile phone or car ad) "),
    ("ALCOHOL_PRODUCT", "Is the ad promoting an alcoholic product? (e.g. beer, wine, liquor) "),
    ("INFANT_FORMULA", "Is the ad promoting infant formula, follow-up, or growing-up milks? (e.g. an ad for Enfamil or Aptamil) ")
]





# questions_intro = [
#     ("IS AN AD", "Does the image contain an advertisement (e.g., promotional content or product images) that is suitable for further analysis?"),
#     ("BRAND", "Can you identify any visible brand name or logo in the image? If so, please provide just the brand name or a brief description of the logo.")
# ]

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
    "9 = alcoholic product/brand "
    "10 = infant formula, follow-up and growing up milks"
)

marketing_str = (
    "MARKETING STRATEGY: Choose just the number of the marketing strategy used in this ad. "
    "Only if none of the 10 options fit, choose 0: "
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
)

# premium offers
premium_offer = ( 
    "PREMIUM OFFER: Choose just the number of the premium offer type in this ad: "
    "Only if none of the 9 options fit, choose 0: "
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
)

# food product category - not very useful for fast-food, more for snacks
who_cat = (
    "WHO CATEGORY: Categorize the ad into one of these product categories: "
    "1 = Chocolate and sugar confectionery "
    "2 = Cakes and pastries "
    "3 = Savoury snacks "
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
    "14 = Processed meat, poultry, fish and similar (e.g. sausages, smoked fish, etc.) "
    "15 = Fresh and frozen fruit, vegetables and legumes "
    "16 = Processed fruit, vegetables and legumes (e.g. tinned, dried, etc.)"
    "17 = Sauces, dips and dressings "
    "A = Alcohol "
    "NA. Non applicable company brand with no foods and drinks "
    "NS. Non-specified "
)

processed = (
    "PROCESSING: How processed is the food in the image? "
    "1 = ultra-processed food "
    "2 = unprocessed or minimally processed food "
    "3 = processed culinary ingredients "
    "4 = processed food "
    "5 = alcohol "
    "NA = Non applicable"
)


def get_intro_answer(answer_dict):
    is_ad, is_ad_expl = answer_dict.get("IS AN AD", ("-1", "Answer missing"))
    if is_ad == "No":  # not an ad
        is_ad_answer = "0"
    elif is_ad == "Yes":
        is_ad_answer = "1"
    else:
        is_ad_answer = "-1"
    
    return is_ad_answer, is_ad_expl