import os
import hashlib
from PIL import Image
import shutil
import re
import pandas as pd


def extract_ids_from_images(folder_path):
    # Define a regular expression pattern to match the IDs
    pattern = re.compile(r'ad_(\d+)(?:_\d+)?_img\.png')

    # Create an empty list to store the IDs
    unique_ids = []

    seen_ids = []
    for filename in os.listdir(folder_path):
        # Match the filename against the pattern
        match = pattern.match(filename)
        if match:
            # Extract the main ID
            main_id = match.group(1)
            # Add to list if not already seen
            if main_id not in seen_ids:
                unique_ids.append(main_id)
                seen_ids.append(main_id)
        else:
            print(f"Image {filename} was not a match")

    return unique_ids


def calculate_image_hash(image_path):
    """
    Calculate the MD5 hash of an image file. The MD5 hash is a 32-character hexadecimal number that uniquely represents the image's pixel data,
    useful for verifying integrity and identifying duplicates.

    :param image_path: The path to the image file.
    :type image_path: str
    :returns: The MD5 hash of the image.
    :rtype: str
    """
    with Image.open(image_path) as img:
        # convert image to RGB
        img = img.convert('RGB')
        # generate a hash for the image
        img_hash = hashlib.md5(img.tobytes()).hexdigest()
    return img_hash


def deduplicate_images(image_folder, unique_img_folder):
    """
    Deduplicate images in a folder and save unique images to a specified folder.

    This function scans a folder for PNG images, calculates the MD5 hash of each image,
    identifies duplicates, and saves only the unique images to a separate folder.

    :param image_folder: The path to the folder containing the original images.
    :type image_folder: str
    :param unique_img_folder: The path to the folder where unique images will be saved.
    :type unique_img_folder: str
    """
    # first check if destination folder exists
    if not os.path.exists(unique_img_folder):
        os.makedirs(unique_img_folder)
        
    unique_images = {}
    duplicate_images = []

    images = os.listdir(image_folder)
    for filename in images:
        if filename.endswith('.png'):
            image_path = os.path.join(image_folder, filename)
            # calculate the MD5 hash and check if it already exists
            img_hash = calculate_image_hash(image_path)

            if img_hash not in unique_images:
                unique_images[img_hash] = image_path
            else:
                print(f"Image {filename} is a duplicate...\n")
                duplicate_images.append(image_path)
    
    # save the unique images in a folder
    for img_hash, unique_image_path in unique_images.items():
        destination_path = os.path.join(unique_img_folder, os.path.basename(unique_image_path))
        shutil.copy(unique_image_path, destination_path)
    
    print(f"Found {len(duplicate_images)} duplicates and saved {len(unique_images)} unique images inside {unique_img_folder}.")


image_folder = 'output/Belgium/ads_images'
unique_img_folder = 'output/Belgium/unique_images'
deduplicate_images(image_folder, unique_img_folder)

df = pd.read_excel('output/Belgium/ads_data/Belgium_original_data.xlsx')

unique_image_ids = extract_ids_from_images(unique_img_folder)

len(unique_image_ids)

# filter the DataFrame to only include rows with IDs in the unique_image_ids list
filtered_df = df[df['id'].astype(str).isin(unique_image_ids)]
len(filtered_df)

# for img_id in unique_image_ids:
#     if img_id not in df['id'].astype(str).values:
#         print(f"ID {img_id} is not in the DataFrame")


filtered_df.loc[:, 'id'] = filtered_df['id'].astype(str)
filtered_df.loc[:, 'page_id'] = filtered_df['page_id'].astype(str)
filtered_df.to_excel('output/Belgium/ads_data/Belgium_ads_subset.xlsx', index=False)

temp = filtered_df.groupby('page_name').size().reset_index(name='count').sort_values(by='count', ascending=True)
#print(temp.to_string(index=False))



