import pandas as pd
import numpy as np
import os
import cv2
#from tensorflow.python.keras.preprocessing import image

os.chdir("I:/Documents/Kaggle/PetFinder.my Adoption Prediction/Созыкин")

def create_dir(dir_name):
    if not os.path.exists(dir_name):
        os.makedirs(dir_name)
        os.makedirs(os.path.join(dir_name,"0"))
        os.makedirs(os.path.join(dir_name,"1"))
        os.makedirs(os.path.join(dir_name,"2"))
        os.makedirs(os.path.join(dir_name,"3"))
        os.makedirs(os.path.join(dir_name,"4"))

create_dir("train")
create_dir("val")
create_dir("test")

train_df = pd.read_csv('../input/train/train.csv')




def fill_in_dir(dir_name, start_ind, end_ind):
    pet_ids = train_df['PetID'].values[start_ind:end_ind]
    PhotoAmts = train_df['PhotoAmt'].values.astype(np.int64)[start_ind:end_ind]
    AdoptionSpeeds = train_df['AdoptionSpeed'].values.astype(np.int64)[start_ind:end_ind]
    
    for i,pet_id in enumerate(pet_ids):     
        PhotoAmt = PhotoAmts[i]
        AdoptionSpeed = AdoptionSpeeds[i]
        if PhotoAmt>0: 
            for j in range(PhotoAmt) :
                try:
                    image_name = f'{pet_id}-{j+1}.jpg'
                    #batch_image = image.load_img(f'../input/train/train_images/{image_name}')
                    #image.save_img(f'{dir_name}/{AdoptionSpeed}/{image_name}',batch_image)
                    image = cv2.imread(f'../input/train/train_images/{image_name}')
                    # отразим изображение по горизонтали
                    if AdoptionSpeed == 0:
                        flip_image = cv2.flip(image,1)
                        cv2.imwrite(f'{dir_name}/{AdoptionSpeed}/{pet_id}_{j+1}.jpg',flip_image)
                    cv2.imwrite(f'{dir_name}/{AdoptionSpeed}/{image_name}',image)
                except:
                    print("something is wrong")
                    pass

nb_of_pets = len(train_df)
last_to_train = int(nb_of_pets*0.8)
last_to_val = int(nb_of_pets*0.1)+last_to_train
fill_in_dir("train",0,last_to_train)
fill_in_dir("val",last_to_train+1,last_to_val)
fill_in_dir("test",last_to_val+1,nb_of_pets)   
    
    
os.replace("train/0","../input")   

os.open("train/0") 
    
    
    
    
    
    
    
    
    
    
    
    
    
    