# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from tensorflow.python.keras.applications.resnet50 import preprocess_input
from tensorflow.python.keras.preprocessing.image import load_img, img_to_array

from tensorflow.python.keras.applications import ResNet50
from learntools.deep_learning.decode_predictions import decode_predictions
from IPython.display import Image, display


from keras.applications.resnet50 import ResNet50
from keras.preprocessing import image
from keras.applications.resnet50 import preprocess_input, decode_predictions
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

import os
print(os.listdir("../input"))

# Any results you write to the current directory are saved as output.
image_size = 224

def read_and_prep_images(img_path, img_height=image_size, img_width=image_size):
    img = load_img(img_path, target_size=(img_height, img_width)) 
    x = img_to_array(img) 
    x = preprocess_input(x)
    return(x)
    
def clean(data_set):  
    data_set['ihuman'] = "00"
    print("ihuman = 00")
    n = len(data_set)
    model =  ResNet50(weights='imagenet')
    print("model is made")
    data_set['PhotoAmt'] = data_set['PhotoAmt'].astype(np.int64)
    for i in range(n):
        id = data_set['PetID'][i]
        PhotoAmt = data_set['PhotoAmt'][i]
        if(PhotoAmt):
            for j in range(PhotoAmt):
                img_path = "../input/train_images/"+str(id)+"-"+str(j+1)+".jpg"
                img = image.load_img(img_path, target_size=(224, 224))
                x = image.img_to_array(img)
                x = np.expand_dims(x, axis=0)
                x = preprocess_input(x)
                preds = model.predict(x)
                most_likely_labels = decode_predictions(preds, top=11)
                display(Image(img_path))
                print(most_likely_labels)
      #data_set = data_set %>% select(-c("Name","RescuerID","Description","State"))
    return data_set
    
train = pd.read_csv("../input/train/train.csv")
#train = 
clean(train.loc[0:10,])