import pandas as pd
import numpy as np
import os
from tqdm import tqdm, tqdm_notebook


img_size = 256
from keras.applications.densenet import preprocess_input, DenseNet121

def resize_to_square(im):
    old_size = im.shape[:2] # old_size is in (height, width) format
    ratio = float(img_size)/max(old_size)
    new_size = tuple([int(x*ratio) for x in old_size])
    # new_size should be in (width, height) format
    im = cv2.resize(im, (new_size[1], new_size[0]))
    delta_w = img_size - new_size[1]
    delta_h = img_size - new_size[0]
    top, bottom = delta_h//2, delta_h-(delta_h//2)
    left, right = delta_w//2, delta_w-(delta_w//2)
    color = [0, 0, 0]
    new_im = cv2.copyMakeBorder(im, top, bottom, left, right, cv2.BORDER_CONSTANT,value=color)
    return new_im

def load_image(path,pet_id,j):
    image = cv2.imread(f'{path}{pet_id}-{j}.jpg')
    new_image = resize_to_square(image)
    new_image = preprocess_input(new_image)
    return new_image
    
from keras.models import Model
from keras.layers import GlobalAveragePooling2D, Input, Lambda, AveragePooling1D
import keras.backend as K
inp = Input((256,256,3))
backbone = DenseNet121(input_tensor = inp, include_top = False)
x = backbone.output
x = GlobalAveragePooling2D()(x)
x = Lambda(lambda x: K.expand_dims(x,axis = -1))(x)
x = AveragePooling1D(4)(x)
out = Lambda(lambda x: x[:,:,0])(x)

m = Model(inp,out)

features = {}

train_df = pd.read_csv('../input/train/train.csv')
pet_ids = train_df['PetID'].values
PhotoAmts = train_df['PhotoAmt'].values.astype(np.int64)
for i,pet_id in enumerate(pet_ids):
    PhotoAmt = PhotoAmts[i]
    if PhotoAmt>0: 
        batch_images = np.zeros((PhotoAmt,img_size,img_size,3))
        for j in range(PhotoAmt) :
            try:
                batch_images[j] = load_image("../input/train_images/",pet_id,j+1)
            except:
                pass
        batch_preds = np.asarray(m.predict(batch_images))
        batch_preds = batch_preds.mean(axis=0)
    else:
        batch_preds = np.zeros((img_size))
    features[i] = batch_preds
    


train_feats = pd.DataFrame.from_dict(features, orient='index')
print("train_feats.shape = "+str(train_feats.shape) + " before concatination")
train_feats = pd.concat([train_df, train_feats.reset_index(drop=True)], axis=1)

print("train_feats.shape = "+str(train_feats.shape) + " after concatination")
train_feats.to_csv('train_img_features.csv')
train_feats.head()
# 19 sek for creating a model   15673 - 4.5 часа
test_df = pd.read_csv('../input/test/test.csv')
pet_ids = test_df['PetID'].values
PhotoAmts = test_df['PhotoAmt'].values.astype(np.int64)
for i,pet_id in enumerate(pet_ids):
    PhotoAmt = PhotoAmts[i]
    if PhotoAmt>0: 
        batch_images = np.zeros((PhotoAmt,img_size,img_size,3))
        for j in range(PhotoAmt) :
            try:
                batch_images[j] = load_image("../input/test_images/",pet_id,j+1)
            except:
                pass
        batch_preds = np.asarray(m.predict(batch_images))
        batch_preds = batch_preds.mean(axis=0)
    else:
        batch_preds = np.zeros((img_size))
    features[i] = batch_preds

        
        
test_feats = pd.DataFrame.from_dict(features, orient='index')
print("test_feats.shape = "+str(test_feats.shape) + " before concatination")

test_feats = pd.concat([test_df, test_feats.reset_index(drop=True)], axis=1, join='outer')
print("test_feats.shape = "+str(test_feats.shape) + " after concatination")
# 17,5 sek for creating a model   3592 - 1 час

test_feats.to_csv('test_img_features.csv')
test_feats.head()