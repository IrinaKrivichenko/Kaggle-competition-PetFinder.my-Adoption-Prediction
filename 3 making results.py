import pandas as pd
import numpy as np
import os

os.chdir("I:/Documents/Kaggle/PetFinder.my Adoption Prediction/input")
models_acc = [ 1, 2]
batch_preds = np.zeros((len(models_acc),3,5))
for x in models_acc:
  model_name = '../input/train/train_img_'+ str(x)+'.csv'
  train_df = pd.read_csv(model_name)
  batch_preds[x-1]=train_df.as_matrix(columns=train_df.columns[1:])*x

probability_cols = ['probability_0','probability_1','probability_2','probability_3','probability_4']
train_df['prediction'] = np.argmax(batch_preds.mean(axis=0), axis=1)
train_df = train_df.drop(columns=probability_cols)

print(train_df)

def split(txt):
    x = txt.split("-")    
    return x[0]

train_df['PetID'] = train_df.filename.apply(split)

train_df = pd.get_dummies(train_df, prefix = 'prediction', columns =['prediction'])

print(train_df.columns)

agg_functions = ['mean', 'sum']
df = train_df.groupby(['PetID']).agg({'prediction_0':'sum',
                                  'prediction_1':'sum',
                                  'prediction_2':'sum',
                                  'prediction_3':'sum',
                                  'prediction_4':'sum',
                                  })

print(df)


#убираем MultiIndex (Hierarchical Index)
#df.columns = df.columns.map('_'.join)

# удаляем переменную train_df

del train_df

train_df = pd.read_csv('../input/train/train_img.csv')

result = pd.merge( train_df, df, on='id', how ='outer')
result.fillna(0)

