#Initializing libraries
import numpy as np
import pandas as pd
import nltk as nlp


import os
print(os.listdir("../input"))

#Reading data set
data = pd.read_csv("C:/Users/rajeshkamasani/Desktop//check/SPAM text message 20170820 - Data.csv")

#change category 1 or 0
data["Category"] = [1 if each == "spam" else 0 for each in data["Category"]]

import re
nlp_data = str(data.iloc[2,:])
nlp_data = re.sub("[^a-zA-Z]"," ",nlp_data)

#Convert to lower case
return lower case
nlp_data = nlp_data.lower()

#tokenizing the data
nlp_data = nlp.word_tokenize(nlp_data)
#nlp_data = nlp_data.split() or we can do so

#Lemmatize
lemma = nlp.WordNetLemmatizer()
nlp_data = [lemma.lemmatize(word) for word in nlp_data]

#join data
nlp_data = " ".join(nlp_data)

import nltk as nlp
import re
description_list = []
for description in data["Message"]:
    description = re.sub("[^a-zA-Z]"," ",description)
    description = description.lower()   # buyuk harftan kucuk harfe cevirme
    description = nlp.word_tokenize(description)
    #description = [ word for word in description if not word in set(stopwords.words("english"))]
    lemma = nlp.WordNetLemmatizer()
    description = [ lemma.lemmatize(word) for word in description]
    description = " ".join(description)
    description_list.append(description) #we hide all word one section
    
#We make bag of word it is including number of all word's info
from sklearn.feature_extraction.text import CountVectorizer 
max_features = 3000 #We use the most common word
count_vectorizer = CountVectorizer(max_features = max_features, stop_words = "english")
sparce_matrix = count_vectorizer.fit_transform(description_list).toarray()
print("the most using {} words: {}".format(max_features,count_vectorizer.get_feature_names()))

#Split train and test dataset
separate our data is train and test
y = data.iloc[:,0].values   # male or female classes
x = sparce_matrix
from sklearn.model_selection import train_test_split
x_train, x_test, y_train, y_test = train_test_split(x,y, test_size = 0.1, random_state = 42)

#Model prediction
make model for predict
from sklearn.naive_bayes import GaussianNB
nb = GaussianNB()
nb.fit(x_train,y_train)
print("the accuracy of our model: {}".format(nb.score(x_test,y_test)))
