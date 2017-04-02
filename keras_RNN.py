# LSTM for sequence classification in the IMDB dataset
import numpy
from keras.datasets import imdb
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers.embeddings import Embedding
from keras.preprocessing import sequence

# fix random seed for reproducibility
numpy.random.seed(7)

# load the dataset but only keep the top n words, zero the rest
top_words = 5000
#(X_train, y_train), (X_test, y_test) = imdb.load_data(nb_words=top_words)
# import drug number array and split into test/train

import pandas as pd
# import numpy as np
dataset = pd.read_csv('./numericalDrugsFrame_20.csv')
drug_dataset = dataset.values

random_y_set = pd.read_csv('./random_y.csv')
random_y = random_y_set.values

X_train = drug_dataset[0:999]
X_test = drug_dataset[1000:1999]
y_train = random_y[0:999]
y_test = random_y[1000:1999]
# truncate and pad input sequences
max_review_length = 20
X_train = sequence.pad_sequences(X_train, maxlen=max_review_length)
X_test = sequence.pad_sequences(X_test, maxlen=max_review_length)
# create the model
embedding_vecor_length = 32
model = Sequential()
model.add(Embedding(top_words, embedding_vecor_length, input_length=max_review_length))
model.add(LSTM(100))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
print(model.summary())
model.fit(X_train, y_train, nb_epoch=3, batch_size=64)
# Final evaluation of the model
scores = model.evaluate(X_test, y_test, verbose=0)
print("Accuracy: %.2f%%" % (scores[1]*100))
