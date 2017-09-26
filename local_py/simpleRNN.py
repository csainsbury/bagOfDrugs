from keras.models import Sequential
from keras.layers import Dense, Dropout
from keras.layers import Embedding
from keras.layers import LSTM
import sklearn


import pandas as pd
import numpy as np
# import numpy as np
dataset = pd.read_csv('./interpolatedTS_hba1c_10y_60increments_2003-2013_locf.csv')
# dataset = pd.read_csv('./hba1c_10y_2002to2012_3mBins_locf.csv')
ts_dataset = dataset.values

random_y_set = pd.read_csv('./hba1c_4y_mortality_y_10y_60increments_2003-2013_locf.csv')
random_y = random_y_set.values

# split
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(ts_dataset, random_y, test_size = 0.2, random_state = 0)

# once done, extract the LinkIds used in the train/test set:
#
Xtest_LinkIds = X_test[:, 60:63]
np.savetxt('./Xtest_LinkIds.csv', Xtest_LinkIds, fmt='%.18e', delimiter=',')

#
Xtrain_linkIds = X_train[:, 60:62]
np.savetxt('./Xtrain_linkIds.csv', Xtrain_linkIds, fmt='%.18e', delimiter=',')

# and then subset the X_train/test to just the hba1c values using
#
X_test = X_test[:, :60]
#
X_train = X_train[:, :60]

# feature scaling
# from sklearn.preprocessing import StandardScaler
# sc = StandardScaler()
# X_train = sc.fit_transform(X_train)
# X_test = sc.transform(X_test)
#
# X_train = sklearn.preprocessing.normalize(X_train)
# X_test = sklearn.preprocessing.normalize(X_test)

model = Sequential()
model.add(Embedding(200, output_dim=256))
model.add(LSTM(128))
model.add(Dropout(0.5))
model.add(Dense(1, activation='sigmoid'))

model.compile(loss='binary_crossentropy',
              optimizer='rmsprop',
              metrics=['mse'])

model.fit(X_train, y_train, batch_size=1024, epochs=6)
score = model.evaluate(X_test, y_test, batch_size=256)

print("Accuracy: %.2f%%" % (score[1]*100))

# predict test set results
y_pred_asNumber = model.predict(X_test)
y_pred = (y_pred_asNumber > 0.11)
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)

print(cm)


from sklearn.metrics import roc_auc_score
roc_auc_score(y_test, y_pred_asNumber)

from sklearn import metrics
import matplotlib.pyplot as plt

fpr, tpr, _ = metrics.roc_curve(y_test, y_pred_asNumber)

fpr = fpr # false_positive_rate
tpr = tpr # true_positive_rate

# This is the ROC curve
plt.plot(fpr,tpr)
# plt.show()
plt.savefig('roc_mortality.png')
auc = np.trapz(tpr, fpr)

np.savetxt('./y_pred.csv', y_pred_asNumber, fmt='%.18e', delimiter=',')




















# LSTM for sequence classification in the IMDB dataset
import numpy
from keras.datasets import imdb
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers.embeddings import Embedding
from keras.preprocessing import sequence
from keras.layers.convolutional import Conv1D
from keras.layers.convolutional import MaxPooling1D
from keras.layers import Dropout






# fix random seed for reproducibility
numpy.random.seed(7)

# load the dataset but only keep the top n words, zero the rest
top_words = 4000
#(X_train, y_train), (X_test, y_test) = imdb.load_data(nb_words=top_words)
# import drug number array and split into test/train

import pandas as pd
# import numpy as np
dataset = pd.read_csv('./hba1c_10y_2002to2012_3mBins_linearInterpolation.csv')
drug_dataset = dataset.values

random_y_set = pd.read_csv('./hba1c_3y_mortality_y_10y_2002to2012_3mBins_linearInterpolation.csv')
random_y = random_y_set.values

X_train = drug_dataset[0:30398]
X_test = drug_dataset[30399:37998]
y_train = random_y[0:30398]
y_test = random_y[30399:37998]
# truncate and pad input sequences
max_review_length = 20
X_train = sequence.pad_sequences(X_train, maxlen=max_review_length)
X_test = sequence.pad_sequences(X_test, maxlen=max_review_length)
# create the model
embedding_vecor_length = 32
model = Sequential()
model.add(Embedding(top_words, embedding_vecor_length, input_length=max_review_length))
model.add(Conv1D(filters=32, kernel_size=3, padding='same', activation='relu'))
model.add(MaxPooling1D(pool_size=2))
model.add(Dropout(0.5))
model.add(LSTM(32))
model.add(Dropout(0.5))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])
print(model.summary())
model.fit(X_train, y_train, nb_epoch=40, batch_size=128)
# Final evaluation of the model
scores = model.evaluate(X_test, y_test, verbose=1)
print("Accuracy: %.2f%%" % (scores[1]*100))

# predict test set results
y_pred_asNumber = model.predict(X_test)
y_pred = (y_pred_asNumber > 0.5)
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)

from sklearn.metrics import roc_auc_score
roc_auc_score(y_test, y_pred_asNumber)

## code with Dropout
