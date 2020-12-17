import pandas as pd
import numpy as np
from os import path

#1. Download this repo with database.
#2. Convert Original Lecun files into csv
def convert(imgf, labelf, outf, n):
    f = open(imgf, "rb")
    o = open(outf, "w")
    l = open(labelf, "rb")

    f.read(16)
    l.read(8)
    images = []

    for i in range(n):
        image = [ord(l.read(1))]
        for j in range(28*28):
            image.append(ord(f.read(1)))
        images.append(image)

    for image in images:
        o.write(",".join(str(pix) for pix in image)+"\n")
    f.close()
    o.close()
    l.close()


from os import listdir
from os.path import isfile, join
import pathlib
mypath = str(pathlib.Path(__file__).parent.absolute()) + "\\"
print(mypath)
#mypath = "E:\Program\R\Progetto ML\\"
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]
print(onlyfiles)


convert(mypath+"train-images.idx3-ubyte", mypath+"train-labels.idx1-ubyte",
mypath+"mnist_train.csv", 60000)
convert(mypath+"t10k-images.idx3-ubyte", mypath+"t10k-labels.idx1-ubyte",
mypath+"mnist_test.csv", 10000)

"""
#3. Read new csv files:
df_orig_train = pd.read_csv(mypath+'mnist_train.csv')
df_orig_test = pd.read_csv(mypath+'mnist_test.csv')
#4. Due to in train set our label column named '5' and in test set it's named '7'
#let's rename those into 'label' column.
df_orig_train.rename(columns={'5':'label'}, inplace=True)
df_orig_test.rename(columns={'7':'label'}, inplace=True)
#5. Writing our final dataframes files into new csv files.
df_orig_train.to_csv('mnist_train_final.csv', index=False)
df_orig_test.to_csv('mnist_test_final.csv', index=False)

#Reading files
#1. Read files
df_orig_train = pd.read_csv('mnist_train_final.csv')
df_orig_test = pd.read_csv('mnist_test_final.csv')
#2. Extract labels
labels_train = df_orig_train['label'].as_matrix()
#3. Extract digits
df_digits = df_orig_train.drop('label',axis=1)
#4. Display one training example
ind = 3
digits_data_train = df_digits.as_matrix()
digit = digits_data_train[ind]
digit = digit.reshape(28,28)
plt.title('this is  --->   ' + str(labels_train[ind]))
plt.imshow(digit, cmap='gray')
"""