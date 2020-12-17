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
