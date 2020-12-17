import gzip
import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
import matplotlib.image as mapl_immage

from os import listdir
from os.path import isfile, join
import pathlib
mypath = str(pathlib.Path(__file__).parent.absolute()) + "\\"
print(mypath)

onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]
print(onlyfiles)

mnist_image = ['train-images-idx3-ubyte.gz',
            't10k-images-idx3-ubyte.gz']

folder_name = ["Trainset\\", "Testset\\"]

len_mnist_image = [60000, 10000]



#Extract image data:
for j in range(len(mnist_image)):

    f = gzip.open( mypath+mnist_image[j],'r')

    image_size = 28
    num_images = len_mnist_image[j]

    f.read(16)
    buf = f.read(image_size * image_size * num_images)
    data = np.frombuffer(buf, dtype=np.uint8).astype(np.float32)
    data = data.reshape(num_images, image_size, image_size, 1)

    #Save images
    data_lenght = len(data)
    for i in range(data_lenght):
        name_image = mypath+folder_name[j]+str(i+1) + ".png"
        image_array = np.asarray(data[i]).squeeze()
        mapl_immage.imsave(name_image, image_array)
