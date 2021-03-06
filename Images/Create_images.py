import gzip
import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
import matplotlib.image as mapl_immage

import os
import pathlib
mypath = pathlib.Path(__file__).absolute().parents[1]
print("Actual path: " + str(mypath))

mnist_image = ['MNIST Dataset/train-images-idx3-ubyte.gz',
            'MNIST Dataset/t10k-images-idx3-ubyte.gz']

folder_name = ["Trainset", "Testset"]

len_mnist_image = [60000, 10000]



#Extract image data:
for j in range(len(mnist_image)):

    f = gzip.open( mypath / mnist_image[j],'r')

    image_size = 28
    num_images = len_mnist_image[j]

    f.read(16)
    buf = f.read(image_size * image_size * num_images)
    data = np.frombuffer(buf, dtype=np.uint8).astype(np.float32)
    data = data.reshape(num_images, image_size, image_size, 1)

    #Create directory if not exists
    directory = mypath / "Images"/ folder_name[j]
    if not os.path.exists(directory):
        os.makedirs(directory)

    #Save images
    data_lenght = len(data)
    for i in range(data_lenght):
        name_image = directory / pathlib.Path(str(i+1) + ".png")
        image_array = np.asarray(data[i]).squeeze()
        mapl_immage.imsave(name_image, image_array)

    print(folder_name[j] + " extracted")

print("End")
