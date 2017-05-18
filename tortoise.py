import PIL.Image
from math import pi,sin,cos
import numpy as np

def rotate_image_anti_clock_wise(data,theta):
	width, height = data.__len__(), data[0].__len__()
	print width, height
	cx, cy = width / 2, height / 2
	new_image = np.zeros((width,height))
	cos_theta = cos(theta)
	sin_theta = sin(theta)
	for x, row in enumerate(data):
		for y, value in enumerate(row):
			xp = cx + (x - cx) * cos_theta - (y - cy) * sin_theta
			yp = cy + (x - cx) * sin_theta + (y - cy) * cos_theta
			if xp < 0 or yp < 0 or xp >= width or yp >= height:
				continue
			new_image[xp, yp] = data[x][y]
	return new_image

def rotate_image_clock_wise(data,theta):
	width, height = data[0].__len__(), data.__len__()
	print width, height
	cx, cy = width / 2, height / 2
	#new_image = np.zeros((height,width))
	new_image = np.zeros((1, width*height))
	flat_data = [data[i][j] for i in range(height) for j in range(width)]
	cos_theta = cos(theta)
	sin_theta = sin(theta)
	for y, row in enumerate(data):
		for x, value in enumerate(row):
			xp = cx + (x - cx) * cos_theta - (y - cy) * sin_theta
			yp = cy + (x - cx) * sin_theta + (y - cy) * cos_theta
			if xp < 0 or yp < 0 or xp >= width or yp >= height:
				continue
			new_image[0][int(yp)*width + int(xp)] = flat_data[int(y) * width + int(x)]
	return new_image

def imageToMatrix(image):
	# gets color values of pixels in the image and saves them
	# in the matrix
	width,height = image.size
	print image.size
	result = [[0 for x in range(width)] for y in range(height)]
	for i in  range(height):
		for j in range(width):
			result[i][j] = image.getpixel((j,i))[0]
	return result

def save_bitmap(file_name, matrix):
	with open(file_name,'w') as file:
		for row in matrix:
			for character in row:
				file.write(str(chr(int(character))))

def tortoise():
	image = PIL.Image.open("tortoise.png")
	matrix = imageToMatrix(image)
	save_bitmap("tortoise.txt", matrix)
	rotated_matrix = rotate_image_clock_wise(matrix,3*pi/2)
	save_bitmap("rotated.txt",rotated_matrix)

tortoise()