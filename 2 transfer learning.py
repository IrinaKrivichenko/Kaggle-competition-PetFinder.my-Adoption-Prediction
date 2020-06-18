#https://github.com/sozykin/dlpython_course/blob/master/computer_vision/cats_and_dogs/cats_and_dogs_vgg16.ipynb

from tensorflow.python.keras.preprocessing.image import ImageDataGenerator
from tensorflow.python.keras.models import Sequential
from tensorflow.python.keras.layers import Activation, Dropout, Flatten, Dense
from tensorflow.python.keras.applications import VGG19
from tensorflow.python.keras.optimizers import Adam
from tensorflow.python.keras.metrics import categorical_accuracy



# Каталог с данными для обучения
train_dir = '/content/drive/My Drive/data/train'
# Каталог с данными для проверки
val_dir = '/content/drive/My Drive/data/val'
# Каталог с данными для тестирования
test_dir = '/content/drive/My Drive/data/test'
# Размеры изображения
img_width, img_height = 224, 224
# Размерность тензора на основе изображения для входных данных в нейронную сеть
# backend Tensorflow, channels_last
input_shape = (img_width, img_height, 3)
# Размер мини-выборки
batch_size = 64
# Количество изображений для обучения
nb_train_samples = 17500
# Количество изображений для проверки
nb_validation_samples = 3750
# Количество изображений для тестирования
nb_test_samples = 3750


datagen = ImageDataGenerator(rescale=1. / 255)

img_width, img_height = 224, 224
#Загружаем предварительно обученную нейронную сеть
vgg19_net = VGG19(weights='imagenet', include_top=False, input_shape=(img_width, img_height, 3))
#"Замораживаем" веса предварительно обученной нейронной сети VGG16
vgg16_net.trainable = False
vgg19_net.summary()

#Создаем составную нейронную сеть на основе VGG16¶
model = Sequential()
# Добавляем в модель сеть VGG16 вместо слоя
model.add(vgg19_net)
model.add(Flatten())
model.add(Dense(256))
model.add(Activation('relu'))
model.add(Dropout(0.5))
model.add(Dense(5))
model.add(Activation('softmax'))

model.compile(loss='categorical_crossentropy',
              optimizer=Adam(lr=1e-5), 
              metrics=['categorical_accuracy'])

#Генератор данных для обучения на основе изображений из каталога
train_generator = datagen.flow_from_directory(
    train_dir,
    target_size=(img_width, img_height),
    batch_size=batch_size,
    class_mode='categorical')


#Генератор данных для проверки на основе изображений из каталога
val_generator = datagen.flow_from_directory(
    val_dir,
    target_size=(img_width, img_height),
    batch_size=batch_size,
    class_mode='categorical')

#Генератор данных для тестирования на основе изображений из каталога
test_generator = datagen.flow_from_directory(
    test_dir,
    target_size=(img_width, img_height),
    batch_size=batch_size,
    class_mode='categorical')

#Обучаем модель с использованием генераторов
model.fit_generator(
    train_generator,
    steps_per_epoch=nb_train_samples // batch_size,
    epochs=30,
    validation_data=val_generator,
    validation_steps=nb_validation_samples // batch_size)

#Оцениваем качество работы сети с помощью генератора
scores = model.evaluate_generator(test_generator, nb_test_samples // batch_size)
print("Аккуратность на тестовых данных: %.2f%%" % (scores[1]*100))

model.save_weights("fine-tune_weights.h5")
model.save("fine-tune_model.h5", True)