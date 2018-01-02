
# 安装并加载Keras包
install.packages("devtools")
devtools::install_github("rstudio/keras")
library(keras)
install_keras()

# 安装并加载TensorFlow包
devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow()

# 输入下面代码
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)

max_features <- 20000
batch_size <- 32

# Cut texts after this number of words (among top max_features most common words)
maxlen <- 80  
cat('Loading data...\n')

imdb <- dataset_imdb(num_words = max_features)
x_train <- imdb$train$x
y_train <- imdb$train$y
x_test <- imdb$test$x
y_test <- imdb$test$y
view(x_train)


cat(length(x_train), 'train sequences\n')
cat(length(x_test), 'test sequences\n')
cat('Pad sequences (samples x time)\n')

x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)

cat('x_train shape:', dim(x_train), '\n')
cat('x_test shape:', dim(x_test), '\n')

cat('Build model...\n')
model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = max_features, output_dim = 128) %>% 
  layer_lstm(units = 64, dropout = 0.2, recurrent_dropout = 0.2) %>% 
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

cat('Train...\n')
model %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = 15,
  validation_data = list(x_test, y_test)
)

# 模型的准确度度量
scores <- model %>% evaluate(
  x_test, y_test,
  batch_size = batch_size
)

cat('Test score:', scores[[1]])
cat('Test accuracy', scores[[2]])


library(randomForest)

y_train <- as.factor(y_train)
y_test <- as.factor(y_test)
rf <- randomForest(x=x_train,y=y_train,ntree=1000)
predict <- predict(rf,newdata=x_test)

