#install.packages("keras")
library(keras)
#install_keras()
mnist <- dataset_mnist()

x_train <- array_reshape(mnist$train$x, c(nrow(mnist$train$x), 784))
x_test  <- array_reshape(mnist$test$x, c(nrow(mnist$test$x), 784))
y_train <- mnist$train$y
y_test  <- mnist$test$y

y_train[8]
image(matrix(x_train[8,],28,28))

y_train[10]
image(matrix(x_train[10,],28,28))

y_train[9]
image(matrix(x_train[9,],28,28))

x_train <- x_train / 255 
x_test  <- x_test  / 255

TrainModel <- function(X_train ,Y_train){
  
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 10, activation = 'softmax')
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  Y_train <- to_categorical(y_train, 10)
  
  model %>% fit(
    X_train, Y_train, 
    epochs = 30, batch_size = 128, 
    validation_split = 0.2
  )
  
  
  
return(model)
}
  
m <- TrainModel(x_train , y_train)


PredictModel <- function(X, model)
{
    pre <- model %>% predict_classes(X)
    return(pre)
}  
  
y_hat <- PredictModel(x_test,m) 

mean(y_hat==y_test)  
  