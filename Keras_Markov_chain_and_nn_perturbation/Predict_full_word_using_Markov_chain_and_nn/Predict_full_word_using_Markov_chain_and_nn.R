#install.packages("keras")
#install.packages('R.matlab')

library(keras)
library(R.matlab)
library(rstudioapi)

# Set work directory to current R code document path 
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())

# load emnist data in R format (converted from matlab format)
load(file='emnist-byclass.RData') 

# max normalization and x,y spliting 
x_train <- emnist$dataset[[1]][[1]]/255
y_train <- emnist$dataset[[1]][[2]]
  
# removing emnist
rm(emnist)

TrainModel <- function(x, y){
  
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 52, activation = 'softmax')
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  num_classes <-length( unique(y) )
  y <- to_categorical(y, num_classes)
  
  model %>% fit(
    x, y, 
    epochs = 6, batch_size = 128, 
    validation_split = 0.2
  )
  
  return(model)
}

# digits removing - (we interested to train on letters only)
x <- x_train[!y_train %in% 0:9,]
y <- matrix(y_train[!y_train %in% 0:9,],,1) - 10

# weights extracting 
m.image <-TrainModel(x , y)

#saving model
save_model_hdf5(m.image, "EMNIST_Model.h5",overwrite = TRUE,include_optimizer = TRUE)


################ Sanity check - predictions  ###########################
m.image <- load_model_hdf5('EMNIST_Model.h5', custom_objects = NULL, compile = TRUE)

# rmooving variabels 
rm(x_train , y_train,x,y)

load(file='emnist-byclass.RData') 
x_test  <- emnist$dataset[[2]][[1]] / 255
y_test  <- emnist$dataset[[2]][[2]]    
rm(emnist)

# PredictModel function
PredictModel <- function(X, model)
{
  pre <- model %>% predict_classes(X)
  return(pre)
}  

# digits removing 
xt <- x_test[!y_test %in% 0:9,]
yt <- matrix(y_test[!y_test %in% 0:9,],,1) -10

# y_hat retrieving 
y_hat <- matrix(PredictModel(xt,m.image),,1)

# accuracy check        
mean(y_hat==yt)




