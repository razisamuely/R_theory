# Set work directory to current R code document path 
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())


PredictModelWord <- function(X,m.image,m.text){
  bm <- X 
  ln <- 5
  wn <- (dim(X)[1]*dim(X)[2]) / (28^2*ln) # num of words
  s  <- m.image
  
  # converting words pictures to matrix of 5*row_num  rows. so now row =letter
  
  h <- matrix(0,wn*ln,784)
  for (i in 1:wn) {
    a <- i*28
    g <- matrix(  apply( t( bm[(a-27):a,] ),1,rev ) ,,784 ,byrow = T  )
    
    for (j in 1:5) {
      k<-i*5
      h[(k-5)+j,] <- g[j,]
    }
  }
  
  
  # predict probabilities function 
  
  PredictModelproba <- function(X, model)
  {
    pre <- model %>% predict_proba(X)
    return(pre)
  }  
  
  p <- PredictModelproba(h,s)
  
  pm_n <- p[,1:26]+p[,27:52]
  
  # guessing first letter in each word only by neural network probabilities
  y_hat_p <- numeric(wn*ln)
  
  for (i in 1:wn) {
    i5 <- (i*5)-4
    y_hat_p[i5] <- which.max(pm_n[i5,])
  }
  
  # guessing the other letters in each word by neural network Weights and text analysis
  for (j in 1:(ln-1)) {
    
    t<-c((ln-1):1)
    for (i in 1:wn) {
      
      a= i*5-t[j]
      a
      r <- y_hat_p[(a)] #???? ??? ????? 
      
      e <- r+26*(1:26)-26 # ??????? ?? ?? ?????? ?? ???? ?????
      
      i5<-(i*5)-t[j]+1# ???? ???? ? ?? ???? ??? ???? ???? ???? ????? ????? ?
      
      y_hat_p[i5] <- which.max(pm_n[i5,]*m.text[e,j]) # ????? ?? ???????? ?????? ?? ???? ??? ?????? ???????? ????? ???? ??????
    }
  }
  
  # Converting numbers to letters
  y_hat <-letters[y_hat_p]
  
  # Converting list of letters to words
  words_hat <-numeric(wn)
  for (i in 1:wn) {
    w <-  i*ln
    lettres  <- y_hat[(w-(ln-1)):w]
    words_hat[i] <-paste(lettres, collapse = '')
  }
  return(words_hat)
}

#####$@$@$@ working check $@$@$@#####

##@@ load @@##
path = 'TEXT_Model.Rdata'
olde_text_path = 'TEXT_Model.Rdata'
load(olde_text_path)

library("keras")
image_path = "C:/Users/Razc/Documents/courses/third year/statisc/project/Missions/mission 2/EMNIST_Model_TomerMalichihamelech.h5"
m.image <- load_model_hdf5(filepath=image_path, custom_objects = NULL, compile = TRUE)

picture_path = 'C:/Users/Razc/Documents/courses/third year/statisc/project/Missions/mission 2/emnist_words.Rdata'
load(picture_path)
X1<-words$X
y1<-words$y

validation_path = 'C:/Users/Razc/Documents/courses/third year/statisc/project/Missions/mission 2/emnist_words_validation.Rdata'
load(validation_path)
X3<-words$X
y3<-words$y
##@@ creating X picture matrix @@##
X1 <- rbind(X1,X1,X1,X1,X1)

##@@ receiving one-dimensional array of string labels y @@##
y_hat1 <- PredictModelWord (X1,m.image,m.text)
y_hat3 <- PredictModelWord (X3,m.image,m.text)

##@@ accuracy check @@##
mean(y1==y_hat1)
y3 <- tolower(y3)
mean(y3==y_hat3)

