#Training data

library(dplyr)
library(glmnet)
library(MASS)
library(ggplot2)
library(randomForest)
library(e1071)

dexter_train <- read.delim("D:/R_programs/dexter_train.txt", header = FALSE)
d <- data.frame(dexter_train)
train_df <- data.frame(matrix(0,ncol = 20000, nrow = 300))
cols = c(1:20000)
colnames(train_df) = cols

for (i in c(1:300)){
  x<-d %>% slice(i)
  x<-paste(unlist(x),collapse = " ")
  x<-unlist(strsplit(x," "))
  for (val in x){
    val<- unlist(strsplit(val,":"))
    train_df[i,val[1]] = val[2]
  }
}
train_df <- as.data.frame(sapply(train_df, as.numeric))

#Validation data

dexter_val <- read.delim("D:/R_programs/dexter_valid.txt", header=FALSE)
dval <- data.frame(dexter_val)
val_df <- data.frame(matrix(0,ncol = 20000, nrow = 300))
cols = c(1:20000)
colnames(val_df) = cols

for (i in c(1:300)){
  x<-dval %>% slice(i)
  x<-paste(unlist(x),collapse = " ")
  x<-unlist(strsplit(x," "))
  for (val in x){
    val<- unlist(strsplit(val,":"))
    val_df[i,val[1]] = val[2]
  }
}
val_df <- as.data.frame(sapply(val_df, as.numeric))

#train/validation labels

dexter_train_labels <- read.table("D:/R_programs/dexter_train_labels.txt", quote="\"", comment.char="")
dexter_val_labels <- read.table("D:/R_programs/dexter_valid_labels.txt", quote="\"", comment.char="")
dt1 <- data.frame(dexter_train_labels)
dt2 <- data.frame(dexter_val_labels)
dt1<-as.numeric(unlist(dt1))
dt2<-as.numeric(unlist(dt2))

dt1<- ifelse(dt1<1,0,1)
dt2<- ifelse(dt2<1,0,1)

#PCA on train/validation data

pca_train <- prcomp(train_df)

pca_train<- pca_train$x[,1:6]
p_comps_train <- cbind(pca_train)

train <- cbind(p_comps_train)
train <- data.frame(train)

pca_val <- prcomp(val_df, center = TRUE)
pca_val <- pca_val$x[,1:6]

p_comps_val <- cbind(pca_val)

val <- cbind(p_comps_val)
val <- data.frame(val)

#SVM

dexter_svm <- svm(y=dt1, x = train, data = train, kernel = 'linear', cost = 0.1)

yhat1 <- predict(dexter_svm,val)
yhat1 = ifelse(yhat1<0.5,0,1)
res_svm = data.frame(table(yhat1,dt2))

#Logistic Regression

dt1 = data.frame(dt1)
dt2 = data.frame(dt2)

train_reg = cbind(train, dt1)
#val_reg = cbind(val,dt2)

logistic_model <- glm(dt1 ~ ., data = train_reg, family = "binomial")

predict_reg <- predict(logistic_model, val_reg)

yhat2 = ifelse(predict_reg < 0.5, 0, 1)
#yhat2 = data.frame(yhat2)
#predict_reg = data.frame(predict_reg)
temp = ifelse(yhat2 == dt2,1 , 0)

#res_reg = data.frame(table(yhat2,dt2))

