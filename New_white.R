#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#  New White Wine
#  Written by Rafael Guimaraes, Yuanxi Yao and Yilan Tang
#  Date March 20th 2017
#  Version 1.0
#  ---------------------------------------------------------------------------
#  ---------------------------------------------------------------------------

#  ---------------------------------------------------------------------------
#  Packages
#  ---------------------------------------------------------------------------
library(readr)  # provide additional data-reading functions
library(corrplot) # attractive correlation graphs
library(ggplot2) # for plots
library(data.table)
library(dplyr)
library(reshape2)
library(corrgram)
library(MASS)  # Package MASS includes the LDA function
library(ggplot2)
library(lattice)
library(e1071)  # for SVM model
library(nnet)   # for basic neural net
library(party)
library(car)
library(glmnet)

#  ---------------------------------------------------------------------------
#  Preparing data
#  ---------------------------------------------------------------------------

# Set location
setwd("~/Desktop/Brandes/Analysing Big Data 2/Analysing Big Data 2")

# Loading data


white_wine <- read_delim("https://raw.githubusercontent.com/rfamg/Big-Data-II-Spring-2017/master/winequality-white.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)



setnames(white_wine, old = c("fixed acidity","volatile acidity", "citric acid", "residual sugar"           
                            ,"free sulfur dioxide", "total sulfur dioxide"), 
         new = c("fixed.acidity","volatile.acidity", "citric.acid", "residual.sugar",           
                 "free.sulfur.dioxide", "total.sulfur.dioxide"))


new_white <- read_delim("https://raw.githubusercontent.com/IBSBigData/Big-Data-II-Spring-2017/master/Project%201%20Wine/new_white.csv", 
                         ",", escape_double = FALSE, trim_ws = TRUE)

setnames(new_white, old = c("fixed acidity","volatile acidity", "citric acid", "residual sugar"           
                             ,"free sulfur dioxide", "total sulfur dioxide"), 
         new = c("fixed.acidity","volatile.acidity", "citric.acid", "residual.sugar",           
                 "free.sulfur.dioxide", "total.sulfur.dioxide"))


View(new_white)
dim(white_wine)
head(white_wine)
names(white_wine)
summary(white_wine)
str(white_wine)

#  ---------------------------------------------------------------------------
#  Quality as a factor
#  ---------------------------------------------------------------------------

white_wine$quality <- as.factor(white_wine$quality)

#  ---------------------------------------------------------------------------
#  Creating a traing set
#  ---------------------------------------------------------------------------

set.seed(1980)   # initialize the randomm number generator


temp <- sample(2,nrow(white_wine),replace=TRUE, prob=c(0.80,0.20)) # vector of random 1s & 2s

train <- white_wine[temp==1,]  # training subset uses rows where temp = 1
test <- white_wine[temp==2,]

dim(train)
dim(test)
View(head(train))
head(test)
names(train)

#  ---------------------------------------------------------------------------
#  Using Lasso to select the variables
#  ---------------------------------------------------------------------------

corrgram(train) # Plot the correlation as a matrix.

x <- as.matrix(train[,-1]) # Removes class
y <- as.double(as.matrix(train[, 1])) # Only class

# Fitting the model (Lasso: Alpha = 1)
set.seed(999)
cv.lasso <- glmnet(x, y, family='gaussian',  alpha=10)

# Results
plot(cv.lasso, xvar="lambda")
plot(fit10, main="LASSO")
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso)




#  ---------------------------------------------------------------------------
#  Create a formula
#  ---------------------------------------------------------------------------



form <- as.formula("quality ~  volatile.acidity + chlorides  + total.sulfur.dioxide + density + alcohol")



corrgram(train) # Plot the correlation as a matrix.

#  ---------------------------------------------------------------------------
#  Multinomial regression
#  ---------------------------------------------------------------------------

mod <- multinom(form, data=train)
hist(mod$fitted.values)
summary(mod$fitted.values)

lgpred <- predict(mod,test)
table(lgpred)

tab <- table(lgpred, test$quality)
print(tab)



#misclassification rate
1-sum(diag(tab))/sum(tab)  # this model misclassifies 26.4 % of cases


#  ---------------------------------------------------------------------------
#  CART model
#  ---------------------------------------------------------------------------

# Now a CART model
tree <- ctree(form, train, 
              controls=ctree_control(mincriterion=0.90, minsplit=10))
pred_tree <- predict(tree, train)
table(pred_tree)
tab <- table(pred_tree, train$quality)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)

#  ---------------------------------------------------------------------------
# Linear Discriminant Analysis
#  ---------------------------------------------------------------------------

fit <- lda(form, data=train,
           na.action="na.omit") 

pred_lda <- predict(fit, train)$class  # use lda model to predict using orig data
table(pred_lda)  # tabulate the predictions

tab <- table(pred_lda, train$quality)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab) 


#  ---------------------------------------------------------------------------
#  SVM - Support Vector Machine
#  ---------------------------------------------------------------------------

svm1 <- svm(form, data=train)

summary(svm1)
# now predict
pred_svm <- fitted(svm1)
table(pred_svm)
tab <- table(pred_svm, train$quality)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)  

#  ---------------------------------------------------------------------------
#  Neural Net
#  ---------------------------------------------------------------------------

mod_nn <- nnet(form, data = train, size = 15)
pred_nn <- predict(mod_nn, data=train, type = "class")  
table(pred_nn)
tab <- table(pred_nn, train$quality)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)

pred <- predict(svm1,new_white)

