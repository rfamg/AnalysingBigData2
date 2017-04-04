# Code for BUS212 Session 5

# demonstrate three more classifiers
# Linear Discrim Analysis
# Support Vector machines
# Neural Network

# First load all packages

library(MASS)  # Package MASS includes the LDA function
library(ggplot2)
library(lattice)
library(e1071)  # for SVM model
library(nnet)   # for basic neural net
library(party)

# First part of script based on iris dataset
#  Another way to split into train and test
n <- nrow(iris)
set.seed(752)
test_idx <- sample.int(n, size= round(0.2 * n))  # 20% test set
train <- iris[-test_idx,]  # train has all rows except the index 
test <- iris[test_idx,]


# Use Lattice Box-Whiskers plot for exploration & demo more R functionality
bwplot(Sepal.Length+Sepal.Width+Petal.Length+Petal.Width ~ Species, data=train,
       allow.multiple=T,   # allow multiple responses (lhs)
       outer=T,            # draw multiple plots separately
       pch=16, col="cyan", # plot character, color,
       alpha=0.5,          #   and semi-transparancy color for median
       scales="free")      # use scale individually for each panel

# plot the data similar to Foster & Provost
ggplot(train, aes(x=Sepal.Width, y=Petal.Width, 
                  color=Species, shape=Species))+
  geom_point(size=3) +
  ggtitle("Iris Species Discrimination")

# Specify a model formula to use in three models
form <- as.formula("Species ~ .") # potentially use all 4 features
# Linear Discriminant Analysis
fit <- lda(form, data=train,
           na.action="na.omit") 

pred_lda <- predict(fit, train)$class  # use lda model to predict using orig data
table(pred_lda)  # tabulate the predictions

table(pred_lda, train$Species)  # confusion matrix

# Now svm
svm1 <- svm(form, data=train)

summary(svm1)
# now predict
pred_svm <- fitted(svm1)
table(pred_svm)
table(pred_svm,train$Species)  # confusion matrix

# Lastly Neural Net
mod_nn <- nnet(form, data = train, size = 4)
pred_nn <- predict(mod_nn, data=train, type = "class")  
table(pred_nn)
table(pred_nn, train$Species)

###################################################
#  COMPREHENSIVE EXTENDED EXAMPLE  
#  NOW, swith to Framingham data
#  Use the five methods we know to build a model
#
fram <- read.csv("https://raw.githubusercontent.com/IBSBigData/Big-Data-II-Spring-2017/master/frmgham1.csv")
# Target variable is in final column "ANYCHD.2"
# ANYCHD.2 reads in as an integer, so make it a factor

fram$ANYCHD.2 <- factor(fram$ANYCHD.2)

table(fram$ANYCHD.2)  # dummy, 1 = has heart disease

# This dataset has a small number of missing values # for a few variables. For this demo, we'll drop cases
# with missing
framcols <- c(1:9, 39)  # subset main data frame,selecting 1st 9 + final column
framc <- na.omit(fram[,framcols])

table(framc$ANYCHD.2)
# Train and test sets
n <- nrow(framc)
set.seed(752)
test_idx <- sample.int(n, size= round(0.2 * n))  # 20% test set
train <- framc[-test_idx,]  # train has all rows except the index 
test <- framc[test_idx,]

# Specify a model formula to use in all models just to illustrate
form <- as.formula("ANYCHD.2 ~ SEX + TOTCHOL + AGE + SYSBP + DIABP + CURSMOKE + CIGPDAY + BMI") # potentially use eight features

# First, make a simple logistic regression. Because
# the target variable is binary, we don't need multinom
# simple binary logistic is done with glm command

mod_lg <- glm(form, family ="binomial", data=train)
hist(mod_lg$fitted.values)
summary(mod_lg$fitted.values)

lgpred <- ifelse(mod_lg$fitted.values > 0.5, 1, 0)
table(lgpred)

# Confusion matrix
tab <- table(lgpred, train$ANYCHD.2)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)  # this model misclassifies 26.4 % of cases

#  Same analysis using multinomial logit
# redundant but will help with Wine project

mod_mlog <-multinom(form,data=train)
summary(mod_mlog$fitted.values)

mlgpred <- ifelse(mod_mlog$fitted.values > 0.5, 1, 0)
table(mlgpred)

# Confusion matrix
tab <- table(mlgpred, train$ANYCHD.2)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)

#############################
# Now a CART model
tree <- ctree(form, train, 
              controls=ctree_control(mincriterion=0.90, minsplit=10))
pred_tree <- predict(tree, train)
table(pred_tree)
tab <- table(pred_tree, train$ANYCHD.2)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)


# Linear Discriminant Analysis
fit <- lda(form, data=train,
           na.action="na.omit") 

pred_lda <- predict(fit, train)$class  # use lda model to predict using orig data
table(pred_lda)  # tabulate the predictions

tab <- table(pred_lda, train$ANYCHD.2)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)  

#########################
##  Now svm
      
svm1 <- svm(form, data=train)

summary(svm1)
# now predict
pred_svm <- fitted(svm1)
table(pred_svm)
tab <- table(pred_svm, train$ANYCHD.2)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)  

# Lastly Neural Net
mod_nn <- nnet(form, data = train, size = 2)
pred_nn <- predict(mod_nn, data=train, type = "class")  
table(pred_nn)
tab <- table(pred_nn, train$ANYCHD.2)
print(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)  