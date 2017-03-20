# BUS212 Session 1 script
# slightly modified full script; differs a little from the annotated version
# Uses data file "On_Time_50.csv" found on GitHub

# This script uses 2 packages
library(readr)  # provide additional data-reading functions
library(corrplot) # attractive correlation graphs
# Be sure to set your own working directory
#

setwd("~/Desktop/Brandes/Analysing Big Data 2/Analysing Big Data 2")
mydata <- read_csv("data/On_Time_50.csv")
# summarize data in various ways
dim(mydata)  # display number of rows, columns
names(mydata) # display var names
head(mydata) # show structure of the table
summary(mydata$ArrDelay)   # compute stats for one variable
#
# This table is huge; take a subset
# to simplify the subset command, first set up a vector of column names
varlist <- c("DepDelay", "ArrDelay", "AirTime", "Distance")
myvars <-subset (mydata, ArrDelay > 15, select=varlist)
str(myvars)

#As standard practice in model-building with "big data" we first split the large data frame into two partitions:
#
#* a TRAINING set, used to initially develop a model for use
#* a TEST set, used to validate the model with a new "out of sample" batch of data

#  before using pseudo-random numbers, set a starting point
set.seed(1234)   # initialize the randomm number generator
# "ind" will be a vectors of randomly generated 1s and 2s. There will be as
# many values as there are rows of data in the myvars df. 60% of the ind values 
# will equal 1 nad 40% will equal 2.

ind <- sample(2,nrow(myvars),replace=TRUE, prob=c(0.6,0.4)) 
head(as.data.frame(ind), 20) # show first 20 values of ind

train <- myvars[ind==1,]  # new df "train" will consists of randomly 
# chosen rows from myvars, corresponding to the 1's in ind.
test <- myvars[ind==2,]

dim(train)
dim(test)

cm <- cor(train, method="pearson")
cm
corrplot(cm, method= "ellipse", type="lower" )

# Estimate 2 linear models with training data

lm1 <- lm(ArrDelay ~ DepDelay, data = train)
summary(lm1)
lm2 <- lm(ArrDelay ~ DepDelay + AirTime, data = train)
summary(lm2)

# 2nd model is very slightly better
# re-estimate with test data
lm2t <- lm(ArrDelay ~ DepDelay + AirTime, data = test)
summary(lm2t)

# One additional step: use the the original lm2 model to predict values for test set
# then evaluate the accuracy of the  fitted values

fittest <- predict.lm(lm2, newdata=test)
testeval <- as.data.frame(cbind(test$ArrDelay, fittest))
plot(train$ArrDelay, lm2$fitted.values)
plot(test$ArrDelay, fittest, main="LM2 using Test data")

results <-cbind(train$ArrDelay, lm2$fitted.values)
# naive goodness of fit for training data
cm2 <- cor(results, method="pearson")
cm2
corrplot(cm2, method= "ellipse", type="lower" )
#
# naive goodness of fit with training coefficients and test data
cm3 <- cor(testeval, method="pearson")
cm3
corrplot(cm3, method= "ellipse", type="lower" )



  