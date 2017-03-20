#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#  White Wine
#  Written by Rafael Guimaraees
#  Date March 20th 2017
#  Version 1.0
#  ---------------------------------------------------------------------------
#  ---------------------------------------------------------------------------

#  ---------------------------------------------------------------------------
#  Packages
#  ---------------------------------------------------------------------------
library(fBasics) # Show basic aspect of the data
library(readr)  # provide additional data-reading functions
library(corrplot) # attractive correlation graphs
library(ggplot2) # for plots
library(data.table)
library(dplyr)
library(reshape2)
library(corrgram)


#  ---------------------------------------------------------------------------
#  Preparing data
#  ---------------------------------------------------------------------------

# Set location
setwd("~/Desktop/Brandes/Analysing Big Data 2/Analysing Big Data 2")

# Loading data

white_wine <- read_delim("https://raw.githubusercontent.com/rfamg/Big-Data-II-Spring-2017/master/winequality-white.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

dim(white_wine)
head(white_wine)
names(white_wine)
summary(white_wine)
str(white_wine)

#  ---------------------------------------------------------------------------
#  Creating a traing set
#  ---------------------------------------------------------------------------

set.seed(1980)   # initialize the randomm number generator


ind <- sample(2,nrow(white_wine),replace=TRUE, prob=c(0.6,0.4)) 
head(as.data.frame(ind), 20) # show first 20 values of ind



train <- white_wine[ind==1,]  # new df "train" will consists of randomly 
# chosen rows from myvars, corresponding to the 1's in ind.
test <- white_wine[ind==2,]

head(train)
head(test)

#  ---------------------------------------------------------------------------
#  Summary of the traning set
#  ---------------------------------------------------------------------------


View(basicStats(train)[c("Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum"),])


#  ---------------------------------------------------------------------------
#  Create a Graph of distribution
#  ---------------------------------------------------------------------------


qplot(train$quality, 
      binwidth = 0.5,  
      main = "Histogram of Quality of Red wine", 
      xlab = "Quality",  
      fill=I("blue"), 
      col=I("blue"))


#  ---------------------------------------------------------------------------
#  Compute and plot the correlations
#  ---------------------------------------------------------------------------


str(train)
train_cor <- data.frame(train$`fixed acidity`, train$`volatile acidity`, 
                        train$`citric acid`, train$`residual sugar`,
                        train$chlorides, train$`free sulfur dioxide`,train$density,
                        train$pH, train$sulphates, train$alcohol, train$quality)

names(train_cor)
setnames(train_cor, old = c("train..fixed.acidity.","train..volatile.acidity.", "train..citric.acid.",        
                            "train..residual.sugar.", "train.chlorides","train..free.sulfur.dioxide.",
                            "train.density", "train.pH", "train.sulphates",        
                            "train.alcohol", "train.quality" ), 
         new = c("fixed.acidity.","volatile.acidity.", "citric.acid.",        
                 "residual.sugar.", "chlorides","free.sulfur.dioxide.",
                 "density", "pH", "sulphates",        
                 "alcohol", "quality" ))



cv <- cor(train_cor, method="pearson")# View Correlation as a matrix
View(cv)
corrplot(cv, method= "ellipse", type="lower" )

list <- as.matrix(cor(cv)) # creating a list of the correlations
list_cor <- arrange(melt(list), desc) # showing the most highly correlated in a list.

list_cor

corrgram(cv) # Plot the correlation as a matrix.


#  ---------------------------------------------------------------------------
#  Create a regression for the tranoing set
#  ---------------------------------------------------------------------------

lm <- lm(quality ~ alcohol + pH, data=train)
summary(lm)

#  ---------------------------------------------------------------------------
#  Create a regression for the test set
#  ---------------------------------------------------------------------------

lm2 <- lm(quality ~ alcohol + pH, data=test)
summary(lm2)
