#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#  A Game of Two Halves 
#  Written by Rafael Guimaraes,  Yuanxi Yao and Yilan Tang
#  Date March 27th 2017
#  Version 1.0
#  ---------------------------------------------------------------------------
#  ---------------------------------------------------------------------------


#  ---------------------------------------------------------------------------
#  Library
#  ---------------------------------------------------------------------------

library(fBasics) # Show basic aspect of the data

library(nnet)
library(party)

#  ---------------------------------------------------------------------------
#  Data understanding
#  ---------------------------------------------------------------------------



#uploading data

df.train <- read.csv("~/Desktop/Brandes/Analysing Big Data 2/Analysing Big Data 2/data/Betting.csv")
df.test <- read.csv("~/Desktop/Brandes/Analysing Big Data 2/Analysing Big Data 2/data/BettingTest.csv", header=T)

#Read file
#mydata<-read.csv("~/Desktop/Brandes/Analysing Big Data 2/Analysing Big Data 2/data/Betting.csv",header=T)

#  ---------------------------------------------------------------------------
#  Train data
#  ---------------------------------------------------------------------------

# First the multinomial Logit model



df.train$prog2<-relevel(df.train$Match_O, ref="Loss")
train<-multinom(prog2~HTGD+RED.H+RED.A+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1, 
               data=df.train)
summary(train)

#2-tailed z test
z <- summary(train)$coefficients/summary(train)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


#Exponentiate
exp(coef(train))

#Decision tree with party
# First install and invoke party


datactree <- ctree(Match_O~HTGD+RED.H+RED.A+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1, 
                   df.train, controls=ctree_control(mincriterion=0.9, minsplit=50))
print(datactree)
plot(datactree,type="simple")

#Misclassification error  
tab<-table(predict(datactree), df.train$Match_O)
print(tab)
1-sum(diag(tab))/sum(tab)


#Number 4

q4<-multinom(prog2~HTGD+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.1, 
            data=df.train)

summary(q4)


coef(q4)[2,2]*3+coef(q4)[2,4]*18+coef(q4)[2,5]*15+coef(q4)[2,6]*39+coef(q4)[2,2]*32

#  ---------------------------------------------------------------------------
#  test data
#  ---------------------------------------------------------------------------

# First the multinomial Logit model

names(test)

df.test$prog2<-relevel(df.test$MATCH_O, ref="Loss")
test<-multinom(prog2~HTGD*3+RED_H+RED_A+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1, 
                data=df.test)
summary(test)

#2-tailed z test
a <- summary(test)$coefficients/summary(test)$standard.errors
b <- (1 - pnorm(abs(z), 0, 1)) * 2
b

#Exponentiate
exp(coef(test))


#Decision tree with party
# First install and invoke party


datactree2 <- ctree(Match_O~HTGD+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1, 
                   df.train, controls=ctree_control(mincriterion=0.9, minsplit=50))
print(datactree2, newdata = df.test)
plot(datactree2,type="simple")

#Misclassification error  
tab2<-table(predict(datactree2), df.test$MATCH_O)
print(tab2)
1-sum(diag(tab2))/sum(tab2)





