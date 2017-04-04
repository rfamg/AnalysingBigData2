#Set working directory
#Read file
mydata<-read.csv("Betting.csv",header=T)


# First the multinomial Logit model

# install.packages("nnet")
library(nnet)

mydata$prog2<-relevel(mydata$Match_O, ref="Loss")
test<-multinom(prog2~HTGD+RED.H+RED.A+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1, 
               data=mydata)
summary(test)

#2-tailed z test
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#Exponentiate
exp(coef(test))

#Decision tree with party
# First install and invoke party

# install.packages("party")
library(party)

datactree <- ctree(Match_O~HTGD+RED.H+RED.A+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1, 
                   mydata, controls=ctree_control(mincriterion=0.9, minsplit=50))
print(datactree)
plot(datactree,type="simple")

#Misclassification error
tab<-table(predict(datactree), mydata$Match_O)
print(tab)
1-sum(diag(tab))/sum(tab)


# Questions 8-10

# 8) Exhibit 10 lists 20 matches played over two weekends in 2012 
# along with the values of the covariates. Use multinomial logistic 
# regression to predict the match outcome in all 20 cases listed in Exhibit 10.

BettingTest <- read.csv("BettingTest.csv", header=T)
str(BettingTest)


BettingTest$prog2 <- relevel(BettingTest$MATCH_O, ref="Loss")
test1<-multinom(prog2~HTGD+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1,
               data=BettingTest)
predict(test1, newdata = BettingTest)
summary(test1)

# 9) Apply the CHAID decision tree on the 20 matches listed in Exhibit 10 
# and compare the results with your answers obtained using multinomial logistic regression.

#2-tailed z test
z1 <- summary(test1)$coefficients/summary(test1)$standard.errors
p1 <- (1 - pnorm(abs(z1), 0, 1)) * 2
p1

#Exponentiate
exp(coef(test1))

#Decision tree with party

library(party)

datactree1 <- ctree(Match_O~HTGD+POINTS_H+POINTS_A+TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1,
                    mydata, controls=ctree_control(mincriterion=0.9, minsplit=50))
predict(datactree1, newdata = BettingTest)
print(datactree1)
plot(datactree1,type="simple")


# 10) If Peter were to choose one match from the list of 20 matches for 
# betting, which match should he choose? Discuss the reasons for your suggestion.



