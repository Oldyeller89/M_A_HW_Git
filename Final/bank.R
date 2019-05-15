
###
### This is suggested analysis for cross-selling telemarketing efforts for a
### portuguese bank
### This script does the following:
###  0) sets up the environment
###  1) imports the dataset from a text file
###  2) transform the data
###  3) understand the data computes descriptive statistics and plots
###  4) estimates a logistic regression model
###  5) computes predictions, a confusion matrix, the lift for the top decile
###



### setup

# setup environment
library(rattle)
# a better scatterplot matrix routine
library(car)
# better summary tables
library(psych)
# alternative data mining library
library(rminer)

# import dataset from file (change the directory to where your data is stored)
setwd("c:/alan/class/marketing analytics/cases/portuguese bank")

# read in the data (notice that the columns are separated with semicolons not commas)
bank=read.csv("bank-full.csv",header=TRUE,sep=";")



### transform the data

# general description of the data
summary(bank)

# compute the number of observations
nobs=nrow(bank)

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# prepare new values using a uniform random number, each record in freemium has 
# a corresponding uniform random value which will be used to decide if the observation
# is assigned to the training, validation or prediction sample
randvalue=runif(nobs)
trainsample=randvalue<.6    # 60% 
validsample=(randvalue>=.6 & randvalue<.9)
predsample=(randvalue>=.9)
plotsample=sample(1:nobs,300)

# create a list with the variables that will be used in the analysis
varlist=ls(bank)

# just a list of numeric variables
isnvar=sapply(bank,is.numeric)
nvarlist=attr(isnvar[isnvar],"names")



### understanding the data with descriptive statistics and graphics

# number of observations
sum(trainsample)
sum(validsample)
sum(predsample)

# let's take a look at just one observation
print(bank[1,])

# describe the data using only the training data
summary(bank[trainsample,varlist])

# use the describe function in the psych package to generate nicer tables
describe(bank[trainsample,varlist],fast=TRUE)

# do the same thing with the recoded data (but just for the training data)
describeBy(bank[trainsample,varlist],group=bank$y[trainsample],fast=TRUE)

# boxplots
par(mfrow=c(2,4))
boxplot(age~y,data=bank[plotsample,],xlab="adopter",ylab="age")
boxplot(balance~y,data=bank[plotsample,],xlab="adopter",ylab="balance")
boxplot(day~y,data=bank[plotsample,],xlab="adopter",ylab="day")
boxplot(duration~y,data=bank[plotsample,],xlab="adopter",ylab="duration")
boxplot(campaign~y,data=bank[plotsample,],xlab="adopter",ylab="campaign")
boxplot(pdays~y,data=bank[plotsample,],xlab="adopter",ylab="pdays")
boxplot(previous~y,data=bank[plotsample,],xlab="adopter",ylab="previous")


# boxplots (try it again with log scale)
par(mfrow=c(2,4))
boxplot(age~y,data=bank[plotsample,],xlab="adopter",ylab="age",log="y")
boxplot(balance~y,data=bank[plotsample,],xlab="adopter",ylab="balance")  # balance can be negative
boxplot(day~y,data=bank[plotsample,],xlab="adopter",ylab="day",log="y")
boxplot(duration~y,data=bank[plotsample,],xlab="adopter",ylab="duration",log="y")
boxplot(campaign~y,data=bank[plotsample,],xlab="adopter",ylab="campaign",log="y")
boxplot(pdays~y,data=bank[plotsample,],xlab="adopter",ylab="pdays")  # zero days
boxplot(previous~y,data=bank[plotsample,],xlab="adopter",ylab="previous")  # zero days


# cross tabs
xtabs(~job+y,data=bank)
xtabs(~marital+y,data=bank)
xtabs(~education+y,data=bank)
xtabs(~default+y,data=bank)
xtabs(~housing+y,data=bank)
xtabs(~loan+y,data=bank)
xtabs(~contact+y,data=bank)
xtabs(~month+y,data=bank)
xtabs(~poutcome+y,data=bank)

# compute correlation matrix (using only complete sets of observations)
#print(cor(bank[,varlist],use="pairwise.complete.obs"),digits=1)

# pairs
par(mfrow=c(1,1),mar=c(5,4,4,1))
pairs(bank[plotsample,varlist])

# nicer scatterplot matrix
par(mfrow=c(1,1),mar=c(5,4,4,1))
scatterplotMatrix(~age+balance+duration|y,data=bank[plotsample,])
scatterplotMatrix(~age+balance+day+duration+campaign+pdays+previous|y,data=bank[plotsample,])



### estimate logistic regressions models

# estimate simple logistic regression (with just trainsample)
lrmdl=glm(y~duration+housing,data=bank[trainsample,varlist],family='binomial')
summary(lrmdl)

# run model with only constant
null = glm(y~1,data=bank[trainsample,varlist],family='binomial')
summary(null)

# run model with all variables
full = glm(y~.,data=bank[trainsample,varlist],family='binomial')  # if you have more time try y~.^2
summary(full)

# run a step-wise regression
fwd = step(null, scope=formula(full),steps=15,dir="forward")
summary(fwd)




### score the predictions from the model

# select the model you are interested in evaluating (rerun for different models)
# but do not use this for the null model
mdl=fwd

# predict probability (for validation sample)
padopter = predict(mdl,newdata=bank[validsample,varlist],type='response')
cadopter = (padopter>.25)+0
(results = xtabs(~cadopter+bank$y[validsample]) )
(accuracy = results[1,1]/sum(results) )
(truepos = results[2,2]/(results[2,1]+results[2,2]))
boxplot(padopter~bank$y[validsample])  # boxplot of probability for predictions

# predict probability (for prediction sample)
padopter = predict(mdl,newdata=bank[predsample,varlist],type='response')
cadopter = as.vector((padopter>.25)+0)    # classify the predictions as adopters or not
trueadopter = bank$y[predsample]
xtabs(~cadopter+trueadopter)  # confusion matrix
hist(padopter)  # histogram of probability of predicted adopters

# compute the predictions for the 10% of most likely adopterers
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( actconv=xtabs(~cadopter[topadopter]+trueadopter[topadopter]) )
( baseconv=sum(trueadopter=="yes")/10 )  # how many would expect to be in the top decile purely due to chance
( lift=actconv[2]/baseconv )  # what is the ratio of how many we got to what we expected

