
###
### This is suggested analysis for cross-selling telemarketing efforts for a
### portuguese bank
### This script does the following:
###  0) sets up the environment
###  1) imports the freemium dataset from a text file
###  2) creates another version of the data with all missing values recoded to their mean
###  3) computes descriptive statistics and plots
###  4) estimates a logistic regression model and tree models
###  5) computes predictions, a confusion matrix, the lift for the top decile
###



###############################################################################
### setup
###############################################################################

# setup environment, make sure this library has been installed
if (!require(tree)) {install.packages("tree"); library(tree)}
# setup environment (if you want to use fancy tree plots)
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(party)) {install.packages("party"); library(party)}
if (!require(partykit)) {install.packages("partykit"); library(partykit)}
# a better scatterplot matrix routine
if (!require(car)) {install.packages("car"); library(car)}
# better summary tables
if (!require(psych)) {install.packages("psych"); library(psych)}

# import dataset from file (change the directory to where your data is stored)
setwd("C:/Users/oldye/Documents/CMU_2018/Spring2019/MarketingAnalytics_95832/FinalExam")

# read in the data (notice that the columns are separated with semicolons not commas)
bank=read.csv("bank-full.csv",header=TRUE,sep=";")



###############################################################################
### summarize the data
###############################################################################

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



###############################################################################
### understanding the data with descriptive statistics and graphics
###############################################################################

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

# boxplots, try it again with log scale--you can think about this as a percentage change
# and makes it easier to see differences between really large and small values
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


varlist = append(varlist[0:6], varlist[8:17])
varlist
###############################################################################
### estimate a stepwise regression model with all the variables and their interactions
###############################################################################

# estimate simple logistic regression (with just trainsample)
lrmdl=glm(y~duration+housing,data=bank[trainsample,varlist],family='binomial')
summary(lrmdl)

# run a step-wise regression
# first estimate the null model (this just has an intercept)
null = glm(y~1,data=bank[trainsample,varlist],family='binomial')
summary(null)

# second estimate a complete model (with all variables that you are interested in)
full = glm(y~.,data=bank[trainsample,varlist],family='binomial')
summary(full)

# alternatively if you want to specify a specific logistic regression you can uncomment
# the following line and specify the variables that you want in the formula
full2 = glm(y~.^2,data=bank[trainsample,varlist],family='binomial')
summary(full2)

# finally estimate the step wise regression starting with the null model
# if you change to steps=20 or steps=30 you will get larger model
fwd = step(full, scope=formula(full),steps=15,dir="forward")
summary(fwd)

# select the model you are interested in evaluating (rerun for different models)
# but do not use this for the null model
mdl=fwd

# predict probability (for validation sample) -- use these results when comparing models
padopter = predict(mdl,newdata=bank[validsample,varlist],type='response')
cadopter = (padopter>.25)+0
trueadopter = (as.vector(bank$y[validsample])=='yes')+0  # turn yes to 1 and no to 0
(results = xtabs(~cadopter+trueadopter) )  # confusion matrix (columns have truth, rows have predictions)
boxplot(padopter~bank$y[validsample])  # boxplot of probability for predictions
hist(padopter)  # histogram of probability of predicted adopters
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses

# compute the predictions for the 10% of most likely adopterers (for validation sample)
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected

# predict probability (for prediction sample) -- use this sample to determine accuracy of your final model
padopter = predict(mdl,newdata=bank[predsample,varlist],type='response')
cadopter = as.vector((padopter>.25)+0)  # classify the predictions as adopters or not
trueadopter = (as.vector(bank$y[predsample])=='yes')+0  # turn yes to 1 and no to 0
(results = xtabs(~cadopter+trueadopter))  # confusion matrix
boxplot(padopter~bank$y[predsample])  # boxplot of probability for predictions
hist(padopter)  # histogram of probability of predicted adopters
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses

# compute the predictions for the 10% of most likely adopterers (for prediction sample)
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected



###############################################################################
### estimate a tree model with all variables
###############################################################################

# use rpart to estimate a tree model
ctree = rpart(y~., data=bank[trainsample,varlist], control=rpart.control(cp=0.005))
summary(ctree)
plot(ctree)
text(ctree)
prp(ctree)
fancyRpartPlot(ctree)

# score the predictions from the model
mdl=ctree   # set mdl to one of the following models full, mytree, ctree

# predict probability (for validation sample) -- use these results when comparing models
padopter = predict(mdl,newdata=bank[validsample,varlist],type='prob') 
padopter = padopter[,2]   # returns a matrix of predictions, we only want predictions of adopter
cadopter = (padopter>.25)+0
trueadopter = (as.vector(bank$y[validsample])=='yes')+0  # turn yes to 1 and no to 0
(results = xtabs(~cadopter+trueadopter) )  # confusion matrix (columns have truth, rows have predictions)
boxplot(padopter~bank$y[validsample])  # boxplot of probability for predictions
hist(padopter)  # histogram of probability of predicted adopters
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses

# compute the predictions for the 10% of most likely adopterers (for validation sample)
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected

# predict probability (for prediction sample) -- use this sample to determine accuracy of your final model
padopter = predict(mdl,newdata=bank[predsample,varlist],type='prob')
padopter = padopter[,2]   # returns a matrix of predictions, we only want predictions of adopter
cadopter = as.vector((padopter>.25)+0)    # classify the predictions as adopters or not
trueadopter = (as.vector(bank$y[predsample])=='yes')+0  # turn yes to 1 and no to 0
(results = xtabs(~cadopter+trueadopter))  # confusion matrix
boxplot(padopter~bank$y[predsample])  # boxplot of probability for predictions
hist(padopter)  # histogram of probability of predicted adopters
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses

# compute the predictions for the 10% of most likely adopterers (for prediction sample)
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected

