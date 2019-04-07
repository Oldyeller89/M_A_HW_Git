#####################################################################################################
# Script: rfj_Script.R
# Copyright (c) 2019 by Alan Montgomery. Distributed using license CC BY-NC 4.0
# To view this license see https://creativecommons.org/licenses/by-nc/4.0/
#
# R script to compute linear regression example using refrigated juice data (this includes OJ)
#
# Requires the following files:
#   rfjdata.csv      weekly sales and price for each product and store
#   rfjstore.csv     store information
#   rfjupc.csv")     product information
#   rfjdemo.csv")    demographic information about stores
#   rfjdate.csv")    dates for each week           
#
# The data included for this exercise is for internal use only and
# may not be posted or distributed further.
#####################################################################################################


# setup library
setwd("C:/Users/josep/OneDrive/Documents/GitHub/M_A_HW_Git/Assignment3/data")
if (!require(psych)) {install.packages("psych"); library(psych)}


#####################################################################################################
# import and transform the data
#####################################################################################################

# import data
#setwd("C:\Users\sprih\OneDrive\Desktop\Marketing Analytics\M_A_HW_Git\Assignment3\data")
rfjdata=read.csv(file="rfjdata.csv")    # weekly sales and price for each product and store
rfjstore=read.csv(file="rfjstore.csv")  # store information
rfjupc=read.csv(file="rfjupc.csv")      # product information
rfjdemo=read.csv(file="rfjdemo.csv")    # demographic information about stores
rfjdate=read.csv(file="rfjdate.csv")    # dates for each week
print(dim(rfjdemo))
# remove blank stores
rfjdemo=rfjdemo[!is.na(rfjdemo$mmid),]

print(dim(rfjdemo))
#rfjdemo=rfjdemo[rfjdemo$mmid == "NA"]

# recode store and upc as factors
rfjdata$store=as.factor(rfjdata$store)
rfjdata$upc=as.factor(rfjdata$upc)
rfjdemo$store=as.factor(rfjdemo$store)
rfjupc$upc=as.factor(rfjupc$upc)

# transform the data by adding log price and log move
rfjdata$lprice=log(rfjdata$price)
rfjdata$lmove=log(rfjdata$move)


# let's create a subset of the data for just tropicana premium
trop=rfjdata[which(rfjdata$upc==4850000102 & rfjdata$price>0 & rfjdata$move>0),]

#add minute maid price comparison into trop data
competitor=rfjdata[which(rfjdata$upc==2500002606 & rfjdata$price>0 & rfjdata$move>0),]
myvars <- c("store", "week", "lprice","lmove")
competitor = competitor[myvars]
colnames(competitor) <- c("store", "week", "MMlprice", "MMmove")
trop <- merge(x = trop, y = competitor, by=c("store","week"))
#create binary row of whether tropicana is cheaper than MM
trop$lessThanMM <- trop$MMlprice > trop$lprice
#find comparative price difference (percentage lower than MM)
trop$MMpercCheaper <- (trop$MMlprice - trop$lprice) / trop$MMlprice

#Florida Gold
competitor=rfjdata[which(rfjdata$upc==1110000142 & rfjdata$price>0 & rfjdata$move>0),]  
myvars <- c("store", "week", "lprice","lmove")
competitor = competitor[myvars]
colnames(competitor) <- c("store", "week", "FGlprice", "FGmove")
trop <- merge(x = trop, y = competitor, by=c("store","week"))
#create binary row of whether tropicana is cheaper than MM
trop$lessThanFG <- trop$FGlprice > trop$lprice
#find comparative price difference (percentage lower than MM)
trop$FGpercCheaper <- (trop$FGlprice - trop$lprice) / trop$FGlprice

#HH OJ
competitor=rfjdata[which(rfjdata$upc==3828154001 & rfjdata$price>0 & rfjdata$move>0),] 
myvars <- c("store", "week", "lprice","lmove")
competitor = competitor[myvars]
colnames(competitor) <- c("store", "week", "HHlprice", "HHmove")
trop <- merge(x = trop, y = competitor, by=c("store","week"))
#create binary row of whether tropicana is cheaper than MM
trop$lessThanHH <- trop$HHlprice > trop$lprice
#find comparative price difference (percentage lower than MM)
trop$HHpercCheaper <- (trop$HHlprice - trop$lprice) / trop$HHlprice

#TreeFresh OJ
competitor=rfjdata[which(rfjdata$upc==7271850001 & rfjdata$price>0 & rfjdata$move>0),]
myvars <- c("store", "week", "lprice","lmove")
competitor = competitor[myvars]
colnames(competitor) <- c("store", "week", "TFlprice", "TFmove")
trop <- merge(x = trop, y = competitor, by=c("store","week"))
#create binary row of whether tropicana is cheaper than MM
trop$lessThanTF <- trop$TFlprice > trop$lprice
#find comparative price difference (percentage lower than MM)
trop$TFpercCheaper <- (trop$TFlprice - trop$lprice) / trop$TFlprice

#Trop pure prm
competitor=rfjdata[which(rfjdata$upc==4850000139 & rfjdata$price>0 & rfjdata$move>0),]
myvars <- c("store", "week", "lprice","lmove")
competitor = competitor[myvars]
colnames(competitor) <- c("store", "week", "pureTroplprice", "pureTropmove")
trop <- merge(x = trop, y = competitor, by=c("store","week"))
#create binary row of whether tropicana is cheaper than MM
trop$lessThanpureTrop <- trop$pureTroplprice > trop$lprice
#find comparative price difference (percentage lower than MM)
trop$pureTroppercCheaper <- (trop$pureTroplprice - trop$lprice) / trop$pureTroplprice

#trop homestyle
competitor=rfjdata[which(rfjdata$upc==4850000139 & rfjdata$price>0 & rfjdata$move>0),]
myvars <- c("store", "week", "lprice","lmove")
competitor = competitor[myvars]
colnames(competitor) <- c("store", "week", "HSlprice", "HSmove")
trop <- merge(x = trop, y = competitor, by=c("store","week"))
#create binary row of whether tropicana is cheaper than MM
trop$lessThanHS <- trop$HSlprice > trop$lprice
#find comparative price difference (percentage lower than MM)
trop$HSpercCheaper <- (trop$HSlprice - trop$lprice) / trop$HSlprice


trop$perCheaperThanMean <- (((trop$MMlprice+trop$FGlprice+trop$HHlprice+trop$TFlprice+trop$pureTroplprice+trop$HSlprice)/6) - trop$lprice) / trop$lprice
trop$cheapest <- (trop$lessThanMM & trop$lessThanFG & trop$lessThanHH & trop$lessThanTF & trop$lessThanpureTrop & trop$lessThanHS)

#merge demographics dataset into rfjdata
#trop = merge(x = trop, y = rfjdemo, by = "store")

# set the random number seed to the samples will be the same when re-run
nobs=nrow(trop)
set.seed(1248765792)
sample = sample.int(3, size=nobs, prob=c(.7,.15,.15), replace=TRUE)  # randomly split data into 3 groups
trainsample=(sample==1)     # put 70% of the observations in the training sample
validsample=(sample==2)     # put the other 15% in the validation sample
predsample=(sample==3)      # put the other 15% in the validation sample



#####################################################################################################
# pooled model
#####################################################################################################

## log linear model (pooled model)
( mdl1=lm(lmove~lprice+feat+disp,data=trop[trainsample,]) )

# plot all data
plot(lmove~lprice,data=trop)
# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(a=mdl1$coefficients["(Intercept)"]+mean(trop$disp)*mdl1$coefficients["disp"]+mean(trop$feat)*mdl1$coefficients["feat"],
       b=mdl1$coefficients["lprice"])
# just plot data for one store
plot(lmove~lprice,data=trop[which(trop$store==5),])
# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(a=mdl1$coefficients["(Intercept)"]+mean(trop$disp)*mdl1$coefficients["disp"]+mean(trop$feat)*mdl1$coefficients["feat"],
       b=mdl1$coefficients["lprice"])

# predict the validation sample using the model
pred1=predict(mdl1,newdata=trop,type='response')      # compute the predictions using the previous estaimtes
err1=(trop$lmove-pred1)    # compute the error = actual - predicted
sqerr1=(err1^2)            # compute the square of the error = error*error
abserr1=abs(err1)          # compute the absolute error = abs(error)
describeBy(cbind(err1,sqerr1,abserr1),group=sample,fast=TRUE)   # summarize the various measures of errors for the training and validation samples



#####################################################################################################
# store model
#####################################################################################################

## log linear model (store model)
# notice since store is a factor it will include store specific intercepts
#        store*lprice means to include a separate lprice coefficient for each store

#per store model
#( mdl2=lm(lmove~store+store*lprice+feat+disp,data=trop[trainsample,]) )

#improved model
( mdl2=lm(lmove~store+store*lprice+perCheaperThanMean+feat+disp,data=trop[trainsample,]) )


# plot the results
plot(lmove~lprice,data=trop[which(trop$store==5),])
# overlay regression line
coef=mdl2$coefficients[c("(Intercept)","disp","feat","store5","lprice","store5:lprice")]  # extract the coefficients
abline(a=coef[1]+mean(trop$disp)*coef[2]+mean(trop$feat)*coef[3]+coef[4],
       b=coef[5]+coef[6],col="blue")
# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(a=mdl1$coefficients["(Intercept)"]+mean(trop$disp)*mdl1$coefficients["disp"]+mean(trop$feat)*mdl1$coefficients["feat"],
       b=mdl1$coefficients["lprice"])

# predict the validation sample using the model
pred2=predict(mdl2,newdata=trop,type='response')      # compute the predictions using the previous estaimtes
err2=(trop$lmove-pred2)    # compute the error = actual - predicted
sqerr2=(err2^2)            # compute the square of the error = error*error
abserr2=abs(err2)          # compute the absolute error = abs(error)
describeBy(cbind(err2,sqerr2,abserr2),group=sample,fast=TRUE)   # summarize the various measures of errors for the training and validation samples

# save the output of the coefficients as a matrix
# we have to extract the store and store*lprice effects and organize them as a matrix so it is easier to deal with
cstoren=as.character(unique(trop$store))   # vector of store numbers
cstore=paste0("store",cstoren)  # create character vector with all store names and store numbers (notice store2 is base level and is dropped)
# first extract the common chain wide effect
mdl2.int=mdl2$coefficients["(Intercept)"]
mdl2.lprice=mdl2$coefficients["lprice"]
mdl2.feat=mdl2$coefficients["feat"]
mdl2.disp=mdl2$coefficients["disp"]
# next extract the store effects
mdl2.store.int=mdl2$coefficients[cstore]  # looks for all stores
mdl2.store.lprice=mdl2$coefficients[paste0(cstore,":lprice")]
# combine them together
mdl2.parm=cbind(mdl2.int,mdl2.lprice,mdl2.feat,mdl2.disp,mdl2.store.int,mdl2.store.lprice)
write.csv("mdl2.parm")  # you can import this into excel



#####################################################################################################
# extract the wholesale costs from week #100
#####################################################################################################

# find the wholesale costs for week #100
week100.cost=trop$cost[trop$week==100]

# find the retail prices actually charged for week #100
week100.price=trop$price[trop$week==100]

# create matrix with store #'s in rows
cbind(trop$store[trop$week==100],week100.cost,week100.price)

