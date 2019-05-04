###############################################################################
###
### evaluate your 'best' prediction model from Part1 to predict the churn
### and LTV under alternative offers or scenarios (e.g., give everyone a free phone)
### 
### this script generates an cell2cell_results.csv table that is useful to
### compare the gains for different offers
###
###############################################################################


###############################################################################
### setup
###############################################################################

# setup environment, make sure this library has been installed
if (!require(tree)) {install.packages("tree"); library(tree)}
# setup environment (if you want to use fancy tree plots)
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
# data manipulation
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
# better describe
if (!require(psych)) {install.packages("psych"); library(psych)}




###############################################################################
### read in the data and prepare the dataset for analysis
###############################################################################

# import dataset from file (change the directory to where your data is stored)
setwd("~/Documents/class/marketing analytics/cases/cell2cell/data") #!! change to your directory !!
cell2cell=read.csv("cell2cell_data.csv")

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# prepare new values
trainsample=(cell2cell$Sample==1)
validsample=(cell2cell$Sample==2)
predsample=(cell2cell$Sample==3)
plotsample=sample(1:nrow(cell2cell),200)

# remove sample from the cell2cell set, since we have the sample variables
cell2cell$Sample=NULL

# recode the location so that we only keep the first 3 characters of the region
# and only remember the areas with more than 800 individuals, otherwise set the
# region to OTH for other
origcsa=cell2cell$Csa
newcsa=substr(as.vector(cell2cell$Csa),1,3)
csasize=xtabs(~newcsa)
csasizename=rownames(csasize)
for (i in 1:length(csasize)) {
  if (csasize[i]<=800) {
    newcsa[newcsa==csasizename[i]]="OTH"
  }
}
# overwrite the original Sca variable with the newly recoded variable
cell2cell$Csa=factor(newcsa)

# recode missing age1 and age2 values as their mean 
# and create an age1miss and age2miss variable
age1=as.vector(cell2cell$Age1)  # create a local copy with just values
age2=as.vector(cell2cell$Age2)
age1[is.na(age1)]=0  # replace missing values with 0
age2[is.na(age2)]=0
age1miss=((age1==0)+0)  # create an indicator for missing age values (which are 0)
age2miss=((age2==0)+0)
age1[age1==0]=mean(age1[age1>0])  # recode missing value values as the mean
age2[age2==0]=mean(age2[age2>0])
cell2cell$Age1=age1   # save the variables
cell2cell$Age1miss=age1miss
cell2cell$Age2=age2
cell2cell$Age2miss=age2miss

# replace missing values with means
for (i in 1:ncol(cell2cell)) {
  if (typeof(cell2cell[,i])=="double") {
    cell2cell[is.na(cell2cell[,i]),i]=mean(cell2cell[,i],na.rm=T)
  }
}




###############################################################################
### estimate your model (either tree or logistic)
### only estimates the model, does not do any diagnostics or statistics
### you must save your model to "mymodel" for the rest of the script
### !! use your best model from part 1
###############################################################################

# this logistic regression uses some important variables and is a gives a good model
# notice that this "." only refers to the variables in mvarlist, not all variables
mymodel = glm(Churn~Eqpdays+Retcall+Months+Overage+Mou+Changem,data=cell2cell[trainsample,],family='binomial')

# this gives tree model
#mymodel = rpart(Churn~., data=cell2cell[trainsample,], control=rpart.control(cp=0.005))

# give a summary of the model's trained parameters
summary(mymodel)




###############################################################################
### compute adjusted probabilities and LTV in the full dataset
###
### hint:
### you may want to create new campaigns as below for offerA
### you can then decide which offer to select for each user by looking to
### see which offer gives you the maximum return
###############################################################################

# set common values for computing ltv
irate=.05/12  # annual discount rate
adja=.5/.02; adjb=.5/.98  # save the adjustments for a and b when computing adjusted churn

### offer (none): compute predictions using your model (for all users)
if ("rpart" %in% class(mymodel)) {
  pchurn.unadj=predict(mymodel,newdata=cell2cell,type='vector')
} else {
  pchurn.unadj=predict(mymodel,newdata=cell2cell,type='response')
}
# now adjust predictions to project probability of churn in the original data
a=pchurn.unadj/adja
b=(1-pchurn.unadj)/adjb
pchurn=a/(a+b)
# compute LTV
ltv.unadj=cell2cell$Revenue*(1+irate)/(1+irate-(1-pchurn.unadj))
ltv=cell2cell$Revenue*(1+irate)/(1+irate-(1-pchurn))
cost=rep(0,length(pchurn))

### offer A: offer a free phone
cell2cell.offerA=cell2cell   # create a new copy of data
cell2cell.offerA$Eqpdays=0   # simulate effect of free phone
# compute predictions using model for all users
if ("rpart" %in% class(mymodel)) {
  pchurn.offerA=predict(mymodel,newdata=cell2cell.offerA,type='vector')
} else {
  pchurn.offerA=predict(mymodel,newdata=cell2cell.offerA,type='response')
}
# now adjust predictions to project probability of churn in the original data
a=pchurn.offerA/adja
b=(1-pchurn.offerA)/adjb
pchurn.offerA=a/(a+b)
# compute LTV for new offer (assumes cost of new phone is $200)
cost.offerA=rep(200,length(pchurn.offerA))  # assume $200 cost for every customer
ltv.offerA=cell2cell.offerA$Revenue*(1+irate)/(1+irate-(1-pchurn.offerA))-cost.offerA

### offer (new)
# if you want to create a new offer use offer A as a template

# compare original churn and adjusted (!! if you create more offers include them in result below)
result=cbind(pchurn.unadj,pchurn,pchurn.offerA,ltv.unadj,ltv,ltv.offerA,cost,cost.offerA)
head(result)
summary(result)




###############################################################################
### save the results to a file
###############################################################################

# create list of users to evaluate
userlist=1:nrow(cell2cell)   # use this line for full dataset
#userlist=c(119,240,30,32)   # uncomment this list for just our four selected users
#set.seed(123); userlist=sample(1:nrow(cell2cell),100)  # uncomment this line for random sample of 100 users

# create vector of variables used in model called mvarlist, and add other variables that we want to write out
# these lines require mymodel to be defined above, must either be rpart or glm
if ("rpart" %in% class(mymodel)) {
  mvarlist=names(mymodel$variable.importance)   # get the list of variables from rpart by looking at the importance
} else {
  mvarlist=names(coefficients(mymodel))[-1]     # get the variables used in your logistic regression moodel, except the intercept which is in first position
}
evarlist=c("Customer","Revenue")     # vector of extra variables to save -- regardless of whether they are in the model
varlist=c(mvarlist,evarlist)         # vector of variable names that we will use (all model variables plus ID and revenue)
print(varlist)  # vector of variables to save

# retrieve data about the users  (model.matrix may not work for complex trees)
if ("rpart" %in% class(mymodel)) {
  modeldata=as.data.frame(cell2cell[userlist,mvarlist])      # extract the variables directly from the data
} else {
  modeldata=model.matrix(mymodel,data=cell2cell[userlist,])   # construct the data used in the model
}
userdata=cell2cell[userlist,evarlist]  # extract the additional variables that we want
userdata=cbind(modeldata,userdata)  # change to dataframe
print(userdata)   # print out user data
head(userdata)

# combine results with userdata and write to augmented result
augresult=cbind(userdata,result[userlist,])  # only selects user in userlist
head(augresult)

# create a table with all the data (this is helpful if you want to work with the data in excel)
write.csv(augresult,file="cell2cell_results.csv")   # if you want you can import this file into excel for easier processing

