
###############################################################################
# Script: FordKa_Analysis.R
#
# R script to create customer segments for ford ka using k-Means
# Requires the excel spreadsheet with the data (FordKaData.xlsx).
# This script creates clusters using both the psycographic and demographic
# datasets using k-means analysis with 3 clusters.  You should consider
# different settings of k and may want to consider different sets of
# transformations of the input variables.
###############################################################################



###############################################################################
### setup the environment
###############################################################################

# load in additional packages to extend R functionality
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(gplots)) {install.packages("gplots"); library(gplots)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(openxlsx)) {install.packages("openxlsx"); library(openxlsx)}

# set to your correct working directory
setwd("~/Documents/class/marketing analytics/cases/ford ka/data") #!! set to your directory



###############################################################################
### input data
###############################################################################

# read in Ford Ka datasets from the Excel file
forddemo=read.xlsx("FordKaData.xlsx",sheet=1,startRow=7,colNames=T,rowNames=F,cols=2:10)  # read the demographic data
fordpsyc=read.xlsx("FordKaData.xlsx",sheet=4,startRow=7,colNames=T,rowNames=F,cols=2:63)  # read the psychographic data
fordquest=read.xlsx("FordKaData.xlsx",sheet=5,startRow=7,colNames=T,rowNames=F,cols=2)  # read the question list

# if you have problems with read.xlsx you can read in the data from CSV files (make sure you uncomment lines below and download CSV files)
#forddemo=read.csv("FordKaDemographicData.csv",row.names=1)  # just the demographic data
#fordpsyc=read.csv("FordKaPsychographicData.csv",row.names=1)  # just the psychographic data
#fordquest=scan("FordKaQuestions.txt",what='a',sep='\n')  # question list, which is read as a vector

# transform the data to make it easier to use
fordquest=paste0(1:62,',',fordquest$Statement)  # convert the question list into a character string to make it easier to work with
afordquest=strtrim(fordquest,30)  # truncate the strings to the first 30 characters since some questions are quite long
ford=cbind(forddemo,fordpsyc)  # create a new dataframe with both demogrpahic and psychographic data

# create some lists of variables which we will use later in the script
nqlist=1:62  # sequence of numbers from 1 to 62
qlist=paste0("Q",nqlist)
# let's try to cluster our questions by transposing the question data
nshortqlist=c(30,57,53,1,4,12)  # short list of questions
shortqlist=paste0("Q",nshortqlist)  # append Q in front of the numbers to generate a list of questions to match variable names
shortqname=strtrim(fordquest[nshortqlist],30)  # the first 30 characters of the strings
nvars=match(qlist,colnames(ford))   # define list of numeric variables

# create new standardized datasets using the scale function (set the mean of the new variable to 0 and stddev to 1)
xforddemo=scale(forddemo)
xfordpsyc=scale(fordpsyc)
xford=scale(ford)



###############################################################################
### exploratory analysis of the data
###############################################################################

# to list the variables in each data frame
ls(forddemo)
ls(fordpsyc)
ls(ford)

# remember these data sets are made up of lists of objects
typeof(ford)      # notice that ford is a list
names(ford)       # this is the list of object names within ford
class(ford)       # the ford object itself is a special type of list known as a data.frame
attributes(ford)  # this prints an objects attributes -- which usually has names of columns and rows
str(ford)         # a more verbose way of checking information about an object is with structure

# descriptive statistics for all the variables
summary(ford)

# to print an individual variable, enter it by itself
ford$Age

# create tables to describe the data
xtabs(~Age,data=ford)
xtabs(~AgeCategory,data=ford)
xtabs(~ChildrenCategory,data=ford)
xtabs(~FirstTimePurchase,data=ford)
xtabs(~Gender,data=ford)
xtabs(~IncomeCategory,data=ford)
xtabs(~MaritalStatus,data=ford)
xtabs(~NumberChildren,data=ford)
xtabs(~PreferenceGroup,data=ford)

# to see the relationship between two variables do a cross-tab
xtabs(~FirstTimePurchase+IncomeCategory,data=ford)
# here is a more visualize representation of a table with a BalloonPlot (uncomment the line if you want to see it)
#balloonplot(table(ford$FirstTimePurchase,ford$IncomeCategory),xlab="FirstTimePurchase",ylab="IncomeCategory")

# let's plot all pairs of data in a matrix plot
pairs(~Age+Gender+FirstTimePurchase+IncomeCategory+MaritalStatus+NumberChildren,data=ford)
pairs(~jitter(Age)+jitter(Gender)+jitter(FirstTimePurchase)
      +jitter(IncomeCategory)+jitter(MaritalStatus)+jitter(NumberChildren),data=ford)

# create boxplots of the questions
# notice that instead of accessing entering each variable as 
# ford$Q1 we instead use the format ford[,"Q1"], R understands
# that we want the column of the object ford named Q1
# in this example we will refer to a list of ten questions
par(mfrow=c(4,1),mar=c(4,3,1,1))
boxplot(ford[,c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")])
boxplot(ford[,c("Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19")])
boxplot(ford[,c("Q20","Q21","Q22","Q23","Q24","Q25","Q26","Q27","Q28","Q29")])
boxplot(ford[,c("Q30","Q31","Q32","Q33","Q34","Q35","Q36","Q37","Q38","Q39")])
boxplot(ford[,c("Q40","Q41","Q42","Q43","Q44","Q45","Q46","Q47","Q48","Q49")])
boxplot(ford[,c("Q50","Q51","Q52","Q53","Q54","Q55","Q56","Q57","Q58","Q59")])
boxplot(ford[,c("Q60","Q61","Q62")])

# to compute the correlation matrix
qcor=cor(ford[,qlist])
# print out the correlations with just 2 digits, since it is a huge matrix
print(qcor,digit=1)
# here is a better visualization of the correlation matrix using a heatmap
qplot(x=Var1,y=Var2,data=melt(cor(ford[,qlist],use="p")),fill=value,geom="tile")+scale_fill_gradient2(limits=c(-1, 1))



###############################################################################
## first cluster analysis with psychographics
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# compute a k-means cluster with k=3 using just the psychographics
(grpA=kmeans(xford[,qlist],centers=3))

# plot the solutions against the Q1 and Q2
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow=c(1,1),mar=c(5,4,4,1)+.1)
plot(jitter(xford[,"Q1"]), jitter(xford[,"Q2"]),xlab=fordquest[1],ylab=fordquest[2],col=grpA$cluster)
points(grpA$centers[,c("Q1","Q2")],col=1:3,pch=8,cex=2)
legend("topleft",pch=8,bty="n",col=1:3,c("1","2","3"))

# compare the cluster solutions with the PreferenceGroup
xtabs(~ford$PreferenceGroup+grpA$cluster)

# summarize the centroids
grpAcenter=t(grpA$centers)   # create variable with the transpose of the centroids
rownames(grpAcenter)=afordquest  # add the question names
print(grpAcenter)   # print the centroid values for each question
parallelplot(t(grpAcenter),auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))  # create a parallel plot to visualize the centroid values
parallelplot(t(grpAcenter[nshortqlist,]),auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))  # a parallel plot with just a few questions

# to save the data to an Excel spreadsheet (one sheet with the centroids and another with cluster assignments)
write.xlsx(list(grpA$centers,grpA$cluster),file="FordKa_ResultsA.xlsx",colnames=T)
#write.csv(grpA$cluster,file="FordKa_ResultsAcluster.csv")
#write.csv(grpA$centers,file="FordKa_ResultsAcenters.csv")


###############################################################################
### second cluster analysis with demographics
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# compute a k-means cluster with k=3 using just demographics
# notice that we omit PreferenceGroup from the list, since we want to use
# this variable for PreferenceGroup
qdlist=c("Age","ChildrenCategory","FirstTimePurchase","Gender","IncomeCategory","MaritalStatus","NumberChildren")
nqdlist=match(qdlist,colnames(xford))  # numeric positions of variables
(grpB=kmeans(xford[,qdlist],centers=3))

# plot the solutions against Age and FirstTimePurchase
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow=c(1,1),mar=c(5,4,4,1))
plot(jitter(xford[,"Age"]), jitter(xford[,"FirstTimePurchase"]),xlab="Age",ylab="FirstTimePurchase",col=grpB$cluster)
points(grpB$centers[,c("Age","FirstTimePurchase")],col=1:3,pch=8,cex=2)
legend("topleft",pch=8,bty="n",col=1:3,c("1","2","3"))

# compare the cluster solutions with the PreferenceGroup
xtabs(~ford$PreferenceGroup+grpB$cluster)

# summarize the centroids
grpBcenter=t(grpB$centers)   # create variable with the transpose of the centroids
rownames(grpBcenter)=qdlist  # add the demographic labels
print(grpBcenter)   # print the centroid values for each question
parallelplot(t(grpBcenter),auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))  # create a parallel plot to visualize the centroid values
splom(~jitter(xford[,qdlist]),groups=grpB$cluster)  # matrix scatter plot with clusters identified

# to save the data to an Excel spreadsheet (one sheet with the centroids and another with cluster assignments)
write.xlsx(list(grpB$centers,grpB$cluster),file="FordKa_ResultsB.xlsx",colnames=T)
#write.csv(grpB$cluster,file="FordKa_ResultsBcluster.csv")
#write.csv(grpB$centers,file="FordKa_ResultsBcenters.csv")

