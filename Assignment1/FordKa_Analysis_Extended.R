
###############################################################################
# Script: FordKa_Analysis_Extended.R
#
# R script to create customer segments for ford ka using k-Means
# Requires the excel spreadsheet with the data (FordKaData.xlsx).
# This script creates clusters using both the psychographic and demographic
# datasets using k-means analysis with varying clusters.
#
# Changes that you need to make have "!!!"
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
if (!require(gmodels)) {install.packages("gmodels"); library(gmodels)}

# set to your correct working directory  
setwd("~/Documents/class/marketing analytics/cases/ford ka/data") # !!! change this line !!!



###############################################################################
### input data
###############################################################################

# read in Ford Ka datasets from the Excel file
forddemo=read.xlsx("FordKaData.xlsx",sheet=1,startRow=7,colNames=T,rowNames=F,cols=2:10)  # read the demographic data
fordpsyc=read.xlsx("FordKaData.xlsx",sheet=4,startRow=7,colNames=T,rowNames=F,cols=2:63)  # read the psychographic data
fordquest=read.xlsx("FordKaData.xlsx",sheet=5,startRow=7,colNames=T,rowNames=F,cols=2)  # read the question list
fordseg=read.xlsx("FordKaData.xlsx",sheet=1,startRow=7,colNames=T,rowNames=F,cols=11:12)  # read the segments that ford created (do not use in cluster)

# if you have problems with read.xlsx you can read in the data from CSV files (make sure you uncomment lines below and download CSV files)
#forddemo=read.csv("FordKaDemographicData.csv",row.names=1)  # just the demographic data
#fordpsyc=read.csv("FordKaPsychographicData.csv",row.names=1)  # just the psychographic data
#fordquest=scan("FordKaQuestions.txt",what='a',sep='\n')  # question list, which is read as a vector
#fordseg=read.csv("FordKaSegmentData.csv")  # these are the segments that Ford came up with

# transform the data to make it easier to use
fordquest=paste0(1:62,',',fordquest$Statement)  # convert the question list into a character string to make it easier to work with
afordquest=strtrim(fordquest,30)  # truncate the strings to the first 30 characters since some questions are quite long
fordseg$SegName=as.factor(fordseg$SegmentName)  # convert the segment names into a factor for easier use as a classification variable
fordseg$SegmentName=NULL  # remove this variable
ford=cbind(forddemo,fordpsyc)  # create a new dataframe with both demogrpahic and psychographic data

# create some lists of variables which we will use later in the script
nqlist=1:62  # sequence of numbers from 1 to 62
qlist=paste0("Q",nqlist)
# let's try to cluster our questions by transposing the question data
nshortqlist=c(30,57,53,1,4,12)  # short list of questions
shortqlist=paste0("Q",nshortqlist)  # append Q in front of the numbers to generate a list of questions to match variable names
shortqname=strtrim(fordquest[nshortqlist],30)  # the first 30 characters of the strings
nvars=match(qlist,colnames(ford))   # define list of numeric variables

# define a list of demographic variables that we will use later in the script
dlist=c("Age","MaritalStatus","Gender","NumberChildren","IncomeCategory","FirstTimePurchase")

# create new standardized datasets using the scale function (set the mean of the new variable to 0 and stddev to 1)
xforddemo=scale(forddemo)
xfordpsyc=scale(fordpsyc)
xford=scale(ford)



###############################################################################
### initial exploratory analysis of the data
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

# to print a record use the selector for the first record
ford[1,]

# check the scaled data
xford[1,]





###############################################################################
######### Psychographic Cluster Analysis
###############################################################################



###############################################################################
### step 1) exploratory analysis of the data
###############################################################################

# create tables to describe the data
xtabs(~PreferenceGroup,data=ford)
xtabs(~Q1,data=ford)

# to see the relationship between two variables do a cross-tab
xtabs(~PreferenceGroup+Q1,data=ford)
# here is a more visualize representation of a table with a BalloonPlot (uncomment the line if you want to see it)
balloonplot(table(ford$PreferenceGroup,ford$Q1),xlab="PreferenceGroup",ylab="Q1")

# optional part of script to create boxplots -- you can skip to "step 2"
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
## step 2) let's determine how many clusters to use by creating kmeans solutions
## with k from 2 to 30 and then we can plot the sum of square errors to 
## understand how much variation each solution explains
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(24895792)

# compute multiple cluster solutions
grpA2=kmeans(xford[,qlist],centers=2)
grpA3=kmeans(xford[,qlist],centers=3)
grpA4=kmeans(xford[,qlist],centers=4)
grpA5=kmeans(xford[,qlist],centers=5)
grpA6=kmeans(xford[,qlist],centers=6)
grpA7=kmeans(xford[,qlist],centers=7)
grpA8=kmeans(xford[,qlist],centers=8)
grpA9=kmeans(xford[,qlist],centers=9)
grpA10=kmeans(xford[,qlist],centers=10)
grpA15=kmeans(xford[,qlist],centers=15)
grpA20=kmeans(xford[,qlist],centers=20)
grpA30=kmeans(xford[,qlist],centers=30)

# compute between and within SS
kclust=c(2:10,15,20,30)
bss=c(grpA2$betweenss,
      grpA3$betweenss,grpA4$betweenss,grpA5$betweenss,grpA6$betweenss,
      grpA7$betweenss,grpA8$betweenss,grpA9$betweenss,grpA10$betweenss,
      grpA15$betweenss,grpA20$betweenss,grpA30$betweenss)
wss=c(grpA2$tot.withinss,
      grpA3$tot.withinss,grpA4$tot.withinss,grpA5$tot.withinss,grpA6$tot.withinss,
      grpA7$tot.withinss,grpA8$tot.withinss,grpA9$tot.withinss,grpA10$tot.withinss,
      grpA15$tot.withinss,grpA20$tot.withinss,grpA30$tot.withinss)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,bss,type="l",main="Between SS for k-means")
points(kclust,bss)
plot(kclust,wss,type="l",main="Within SS for k-means")
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")
points(kclust,bss/(wss+bss))



###############################################################################
### step 3) cluster analysis with psychographics and 3 clusters
### k=3 may be a good value based upon the previous analysis, but you may
### want to try having more or fewer clusters, and also change the set of
### variables that you use in the analysis
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# set the value of k
k=3  # !!! change this value to 2, 3, ... !!!

# compute a k-means cluster with specified k using just the psychographics
(grpA=kmeans(xford[,qlist],centers=k))

# plot the solutions against the Q1 and Q2
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow=c(1,1),mar=c(5,4,4,1)+.1)
plot(jitter(xford[,"Q1"]), jitter(xford[,"Q2"]),xlab=fordquest[1],ylab=fordquest[2],col=grpA$cluster)
points(grpA$centers[,c("Q1","Q2")],col=1:k,pch=8,cex=2)
legend("topleft",pch=8,bty="n",col=1:k,as.character(1:k))

# let's do a pairwise plot with the short list of questions
par(mfrow=c(length(shortqlist),length(shortqlist)),mar=c(4.5,4.5,0,0))
for (j in 1:length(shortqlist)) {
  for (i in 1:length(shortqlist)) {
    plot(jitter(xford[,shortqlist[i]]),jitter(xford[,shortqlist[j]]),
         xlab=shortqlist[i],ylab=shortqlist[j],col=grpA$cluster)
    points(grpA$centers[,c(shortqlist[i],shortqlist[j])],col=1:5,pch=8,cex=2)
    if (i==j) legend("topleft",pch=8,bty="n",col=1:k,as.character(1:k))
  }
}

# list out the short list of questions
fordquest[nshortqlist]

# compare the cluster solutions with the PreferenceGroup
# preference group #1 are "Ka Chooser (top 3)"
#                  #2 are Ka Non-Chooser (bottom 3)"
#                  #3 is "Middle (Middle 4)"
xtabs(~ford$PreferenceGroup+grpA$cluster)
CrossTable(ford$PreferenceGroup,grpA$cluster)   # slightly nicer cross tabulation
# here is a more visualize representation of a table with a BalloonPlot (uncomment the line if you want to see it)
par(mfrow=c(1,1))  # reset graphics to one panel
balloonplot(table(ford$PreferenceGroup,grpA$cluster),xlab="PreferenceGroup",ylab="Cluster")

# summarize the centroids
grpAcenter=t(grpA$centers)   # create variable with the transpose of the centroids
rownames(grpAcenter)=afordquest  # add the question names
print(grpAcenter)   # print the centroid values for each question
parallelplot(t(grpAcenter))  # create a parallel plot to visualize the centroid values
print(round(grpAcenter[nshortqlist,],2))  # print the centroid values for short list of questions
parallelplot(t(grpAcenter[nshortqlist,]),auto.key=list(text=as.character(1:k),space="top",columns=3,lines=T))  # a parallel plot with just a few questions

# to save the data to an Excel spreadsheet (one sheet with the centroids and another with cluster assignments)
write.xlsx(list(grpA$centers,grpA$cluster),file="FordKa_ResultsA.xlsx",colnames=T)
# if you have problems with write.xlsx you can uncomment the following lines
#write.csv(grpA$cluster,file="FordKa_ResultsAcluster.csv")
#write.csv(grpA$centers,file="FordKa_ResultsAcenters.csv")





###############################################################################
######### Demographic Cluster Analysis
###############################################################################



###############################################################################
### step 1) exploratory analysis of the data
###############################################################################

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
xtabs(~PreferenceGroup+AgeCategory,data=ford)
# here is a more visualize representation of a table with a BalloonPlot (uncomment the line if you want to see it)
balloonplot(table(ford$PreferenceGroup,ford$AgeCategory),xlab="PreferenceGroup",ylab="AgeCategory")

# let's plot all pairs of data in a matrix plot
pairs(~Age+MaritalStatus+Gender+NumberChildren+IncomeCategory+FirstTimePurchase,data=ford)
pairs(~jitter(Age)+jitter(MaritalStatus)+jitter(Gender)+jitter(NumberChildren)
      +jitter(IncomeCategory)+jitter(FirstTimePurchase),data=ford)



###############################################################################
## step 2) examine various cluster solutions with different values of k
## let's determine how many clusters to use by creating kmeans solutions
## with k from 2 to 30 and then we can plot the sum of square errors to 
## understand how much variation each solution explains
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1234)

# compute multiple cluster solutions
grpB2=kmeans(xford[,dlist],centers=2)
grpB3=kmeans(xford[,dlist],centers=3)
grpB4=kmeans(xford[,dlist],centers=4)
grpB5=kmeans(xford[,dlist],centers=5)
grpB6=kmeans(xford[,dlist],centers=6)
grpB7=kmeans(xford[,dlist],centers=7)
grpB8=kmeans(xford[,dlist],centers=8)
grpB9=kmeans(xford[,dlist],centers=9)
grpB10=kmeans(xford[,dlist],centers=10)
grpB15=kmeans(xford[,dlist],centers=15)
grpB20=kmeans(xford[,dlist],centers=20)
grpB30=kmeans(xford[,dlist],centers=30)

# compute between and within SS
kclust=c(2:10,15,20,30)
bssB=c(grpB2$betweenss,
      grpB3$betweenss,grpB4$betweenss,grpB5$betweenss,grpB6$betweenss,
      grpB7$betweenss,grpB8$betweenss,grpB9$betweenss,grpB10$betweenss,
      grpB15$betweenss,grpB20$betweenss,grpB30$betweenss)
wssB=c(grpB2$tot.withinss,
      grpB3$tot.withinss,grpB4$tot.withinss,grpB5$tot.withinss,grpB6$tot.withinss,
      grpB7$tot.withinss,grpB8$tot.withinss,grpB9$tot.withinss,grpB10$tot.withinss,
      grpB15$tot.withinss,grpB20$tot.withinss,grpB30$tot.withinss)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,bssB,type="l",main="Between SS for k-means")
points(kclust,bssB)
plot(kclust,wssB,type="l",main="Within SS for k-means")
points(kclust,wssB)
plot(kclust,bssB/(wssB+bssB),type="l",main="R-Squared for k-means")
points(kclust,bssB/(wssB+bssB))



###############################################################################
## step 3) analysis with demographics
## use the value of k from the previous analysis
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# compute a k-means cluster with specified number of k using just the demographics
k=3  # !!! change from 3 to whatever value you decide !!!
(grpB=kmeans(xford[,dlist],centers=k))

# plot the solutions against the Age and NumberChildren
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow=c(1,1),mar=c(5,4,4,1)+.1)
plot(xford[,"Age"],xford[,"NumberChildren"],xlab="Age",ylab="NumberChildren",col=grpB$cluster)
points(grpB$centers[,c("Age","NumberChildren")],col=1:k,pch=8,cex=2)
legend("topright",pch=8,bty="n",col=1:k,as.character(1:k))

# compare the cluster solutions with the PreferenceGroup
xtabs(~ford$PreferenceGroup+grpB$cluster)
CrossTable(ford$PreferenceGroup,grpB$cluster)   # slightly nicer cross tabulation
# here is a more visualize representation of a table with a BalloonPlot
balloonplot(table(ford$PreferenceGroup,grpB$cluster),xlab="PreferenceGroup",ylab="Cluster")

# summarize the centroids
grpBcenter=t(grpB$centers)   # create variable with the transpose of the centroids
rownames(grpBcenter)=dlist   # add the variable names
print(round(grpBcenter,2))   # print the centroid values for each question
parallelplot(t(grpBcenter),auto.key=list(text=as.character(1:k),space="top",columns=3,lines=T))  # create a parallel plot to visualize the centroid values

# to save the data to an Excel spreadsheet (one sheet with the centroids and another with cluster assignments)
write.xlsx(list(grpB$centers,grpB$cluster),file="FordKa_ResultsB.xlsx",colnames=T)
#write.csv(grpB$cluster,file="FordKa_ResultsBcluster.csv")
#write.csv(grpB$centers,file="FordKa_ResultsBcenters.csv")






###############################################################################
######### compare Psychographic and Demographic
###############################################################################

# compare the cluster solutions with the each other
xtabs(~grpA$cluster+grpB$cluster)
# here is a more visualize representation of a table with a BalloonPlot
balloonplot(table(grpA$cluster,grpB$cluster),xlab="Demo Cluster",ylab="Psych Cluster")

