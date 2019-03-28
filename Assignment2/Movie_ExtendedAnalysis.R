#
# Script: Movie_ExtendedAnalysis.R
#
# R script for for analyzing movie similarities
#
# Requires the following files:
#  opus_movies.txt              Movie characteristics of wide releases from 2006-2014
#  opus_movielens_tags.txt      Keywords that describe the movie from MovieLens
#  opus_keywords.txt            Keywords that describe the movie from Opus
#
# The data included for this exercise is for internal use only and
# may not be posted or distributed further.
# Specifically the files opus_movies.txt and opus_keywords.txt
# is data that is provided by The Numbers (http://www.the-numbers.com),
# powered by OpusData (http://www.opusdata.com).
# The opus_movielens_tags.txt is available from Movielens
# which is located at http://grouplens.org/datasets/movielens/latest
#




##################### setup environment  ######################

# setup environment
if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(plyr)) {install.packages("plyr"); library(plyr)}

# setup environment, make sure this library has been installed
if (!require(tree)) {install.packages("tree"); library(tree)}
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
# if you are using a mac then install XQuartz from http://xquartz.macosforge.org first
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(party)) {install.packages("party"); library(party)}
if (!require(partykit)) {install.packages("partykit"); library(partykit)}

# a better scatterplot matrix routine
if (!require(car)) {install.packages("car"); library(car)}

# better summary tables
if (!require(psych)) {install.packages("psych"); library(psych)}

# setup clustering
if (!require(fclust)) {install.packages("fclust"); library(fclust)}
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(gmodels)) {install.packages("gmodels"); library(gmodels)}  # nicer cross tabulations
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}

# setup topic modeling
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(NLP)) {install.packages("NLP"); library(NLP)}
if (!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
if (!require(tm)) {install.packages("tm"); library(tm)}
if (!require(slam)) {install.packages("slam"); library(slam)}




##################### input the data  ######################

## read in the data

# set to your correct working directory
setwd("~/Documents/class/marketing analytics/examples/movies")

# read in movie datasets
movies=read.delim("opus_movies.txt",header=T)  # the Opus movie data
tags=read.delim("opus_movielens_tags.txt",header=T)  # just the tags from movielens
keywords=read.delim("opus_keywords.txt",header=T) # the Opus keywords data

## make modifications to the dataset

# create a short version of the title
movies$short_name=strtrim(enc2native(as.character(movies$display_name)),20)
# change data formats
movies$release_date=as.Date(as.character(movies$release_date),format="%Y-%m-%d")
movies$release_month=format(movies$release_date,"%m")
tags$odid=as.factor(tags$odid)
keywords$odid=as.factor(keywords$odid)
# map the months to seasons
movies$release_season=rep('1Winter',length(movies$release_month))
movies$release_season[movies$release_month %in% c('03','04')]='2Spring'
movies$release_season[movies$release_month %in% c('05','06','07')]='3Summer'
movies$release_season[movies$release_month %in% c('08','09','10')]='4Fall'
movies$release_season[movies$release_month %in% c('11','12')]='5Holiday'
# remove punctuation from genre and rating
movies$rating=revalue(movies$rating,c("PG-13"="PG13"))
movies$genre=revalue(movies$genre,c("Black Comedy"="BlackComedy","Concert/Performance"="Performance","Romantic Comedy"="RomanticComedy","Thriller/Suspense"="Thriller"))
# create a matrix with genre and rating as 
dummygenre=model.matrix(~genre,movies)[,-1]  # omit the intercept in the first column
dummyrating=model.matrix(~rating,movies)[,-1]  # omit the intercept in the first column
# since these are matrix, we coerce them to lists, merge them, and then overwrite movies
movies=cbind(movies,as.data.frame(cbind(dummygenre,dummyrating)))
valgenre=colnames(dummygenre)
valrating=colnames(dummyrating)

# create a standardized version of the data
nvariables=sapply(movies,is.numeric)
nvariables=names(nvariables[nvariables])
smovies=scale(movies[,nvariables])

## transform the terms into a structure that can be used for topic modeling

# use this definition of mterms for movielens tags
# put data in sparse matrix form using simple_triplet_matrix as needed by LDA
mterms=simple_triplet_matrix(i=as.integer(tags$odid),j=as.integer(tags$tag),v=tags$count,
                             dimnames=list(levels(tags$odid),levels(tags$tag)))
# let's only keep words that are used frequently (by at least 20 movies)
mterms=mterms[,apply(mterms,2,sum)>=20]
# also delete any movies that do not have any terms
mterms=mterms[apply(mterms,1,sum)>0,]

# use this definition of mterms for Opus Keywords
# put data in sparse matrix form using simple_triplet_matrix as needed by LDA
##mterms=simple_triplet_matrix(i=as.integer(keywords$odid),j=as.integer(keywords$keyword),
##                                v=rep(1,length(keywords$keyword)),
##                                dimnames=list(levels(keywords$odid),levels(keywords$keyword)))

# determine dimensions of mterms
umovies=movies[movies$odid %in% as.integer(rownames(mterms)),]   # create a subset of the movies that have terms
lmterms=apply(mterms,1,sum)   # compute the sum of each of the rows (# of terms per movie)
lwterms=apply(mterms,2,sum)   # compute the sum of each of the columns (# of times word used)

# also create another version as DocumentTermMatrix
tmterms = as.DocumentTermMatrix(mterms,weight=weightTfIdf)

# create a vector with the names of the most frequent terms
topterms = findFreqTerms(tmterms,20)
idxtopterms = (1:ncol(mterms))[colnames(mterms) %in% topterms]  # get the indices of the topterms

# create a matrix with just the top keywords (cast this as a dense matrix)
movieterms = as.matrix(mterms[,topterms])




##################### explore the data  ######################

# to list the variables in each dataset
ls(movies)
ls(tags)
ls(keywords)

# remember these data sets are made up of lists of objects
typeof(movies)
names(movies)
class(movies)
attributes(movies)

# to print an individual movie, enter it by itself
movies[1,]

# create tables to describe the data
xtabs(~genre,data=movies)
xtabs(~rating,data=movies)
xtabs(~release_season,data=movies)

# to see the relationship between two variables do a cross-tab
xtabs(~genre+release_season,data=movies)

# here is a summary of all the variables
alist=ls(movies)
summary(movies[,alist])

# if we want to list the releases in a specific month
movies[format(movies$release_date,"%m-%Y")=="09-2014",c("odid","display_name","production_budget","genre","production_company1","release_date")]




##################### decision tree to predict release season  ######################

# estimate a tree to predict the release month
ctree = rpart(release_season~genre+rating+production_budget,data=movies, control=rpart.control(cp=0.01))
summary(ctree)
plot(ctree)
text(ctree)
prp(ctree)
fancyRpartPlot(ctree,cex=.7)




##################### k-means clustering of movies based upon characteristics ######################

# make a list of variables to include in a kmeans solution
qlist=c("production_budget","sequel",valgenre,valrating)

## let's determine how many clusters to use by computing many cluster solutions
grpA2=kmeans(smovies[,qlist],centers=2)
grpA3=kmeans(smovies[,qlist],centers=3)
grpA4=kmeans(smovies[,qlist],centers=4)
grpA5=kmeans(smovies[,qlist],centers=5)
grpA6=kmeans(smovies[,qlist],centers=6)
grpA7=kmeans(smovies[,qlist],centers=7)
grpA8=kmeans(smovies[,qlist],centers=8)
grpA9=kmeans(smovies[,qlist],centers=9)
grpA10=kmeans(smovies[,qlist],centers=10)
grpA15=kmeans(smovies[,qlist],centers=15)
grpA20=kmeans(smovies[,qlist],centers=20)
grpA30=kmeans(smovies[,qlist],centers=30)

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

## let's examine the k=9 solution in more depth
grpA=grpA9
valclusters=1:9

# plot the solutions against the production value and genreAction
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow=c(1,1),mar=c(5,4,4,1)+.1)
plot(movies$production_budget, jitter(movies$genreAction),col=grpA$cluster)
points(grpA$centers[,c("production_budget","genreAction")],col=valclusters,pch=8,cex=2)
legend("topright",pch=8,bty="n",col=valclusters,as.character(valclusters))

# compare the cluster solutions with the Release Season
CrossTable(movies$release_season,grpA$cluster)   # slightly nicer cross tabulation

# summarize the centroids
grpAcenter=t(grpA$centers)
rownames(grpAcenter)=strtrim(colnames(movies[,qlist]),40)
print(grpAcenter[qlist,])
parallelplot(t(grpAcenter[qlist,]))

# print a table with the movies assigned to each cluster
for (i in valclusters) {
  print(paste("* * * Movies in Cluster #",i," * * *"))
  print(movies$display_name[grpA$cluster==i])
}




##################### hierarchical clustering of movies based upon characteristics ######################

# try a hierarchical cluster on the question data
par(mfrow=c(1,1))
(grphQ=hclust(dist(smovies),method="complete"))
plot(grphQ,labels=movies$short_name,cex=.5)




##################### k-means clustering of movies based upon keywords ######################

# create a matrix from the terms for clustering
mtxterms=as.matrix(mterms)  # this is a dense matrix
# normalize terms as % of times used in movie to make movies more comparable
for (i in 1:nrow(mtxterms)) {
  mtxterms[i,]=mtxterms[i,]/sum(mtxterms[i,])
}


## let's determine how many clusters to use by computing many cluster solutions
grpB2=kmeans(mtxterms,centers=2)
grpB3=kmeans(mtxterms,centers=3)
grpB4=kmeans(mtxterms,centers=4)
grpB5=kmeans(mtxterms,centers=5)
grpB6=kmeans(mtxterms,centers=6)
grpB7=kmeans(mtxterms,centers=7)
grpB8=kmeans(mtxterms,centers=8)
grpB9=kmeans(mtxterms,centers=9)
grpB10=kmeans(mtxterms,centers=10)
grpB15=kmeans(mtxterms,centers=15)
grpB20=kmeans(mtxterms,centers=20)
grpB30=kmeans(mtxterms,centers=30)


# compute between and within SS
kclust=c(2:10,15,20,30)
bss=c(grpB2$betweenss,
      grpB3$betweenss,grpB4$betweenss,grpB5$betweenss,grpB6$betweenss,
      grpB7$betweenss,grpB8$betweenss,grpB9$betweenss,grpB10$betweenss,
      grpB15$betweenss,grpB20$betweenss,grpB30$betweenss)
wss=c(grpB2$tot.withinss,
      grpB3$tot.withinss,grpB4$tot.withinss,grpB5$tot.withinss,grpB6$tot.withinss,
      grpB7$tot.withinss,grpB8$tot.withinss,grpB9$tot.withinss,grpB10$tot.withinss,
      grpB15$tot.withinss,grpB20$tot.withinss,grpB30$tot.withinss)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,bss,type="l",main="Between SS for k-means")
points(kclust,bss)
plot(kclust,wss,type="l",main="Within SS for k-means")
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")
points(kclust,bss/(wss+bss))

## let's examine the k=10 solution in more depth
grpB=grpB10
valclusters=1:10

# plot the solutions against the production value and genreAction
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow=c(1,1),mar=c(5,4,4,1)+.1)
plot(umovies$production_budget, jitter(umovies$genreAction),col=grpB$cluster)

# compare the cluster solutions with the Release Season
CrossTable(umovies$release_season,grpB$cluster)

# compare the cluster solutions
CrossTable(grpB$cluster,grpA$cluster[movies$odid %in% umovies$odid])

# summarize the centroids
grpBcenter=t(grpB$centers)
rownames(grpBcenter)=strtrim(colnames(mterms),40)
print(grpBcenter)
parallelplot(t(grpBcenter[idxtopterms,]))

# print a table with the movies assigned to each cluster
for (i in valclusters) {
  print(paste("* * * Movies in Cluster #",i," * * *"))
  print(movies$display_name[grpB$cluster==i])
}




##################### estimate an LDA topic model using keywords  ######################

# setup the parameters for LDA control vector
burnin=1000     # number of initial iterations to discard for Gibbs sampler (for slow processors use 500)
iter=5000       # number of iterations to use for estimation  (for slow processors use 1000)
thin=50         # only save every 50th iteration to save on storage
seed=list(203,5,63,101,765)  # random number generator seeds
nstart=5        # number of repeated random starts
best=TRUE       # only return the model with maximum posterior likelihood

# estimate a series of LDA models (each run can take a few minutes depending upon your processor)
ClusterOUT2 = LDA(mterms,2,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT3 = LDA(mterms,3,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT4 = LDA(mterms,4,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT5 = LDA(mterms,5,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT6 = LDA(mterms,6,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT7 = LDA(mterms,7,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT8 = LDA(mterms,8,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT9 = LDA(mterms,9,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT10 = LDA(mterms,10,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT15 = LDA(mterms,15,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))


### compare the solutions

# generate a vector to store the likelihoods
ntopics=c(2:10,15)
llike=rep(NA,length(ntopics))
llike[1]=ClusterOUT2@loglikelihood
llike[2]=ClusterOUT3@loglikelihood
llike[3]=ClusterOUT4@loglikelihood
llike[4]=ClusterOUT5@loglikelihood
llike[5]=ClusterOUT6@loglikelihood
llike[6]=ClusterOUT7@loglikelihood
llike[7]=ClusterOUT8@loglikelihood
llike[8]=ClusterOUT9@loglikelihood
llike[9]=ClusterOUT10@loglikelihood
llike[10]=ClusterOUT15@loglikelihood
infocrit=2*llike-ntopics*ncol(mterms)  # this is a measure of fit that penalizes fit by the number of parameters

# plot the results to identify the "best" model using the fit as measured by loglikelihood or Bayesian-Information-Criterion (BIC)
par(mfrow=c(2,1))
plot(ntopics,llike,ylab="LogLikelihood")
lines(ntopics,llike)  # overlay a line
plot(ntopics,infocrit,ylab="Information Criterion")
lines(ntopics,infocrit)   # overlay a line
par(mfrow=c(1,1))


### analyze a particular model with k topics

# select a cluster to analyze, all the following lines use this cluster
# so change to a different cluster and repeat
ClusterOUT=ClusterOUT6

# matrix with probabilities of each question per topic
ClustTopics = exp(ClusterOUT@beta)
colnames(ClustTopics)=colnames(mterms)

# show the questions and associated topics
parallelplot(ClustTopics[,idxtopterms],main="Topic associated with selected Terms")
print(format(t(ClustTopics),digits=1,scientific=FALSE))   # print topics in columns and probabilities in rows

# probability of topic assignments (each movie has its own unique profile)
ClustAssign = ClusterOUT@gamma   # this is a matrix with the row as the movie and column as the topic
ClustBest = apply(ClustAssign,1,which.max)  # determine the best guess of a cluster, a vector with best guess
head(cbind(ClustAssign,ClustBest),n=10)   # show the actual topic probabilities and best guess associated with the first 10 movies

# show the topics associated with a selected movie
imovie=1
barplot(ClustAssign[imovie,],names.arg=1:ncol(ClustAssign),main=paste("Topics Associated with selected movie",umovies$display_name[imovie]))
# visualize the distribution of topics across the movies
parallelplot(ClustAssign,groups=ClustBest,ylab="Topic",main="Topic Assignments for each movie")
boxplot(ClustAssign,xlab="Topic",ylab="Probability of Topic across Movies")

# the score of each movie in the "topics" space gives a new way of trying to compare movies
plot(ClustAssign[,1],ClustAssign[,2])
# we can compute the distance between a target movie and all other movies in the "topics" space
imovie=1000
topicdist=ClustAssign-matrix(ClustAssign[imovie,],nrow=nrow(ClustAssign),ncol=ncol(ClustAssign),byrow=T)
topicdistss=apply(topicdist^2,1,sum)
# and identify the ten most similar movies movies are most similar
cutoff=sort(topicdistss)[10]  # select the 10th smallest sum of square distance
print(paste("Most similar movies to movie #",imovie,":",umovies$display_name[imovie]))
umovies[topicdistss<=cutoff,c("display_name","release_season")]

# for reference compute the k-means cluster
# chose the value of k to match with the number of topics in your topic model
(grpKmeans=kmeans(mterms,centers=6))

# determine the best guess for each person/question combination
ClustGuess=(ClustAssign%*%ClustTopics)*lmterms
ClustErr=mterms-ClustGuess   # errors associated with best guess
( withinss=sum(ClustErr^2) )   # sum of the squared errors associated with predictions
1-withinss/grpKmeans$totss     # or if we prefer we can compute the R-squared
sum(grpKmeans$withinss)        # we can compare this with the within sum-of-squares from the k-means

# we can compare the predictions for a selected movie
imovie=1
mcompare=cbind(ClustGuess[imovie,],as.vector(mterms[imovie,]))
print(mcompare)

# compare kmeans solutions with the topic model
# remember that kmeans assignments are deterministic, while topic models are probabilistic
# so this cross tab only considers the matches between the most likely
xtabs(~grpKmeans$cluster+ClustBest)

