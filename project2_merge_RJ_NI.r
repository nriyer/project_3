library(MASS)
library(dplyr)
library(rvgtest)
library(Hmisc)
library(reshape2)
library(caret)
library(FactoMineR)
#set wd
setwd("~/Desktop/GW_spring2016/bigdata")
mr <- read.table('agaricus-lepiota.data',sep=",",header=FALSE)
str(mr)

#########################################
# DEFINE FUNCTIONS THAT WILL BE USED    #
#########################################
#define function for preprocessing - using 'range'

scale <- function(df){
  pre_range <- preProcess(df,method="range")
  processed <- predict(pre_range,df)
  return(data.frame(processed))
}

#define function for wss and bss plots

wss_and_bss <- function(df){
  #within sum of squares
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 1:12) wss[i] <- sum(kmeans(df, 
                                       centers=i)$withinss)
  print(plot(1:12, wss, type="b", xlab="Number of Clusters",
             ylab="Within groups sum of squares"))
  
  #between sum of squares
  bss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 1:12) bss[i] <- sum(kmeans(df, 
                                       centers=i)$betweenss)
  print(plot(1:12, bss, type="b", xlab="Number of Clusters",
             ylab="Between groups sum of squares"))
}


#define function for final output
kmeans_iyer <- function(df,k,n=20){
  fit <-kmeans(df,k)
  #get cluster means:this illustrates amount of each characteristic in each cluster
  aggregate(df,by=list(fit$cluster), FUN=mean)
  #append cluster assignment
  cluster_assignment <- data.frame(df, fit$cluster)
  #table of how many variables fit in each cluster
  print(table(fit$cluster))
  km.out<- kmeans(cluster_assignment,k,nstart=n)
  print(km.out$tot.withinss)
  print(km.out$betweenss)
  print(km.out)
}

################################################################################################

#set names
names(mr) <- c("class", "capshape", "capsurface", "capcolor","bruises", "odor","gillattachment",
                "gillspacing","gillsize","gillcolor","stalkshape","stalkroot","stalksurfaceabovering",
                "stalksurfacebelowring","stalkcolorabovering","stalkcolorbelowring","veiltype",
                "veilcolor","ringnumber","ringtype","sporeprintcolor","population","habitat")
#data exploration
#plot all variables vs. class and ftable for each against class
for (i in names(mr)){
  plot(class ~ mr[,i], data=mr,xlab = i)
  print(ftable(mr$class,mr[,i]))
}
#look at pairwise correlations (look through all)
pairs(class ~ ccolor, data = mr)

#####################################
## GET DATA READY FOR CLUSTERING   ##
#####################################

numeric <- mr
#this translates column from factor to number
#loop to translate all to factors
for (i in names(numeric)){
  numeric[,i] <- as.numeric(numeric[,i])
}
str(numeric)
##melt and try ggplot to see each var against target
numeric$class <- mr$class
melt.mr <- melt(numeric)
#value represented by 'stat="identity)
ggplot(data=melt.mr,aes(x=variable,y=value)) + geom_bar(stat="identity")
qplot(variable, color=value, data=melt.mr, geom='density')

######################################
# INITIAL CLUSTERING - WITH CLASS    #
######################################

initial <- numeric
set.seed(12345)
row <- nrow(initial)
col <- col(initial)
#train/test 70-30
trainindex <- sample(row, 0.7*row, replace=FALSE)
train_initial <- initial[trainindex,]
test_initial <- initial[-trainindex,]
dim(train_initial)
##plot wss and bss
wss_and_bss(train_initial)
## optimum number here is 4 clusters
kmeans_iyer(train_initial,4)
#K-means clustering with 4 clusters of sizes 1536, 1988, 1332, 830
#Within cluster sum of squares by cluster:
#  [1] 52552.72 55329.35 22906.27 28094.94
#(between_SS / total_SS =  47.2 %)
kmeans_iyer(test_initial,4)
#Within cluster sum of squares by cluster:
#  [1] 25666.340  8925.573 10536.847 21587.822
#(between_SS / total_SS =  47.8 %)

##Now to run PCA and reduce dimensions, then try clustering again using
## K=3, K=5 and K=7 AND two other clustering methods - K-Nearest Neighbors
## and Hierarchical clustering

#################################
# Principal Component Analysis  #
#################################

PCA1 <- PCA(numeric, scale.unit = TRUE, ncp = 22, graph = TRUE,quali.sup=1:13 )
PCA1
PCA1$eig
PCA1$var
summary(PCA1) #we want components 1 - 4 because they have a eigenvalue of 1 and above 
dimdesc(PCA1, axes = 1:4) # to see which variables make an impact on the dimensions 
var(numeric)
dimdesc(PCA1, axes = 1:4)
myvars1 <- c("sporeprintcolor","stalkcolorabovering","stalkcolorbelowring","ringnumber", "gillsize", "habitat", "ringtype", "population", "bruises","veilcolor", "gillattachment", "stalkshape")
mush1 <- numeric[myvars1]
dim(mush1)
##12 variables


PCA2 <- PCA(numeric, scale.unit = TRUE, ncp = 22, graph = TRUE)
PCA2
PCA2$eig #will use components 1-7 
PCA2$var
summary(PCA2) # 
dimdesc(PCA2, axes = 1:7)
PCA2$var$contrib
PCA2$call
PCA2$ind
PCA2$ind$contrib
myvars2 <- c("sporeprintcolor","gillsize", "odor", "stalkshape","bruises","stalkroot", "gillcolor", "ringtype", "stalksurfaceabovering", "stalksurfacebelowring", "gillspacing", "population", "gillattachment","veilcolor", "habitat", "ringnumber", "capshape")
mush2 <- numeric[myvars2]
dim(mush2)
#17 variables

PCA3 <- PCA(numeric, scale.unit = TRUE, ncp = 22, graph = TRUE,quali.sup=(c(4,6,7,8,10,16)))
summary(PCA3)
PCA3$eig #will use components 1-6
dimdesc(PCA3, axes = 1:6)
myvars3 <- c("ringtype","gillcolor", "stalkroot", "bruises","odor", "gillsize", "sporeprintcolor","stalksurfaceabovering","stalksurfacebelowring","bruises","stalkcolorbelowring","stalkcolorabovering","gillspacing","capcolor","population", "stalkshape","habitat","ringnumber","capsurface","veilcolor","gillattachment")
mush3 <- numeric[myvars3]
dim(mush3)
#21 variables

###########################################################################
## CLUSTERING - now on reduced dimensionality set, we will use mush1      #
## except for on the optimal number of clusters, where we will try        #
## all three PCA sets                                                     #
###########################################################################

#using mush1



#take out target var and turn rest into numeric for clustering
cluster <- mush1
cluster <- mush1[,-1]
#no variance in vtype
cluster$vtype <- NULL

#split into training and test
row <- nrow(cluster)
col <- col(cluster)
set.seed(12345)

##train and test at 50-50 split
trainindex <- sample(row, 0.5*row, replace=FALSE)
train_50 <- cluster[trainindex,]
test_50 <- cluster[-trainindex,]
dim(train_50)


##train and test at 60-40 split
trainindex <- sample(row, 0.6*row, replace=FALSE)
train_60 <- cluster[trainindex,]
test_60 <- cluster[-trainindex,]
dim(train_60)


##train and test set at 70-30 split
trainindex <- sample(row, 0.7*row, replace=FALSE)
train_70 <- cluster[trainindex,]
test_70 <- cluster[-trainindex,]
dim(train_70)

#preProcess, use 'range' and then also try 
#to use center,scale for std. dev 1 and mean 0
#to get each factor between 0 and 1 for distance measures
#to be more accurate

####################
# 50-50 train/test #
####################

##range 
train1 <- scale(train_50)
test1 <- scale(test_50)

##turn both into matrix
train1 <- as.matrix(train1)
test1 <- as.matrix(test1)

wss_and_bss(train1)
#optimal looks like 4 clusters. Will use K=4, K=5 and K=7

set.seed(12345)
#K=4
kmeans_iyer(train1,4)

#K-means clustering with 4 clusters of sizes 914, 1037, 1052, 1059
#Within cluster sum of squares by cluster:
#  [1] 147.2922 206.5798 944.4668 738.4934
#(between_SS / total_SS =  79.4 %)

kmeans_iyer(test1,4)
#Within cluster sum of squares by cluster:
#  [1]  530.7143  192.7036 1048.5809  144.2148
#(between_SS / total_SS =  80.7 %)

#K=5
kmeans_iyer(train1,5)
#K-means clustering with 5 clusters of sizes 325, 643, 792, 1037, 1265

#Within cluster sum of squares by cluster:
#  [1] 147.4262 365.3925 383.6681 206.5798 491.7910
#(between_SS / total_SS =  86.7 %)

kmeans_iyer(test1,5)
#Within cluster sum of squares by cluster:
#  [1] 666.09769 192.70358 144.21480 321.24278  35.41523
#(between_SS / total_SS =  89.3 %)

#K=7
kmeans_iyer(train1,7)
#K-means clustering with 7 clusters of sizes 249, 297, 1037, 689, 282, 517, 991
#Within cluster sum of squares by cluster:
#  [1] 113.310071   2.319865 206.579776 219.178457 115.542778 237.388719 650.747341
#(between_SS / total_SS =  92.4 %)

kmeans_iyer(test1,7)
#Within cluster sum of squares by cluster:
#  [1] 144.2148048  85.9124272   0.5967213  16.9074503 954.5352683 192.4212013  35.4152284
#(between_SS / total_SS =  93.7 %)

###7 does the best but could be overfitting also may have something to do with 50-50 split


####################
# 60-40 train/test #
####################
##range 
train2 <- scale(train_60)
test2 <- scale(test_60)

##turn both into matrix
train2 <- as.matrix(train2)
test2 <- as.matrix(test2)

wss_and_bss(train2)
#optimal looks like 4 clusters. Will use K=4, K=5 and K=7

set.seed(12345)
#K=4
kmeans_iyer(train2,4)

#K-means clustering with 4 clusters of sizes 780, 1527, 1298, 1269
#Within cluster sum of squares by cluster:
#  [1] 420.6765 616.2767 243.3934 836.7560
#(between_SS / total_SS =  82.7 %)

kmeans_iyer(test2,4)
#Within cluster sum of squares by cluster:
#  [1] 322.1445 156.1216 267.2515 854.9291
#(between_SS / total_SS =  81.1 %)

#K=5
kmeans_iyer(train2,5)
#K-means clustering with 5 clusters of sizes 439, 1298, 1265, 780, 1092

#Within cluster sum of squares by cluster:
#  [1]  39.31298 243.39336 482.64913 420.67647 590.09397
#(between_SS / total_SS =  88.3 %)

kmeans_iyer(test2,5)
#Within cluster sum of squares by cluster:
#  [1] 418.8588 156.1216 343.2002 333.3569 123.3989
#(between_SS / total_SS =  81.4 %)

#K=7
kmeans_iyer(train2,7)
#K-means clustering with 7 clusters of sizes 740, 786, 1298, 792, 785, 348, 125
#Within cluster sum of squares by cluster:
#  [1] 420.67647 877.56689  26.82659 168.18897 150.10988 234.09223  84.79622
#(between_SS / total_SS =  89.4 %)

kmeans_iyer(test2,7)
#Within cluster sum of squares by cluster:
#  [1]  56.24304  29.34711 175.51366 108.94868 571.43948 144.53867  11.80223
#(between_SS / total_SS =  92.6 %)

###7 does the best again..

####################
# 70-30 train/test #
####################

##range 
train3 <- scale(train_70)
test3 <- scale(test_70)

##turn both into matrix
train3 <- as.matrix(train3)
test3 <- as.matrix(test3)

wss_and_bss(train3)
#optimal looks like 3 clusters. Will use K=3, K=5 and K=7

set.seed(12345)
#K=3
kmeans_iyer(train3,3)

#K-means clustering with 3 clusters of sizes 1813, 2245, 1628
#Within cluster sum of squares by cluster:
#  [1]  736.2414 1159.5432 1326.1124
#(between_SS / total_SS =  68.8 %)


kmeans_iyer(test3,3)
#Within cluster sum of squares by cluster:
#  [1] 323.6217 883.0550 128.3807
#(between_SS / total_SS =  71.8 %)

#K=5
kmeans_iyer(train3,5)
#K-means clustering with 5 clusters of sizes 1494, 1267, 920, 546, 1459

#Within cluster sum of squares by cluster:
#  [1] 982.39573 463.55884 494.30731  49.13407 271.11248
#(between_SS / total_SS =  86.3 %)

kmeans_iyer(test3,5)
#Within cluster sum of squares by cluster:
#  [1]  19.96541 883.05502  79.31403  96.84709  19.46909
#(between_SS / total_SS =  86.4 %)


#K=7
kmeans_iyer(train3,7)
#K-means clustering with 7 clusters of sizes 625, 1056, 1813, 550, 620, 618, 404

#Within cluster sum of squares by cluster:
#  [1] 115.5974 184.9974 736.2414 539.3484 383.2925 178.7366 168.3513
#(between_SS / total_SS =  91.9 %)

kmeans_iyer(test3,7)
#Within cluster sum of squares by cluster:
#  [1]  19.96541  86.43939 145.47685  96.84709  11.95376 193.64401 130.62864
#(between_SS / total_SS =  93.8 %)

#7 clusters does the best. Try 7 clusters with all 3 PCA sets??:



####################################
#HIERARCHICAL                     ##
####################################
#hierarchical starts with all variables and forms clusters based on cutoff criteria 
#and distance measures given

#use training and test 70-30 split, normalized using 'range'

train3 <- as.matrix(train3)
test3<- as.matrix(test3)

hc.complete=hclust(dist(train3), method="complete")
hc.average=hclust(dist(train3), method="average")
hc.single=hclust(dist(train3), method="single")


#this is a little hard to make sense of - trying another method below this
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
dev.off()

#Ward Hierarchical Clustering
#Ward's minimum variance criterion minimizes the total within-cluster variance. 
#To implement this method, at each step find the pair of clusters that leads to minimum increase 
#in total within-cluster variance after merging. This increase is a weighted squared distance between 
#cluster centers. At the initial step, all clusters are singletons (clusters containing a single point). 
#To apply a recursive algorithm under this objective function, 
#the initial distance between individual objects must be (proportional to) squared Euclidean distance.

d <- dist(train_processed, method = "euclidean") # distance matrix
fit_ward <- hclust(d, method="ward.D") 
plot(fit_ward) # display dendogram
groups <- cutree(fit_ward, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit_ward, k=5, border="red")


######################################################
# K-NN nearest neigbors to edible and poisonous     ##
######################################################
# KNN uses the Euclidian distance measure in order to find the k-nearest neighbours to your new, 
# unknown instance. Here, the k parameter is one that you set yourself. 
# As mentioned before, new instances are classified by looking at the majority vote or weighted vote. 
# In case of classification, the data point with the highest score wins the battle and the 
# unknown instance receives the label of that winning data point. 
# If there is an equal amount of winners, the classification happens randomly.


##reference: https://www.datacamp.com/community/tutorials/machine-learning-in-r
library(class)
library(gmodels)
str(numeric)
#use numeric dataframe. predicting class, this is the only one i do not change into a factor.
table(numeric$class)
#classes are not biased

#training and test for knn
ind <- sample(2, nrow(numeric), replace=TRUE, prob=c(.67,.33))
knn.train <- numeric[ind==1,2:23]
knn.test <- numeric[ind==2,2:23]
#train and test labels for target var
knn.trainLabels <- numeric[ind==1,1]
knn.testLabels <- numeric[ind==2,1]

#k nearest on training using 10 as k
knn_pred <- knn(train = knn.train, test = knn.test, cl = knn.trainLabels, k=10)
knn_pred

#cross table
CrossTable(x = knn.testLabels, y = knn_pred, prop.chisq=FALSE)
#true negative/true positive = model score
#1292/1311 = .9855





