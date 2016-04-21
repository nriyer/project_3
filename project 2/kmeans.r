myfunction <- function(arg1, arg2, ... ){
statements
return(object)
}

kmeans_iyer <- function(df,k){
	df <- as.matrix(df)
	fit <-kmeans(df,k)
	#get cluster means:this illustrates amount of each characteristic in each cluster
	aggregate(df,by=list(fit$cluster), FUN=mean)
	#append cluster assignment
	cluster_assignment <- data.frame(df, fit$cluster)
	#table of how many variables fit in each cluster
	return(table(fit$cluster))
	km.out<- kmeans(cluster_assignment,k,nstart=n)
	print(km.out$tot.withinss)
	print(km.out$betweenss)
	print(km.out)
}

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
	print(plot(1:12, wss, type="b", xlab="Number of Clusters",
	     ylab="Between groups sum of squares"))
}

###########################
##centered and scaled for normal distribution
pre_center_scale <- preProcess(train,method=c('center','scale'))
post_center_scale <- preProcess(test,method=c('center','scale'))
train_processed_cs <- predict(pre_center_scale, train)
test_processed_cs <- predict(post_center_scale,test)
############################

scale <- function(df){
  pre_range <- preProcess(df,method="range")
  processed <- predict(pre_range,df)
  return(data.frame(processed))
}
str(train_50)
pre_range_50 <- preProcess(train_50,method="range")
train_processed_50 <- predict(pre_range_50, train_50)
str(test_50)
post_range_50 <- preProcess(test_50,method="range")
test_processed_50 <- predict(post_range_50, train_50)
test_processed_50 <- as.data.frame(test_processed_50)


#(incase function is failing)
fit <-kmeans(train_processed,5)
#get cluster means:this illustrates amount of each characteristic in each cluster
aggregate(train_processed,by=list(fit$cluster), FUN=mean)
#append cluster assignment
cluster_assignment <- data.frame(train_processed, fit$cluster)
#look at some of the characteristics within clusters
check <- cluster_assignment[which(cluster_assignment$cshape == .4),c(1,2,22)]
#table of how many variables fit in each cluster
table(fit$cluster)


#TRAIN MODEL using TRAINING SCALED BY 'RANGE' and 70-30 train/test set
train_processed <- as.matrix(train_processed)
#function to find optimal number of clusters (takes too long)
km.numcluster <- NbClust(train_processed, method = 'kmeans', min.nc=2,max.nc=12)



#plot 5 clusters for kmeans
set.seed(12345)
#nstart option that attempts multiple initial configurations and reports on the best one
km.out.train <- kmeans(cluster_assignment,5,nstart=20)
plot(cluster_assignment, col=(km.out.train$cluster), main="K-means Clustering Results with K=5")
#find w/in b/w ss for 3 clusters
km.out.train$tot.withinss
#5373.021
km.out$betweenss
#14708.03
#Within cluster sum of squares by cluster:
#[1]  340.7459 1122.6772  274.5883 1334.7825 2300.2273
#(between_SS / total_SS =  73.2 %)
#fit is at .732, ok lets test this against test set