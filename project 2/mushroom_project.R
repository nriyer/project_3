library(MASS)
library(d)
#set wd
setwd("~/Desktop/GW_spring2016/bigdata")
mushroom <- read.csv('agaricus-lepiota.csv',header = TRUE)
#pre-processing and data exploration
str(mushroom)
#ok all are factors - like they should be
#dependent variable in this case is 'Target' lets look at it
summary(mushroom$Target)
names(mushroom)
#distribution across variables:
ft <- rvgt.ftable(mushroom$Target,mushroom$cap_shape)
plot(Target ~ stalk_root, data=mushroom)
plot(Target ~ cap_shape, data = mushroom)

###do it like he does:
mr <- read.table('agaricus-lepiota.data',sep=",",header=FALSE)
str(mr)
#set names
names(mr) <- c("class","cshape","csurface","ccolor","bruises","odor","gattach","gspace",
"gsize","gcolor","sshape","sroot","ssabove","ssbelow","scabove","scbelow","vtype","vcolor",
"rnumber","rtype","spcolor","popnum","habitat")
names(mr)
#data exploration
#plot all variables vs. class and ftable for each against class
for (i in names(mr)){
  plot(class ~ mr[,i], data=mr,xlab = i)
  print(ftable(mr$class,mr[,i]))
}
#look at pairwise correlations (look through all)
pairs(class ~ ccolor, data = mr)

##-------get ready for clustering:

#take out target var and turn rest into numeric for clustering
numeric <- mushroom[,-1]
str(numeric)
#this translates column from factor to number
numeric$cap_shape <- as.numeric(numeric$cap_shape)
#loop to translate all to factors
for (i in names(numeric)){
  numeric[,i] <- as.numeric(numeric[,i])
}
#split into various size train and test sets:

