#project 3

library(tm)
library(RColorBrewer)
library(wordcloud)
library(cluster)
library(quanteda)
library(koRpus)
library(openNLP)
library(openNLPdata)
library(qdap)
library(stringr)


myStopwords <- c(stopwords('english'))

setwd("~/Desktop/Spring2016/6907BigDataAnalysis/Project3")

#Data Set: acq

docs <- system.file("texts","acq", package = "tm")

SAT <- VCorpus(DirSource(docs, encoding = "UTF-8", ignore.case=TRUE, mode="text"),
                readerControl = list(language = "lat"))
writeCorpus(SAT)
summary(SAT, showmeta = TRUE)
data(acq)
SAT <- acq
inspect(acq)

writeCorpus(acq)
##########
#You can start by following the slides in Lecture 7.
#You should do at least the following:
#For the complete set of documents, try the functions in lecture 7. 
#What happens? Does it yield anything understandable about the documents. [answered below]

#this tell us what information (metadata) about our documents.  For example, how many chars are within each doc. 
inspect(SAT) #all 50 docs
inspect(SAT[1:2]) #just the first 2

meta(SAT[[2]], "id") #this is another way to reference 

test1 <- SAT[[1]] #a way to look at one document 
test1

#This function tells us more information about the texts (all 50)
#For example, the maximal term length, non/sparse entries 
SATdtm <- DocumentTermMatrix(SAT)
SATdtm

inspect(SATdtm[1:10,1:7])

#termFreq tells us more about an individual doc/text such as term freq within the doc
test1tf <- termFreq(test1)
test1tf
test1df <- as.data.frame(sort(test1tf))
test1df

#the tm_map and content_transformer transforms the data 
#such as converting the terms to lower case
SATlow <- tm_map(SAT, content_transformer(tolower))


#the next function removes anything other than English letters or spaces 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
SATcl <- tm_map(SATlow, content_transformer(removeNumPunct))
SATcl

#after converting the text to lower case, and removing punctionation 
#we are going to remove stopwords (filler words such as a, an, the, etc.)
myStopwords <- c(stopwords('english'))

SATstop <- tm_map(SATcl, removeWords, myStopwords)

#here we can look at the first two text docs and see how the word count (char) differs
inspect(SAT)#the original; 1 with 1770 chars, 2nd with 1272 chars
inspect(SATstop[1:2]) #the amount of words is much less; first with 1362 chars, second with 950 chars
names(SAT)
#now we are putting the terms without punctuation and stopwords into a matrix
SATtdm2 <- TermDocumentMatrix(SATstop, control = list(wordLengths = c(1,Inf)))
SATtdm2
inspect(SATtdm2)
#here we are creating a term frequency list; at first this is a list of all of the terms 
freq.terms <- findFreqTerms(SATtdm2)
freq.terms#there are 1919 terms! 

#now we are looking at terms with freq of 5 or more
freq.terms <- findFreqTerms(SATtdm2, lowfreq = 5)
freq.terms #251 terms 

#the Assocs function finds associations with terms, such as states or year
findAssocs(SATtdm2, "states", 0.25)
findAssocs(SATtdm2, "year", 0.25)

#Next we're going to put the terms with frequency count of 5 or more into a dataframe, t
term.freq <- rowSums(as.matrix(SATtdm2))
term.freq <- subset(term.freq, term.freq >= 5)
df <- data.frame(term = names(term.freq),freq = term.freq)
sort(term.freq)
df #this df will have the count of terms, all terms have at least a count of 5


##########################################################################
##What happens? Does it yield anything understandable about the documents#
##########################################################################

#yes, the different functions allows us to break down the different text documents 
#we were able to see how many stopwords and punctuation was included in the total character count of the texts
#the term frequencies allowed us insight into the top frequented words in the text
#the functions provided a lot of insight into the general documents, text, and words used in the texts

##########################################################################
#######   Find the 10 longest documents (in number of words).      #######
##########################################################################

#we're going to start with the document after punctuation has been removed
inspect(SATcl)
dtm <- DocumentTermMatrix(SATcl)
#remove sparse terms, leaving a matrix that is 10% empty space, max
dtms <- removeSparseTerms(dtm, 0.1)
inspect(dtms)

top10 <- sort(rowSums(as.matrix(dtms)), decreasing = TRUE)[1:10] #use dtm for with sparsity
top10 <- as.data.frame(top10)
top10

#results without sparsity: 7, 20, 30, 53, 26, 36, 4, 1, 23, 48
#meta(SAT[[42]], "id") #<-- to check file names 

###BACK UP >>>#however, if we want to keep sparsity, #results with sparsity: documents 7, 26,30, 53, 20, 4, 23, 36, 48, 1
###BACK UP >>>documents 7, 26, 4, 30, 48, 53, 1, 20, 21, 23 without stopwords and without sparsity

##########################################################################
######## For each document (of top 10) work through the examples        ## 
######## given in Lecture 7 to display the dendrogram and the WordCloud. #                  ##
##########################################################################

############# Doc 7 ################
inspect(SATcl) #without punctuation 
doc7 <- SATcl[7] #doc7 has 73 words 
inspect(doc7)
SATcl[[7]][[1]]
doc7 <- tm_map(doc7, removeWords, myStopwords) #remove stopwords 
doc7m <- DocumentTermMatrix(doc7) 
doc7m <- removeSparseTerms(doc7m, 0.25)
inspect(doc7m)
doc7m

###word cloud - doc 7 ###
freq7 <- colSums(as.matrix(doc7m))
freq7
wordcloud(names(freq7), freq7, min.freq=5) #wordcloud with min freq 5 otherwise get error of a word unable to get on page


###clustering - doc 7 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d7 <- dist(t(doc7m), method = "euclidian")
fit7 <- hclust(d=d7, method = "ward.D")
fit7
plot(fit7, hang=-1)
groups7 <- cutree(fit7, k=6)
rect.hclust(fit7,k=6, border='red')

############# Doc 20 ################
inspect(SATcl) #without punctuation 
doc20 <- SATcl[19]
doc20 <- tm_map(doc20, removeWords, myStopwords) #remove stopwords 
doc20m <- DocumentTermMatrix(doc20) 
doc20m <- removeSparseTerms(doc20m, 0.25)
inspect(doc20m)
doc20m

###word cloud - doc 20 ###
freq20 <- colSums(as.matrix(doc20m))
freq20
wordcloud(names(freq20), freq20, min.freq=3) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 20 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d20 <- dist(t(doc20m), method = "euclidian")
fit20 <- hclust(d=d20, method = "ward.D")
fit20
plot(fit20, hang=-1)
groups20 <- cutree(fit20, k=4)
rect.hclust(fit20,k=4, border='red')

############# Doc 30 ################
inspect(SATcl) #without punctuation 
doc30 <- SATcl[29] 
doc30 <- tm_map(doc30, removeWords, myStopwords) #remove stopwords 
doc30m <- DocumentTermMatrix(doc30) 
doc30m <- removeSparseTerms(doc30m, 0.25)
inspect(doc30m)
doc30m

###word cloud - doc 30 ###
freq30 <- colSums(as.matrix(doc30m))
freq30
wordcloud(names(freq30), freq30, min.freq=2) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 30 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d30 <- dist(t(doc30m), method = "euclidian")
fit30 <- hclust(d=d30, method = "ward.D")
fit30
plot(fit30, hang=-1)
groups30 <- cutree(fit30, k=3)
rect.hclust(fit30,k=3, border='red')

############# Doc 53 ################
inspect(SATcl) #without punctuation 
c
doc53 <- tm_map(doc53, removeWords, myStopwords) #remove stopwords 
doc53m <- DocumentTermMatrix(doc53) 
doc53m <- removeSparseTerms(doc53m, 0.25)
inspect(doc53m)
doc53m

###word cloud - doc 53 ###
freq53 <- colSums(as.matrix(doc53m))
freq53
wordcloud(names(freq53), freq53, min.freq=3) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 53 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d53 <- dist(t(doc53m), method = "euclidian")
fit53 <- hclust(d=d53, method = "ward.D")
fit53
plot(fit53, hang=-1)
groups53 <- cutree(fit53, k=6)
rect.hclust(fit53,k=6, border='red')

############# Doc 26 ################
inspect(SATcl) #without punctuation 
doc26 <- SATcl[25] 
doc26 <- tm_map(doc26, removeWords, myStopwords) #remove stopwords 
doc26m <- DocumentTermMatrix(doc26) 
doc26m <- removeSparseTerms(doc26m, 0.25)
inspect(doc26m)
doc26m

###word cloud - doc 26 ###
freq26 <- colSums(as.matrix(doc26m))
freq26
wordcloud(names(freq26), freq26, min.freq=5) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 26 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d26 <- dist(t(doc26m), method = "euclidian")
fit26 <- hclust(d=d26, method = "ward.D")
fit26
plot(fit26, hang=-1)
groups26 <- cutree(fit26, k=4)
rect.hclust(fit26,k=4, border='red')

############# Doc 36 ################
inspect(SATcl) #without punctuation 
doc36 <- SATcl[34] 
doc36 <- tm_map(doc36, removeWords, myStopwords) #remove stopwords 
doc36m <- DocumentTermMatrix(doc36) 
doc36m <- removeSparseTerms(doc36m, 0.25)
inspect(doc36m)
doc36m

###word cloud - doc 26 ###
freq36 <- colSums(as.matrix(doc36m))
freq36
wordcloud(names(freq36), freq36, min.freq=3) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 36 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d36 <- dist(t(doc36m), method = "euclidian")
fit36 <- hclust(d=d36, method = "ward.D")
fit36
plot(fit36, hang=-1)
groups36 <- cutree(fit36, k=3)
rect.hclust(fit36,k=3, border='red')

############# Doc 4 ################
inspect(SATcl) #without punctuation 
doc4 <- SATcl[4]  
doc4 <- tm_map(doc4, removeWords, myStopwords) #remove stopwords 
doc4m <- DocumentTermMatrix(doc4) 
doc4m <- removeSparseTerms(doc4m, 0.25)
inspect(doc4m)
doc4m

###word cloud - doc 4 ###
freq4 <- colSums(as.matrix(doc4m))
freq4
wordcloud(names(freq4), freq4, min.freq=4) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 4 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d4 <- dist(t(doc4m), method = "euclidian")
fit4 <- hclust(d=d4, method = "ward.D")
fit4
plot(fit4, hang=-1)
groups4 <- cutree(fit4, k=4)
rect.hclust(fit4,k=4, border='red')

############# Doc 1 ################
inspect(SATcl) #without punctuation 
doc1 <- SATcl[1]  
doc1 <- tm_map(doc1, removeWords, myStopwords) #remove stopwords 
doc1m <- DocumentTermMatrix(doc1) 
doc1m <- removeSparseTerms(doc1m, 0.25)
inspect(doc1m)
doc1m

###word cloud - doc 1 ###
freq1 <- colSums(as.matrix(doc1m))
freq1
wordcloud(names(freq1), freq1, min.freq=3) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 1 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d1 <- dist(t(doc1m), method = "euclidian")
fit1 <- hclust(d=d1, method = "ward.D")
fit1
plot(fit1, hang=-1)
groups1 <- cutree(fit1, k=4)
rect.hclust(fit1,k=4, border='red')

############# Doc 23 ################
inspect(SATcl) #without punctuation 
doc23 <- SATcl[22]  
doc23 <- tm_map(doc23, removeWords, myStopwords) #remove stopwords 
doc23m <- DocumentTermMatrix(doc23) 
doc23m <- removeSparseTerms(doc23m, 0.25)
inspect(doc23m)
doc23m

###word cloud - doc 23 ###
freq23 <- colSums(as.matrix(doc23m))
freq23
wordcloud(names(freq23), freq23, min.freq=3) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 23 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d23 <- dist(t(doc23m), method = "euclidian")
fit23 <- hclust(d=d23, method = "ward.D")
fit23
plot(fit23, hang=-1)
groups23 <- cutree(fit23, k=4)
rect.hclust(fit23,k=4, border='red')

############# Doc 48 ################
inspect(SATcl) #without punctuation 
doc48 <- SATcl[42]  
doc48 <- tm_map(doc48, removeWords, myStopwords) #remove stopwords 
doc48m <- DocumentTermMatrix(doc48) 
doc48m <- removeSparseTerms(doc48m, 0.25)
inspect(doc48m)
doc48m

###word cloud - doc 48 ###
freq48 <- colSums(as.matrix(doc48m))
freq48
wordcloud(names(freq48), freq48, min.freq=4) #wordcloud with min freq 3 otherwise get error of a word unable to get on page


###clustering - doc 48 ### 

#normalize 
#norm_ecul <- function(doc7m) doc7m/apply(doc7m, MARGIN=1, FUN = function(x) sum(x^2)^.5)
#m_norm <- norm_ecul(doc7m)
#d7 <- dist(t(m_norm), method = "euclidian")
d48 <- dist(t(doc48m), method = "euclidian")
fit48 <- hclust(d=d48, method = "ward.D")
fit48
plot(fit48, hang=-1)
groups48 <- cutree(fit48, k=4)
rect.hclust(fit48,k=4, border='red')

#####################################################################################
#For the following you will need to write R functions to help you compute the results.
#Use the packages textreuse, wordnet, zipfR
library(textreuse)
library(wordnet)
library(zipfR)
#####################################################################################
#Prior to removing the punctuation, find the longest word and longest sentence
#in each document from the 10 largest documents.

############# Doc 7 ################
doc7b <- SAT[7]
sentences7 <- sent_detect_nlp(doc7b)
count7 <- nchar(sent_detect_nlp(doc7b))
count7 <- sort(count7) # sentence 2 had largest with 610 char
count7 <- as.table(count7) #table 
count7
sentences7[2] #max for doc 7 
nchar(sentences7[2]) #610 character 

#Find largest word 

word7b <- scan_tokenizer(doc7b)
max(nchar(word7b)) #27
subset(word7b, nchar(word7b)==27) #"BC-AMERICAN-EXPRESS-&lt;AXP"

############# Doc 20 ################
doc20b <- SAT[19]
sentences20 <- sent_detect_nlp(doc20b)
count20 <- nchar(sent_detect_nlp(doc20b))
count20 <- sort(count20) # sentence 2 had largest with 610 char
count20 <- as.table(count20) #table 
count20
sentences20[2] #max for doc 7 
nchar(sentences7[2]) #610 character 

#Find largest word 

word20b <- scan_tokenizer(doc20b)
max(nchar(word20b)) #27
subset(word20b, nchar(word20b)==25) #CGISPLIT=\\\"TRAINING-SET\\\""

############# Doc 30 ################
doc30b <- SAT[29] 
sentences30 <- sent_detect_nlp(doc30b)
count30 <- nchar(sent_detect_nlp(doc30b))
count30 <- sort(count30) # sentence 2 had largest with 610 char
count30 <- as.table(count30) #table 
count30 #491
sentences30[2] #max for doc 7 
nchar(sentences30[2]) #610 character 

#Find largest word 

word30b <- scan_tokenizer(doc30b)
max(nchar(word30b)) #25
subset(word30b, nchar(word30b)==25) #CGISPLIT=\\\"TRAINING-SET\\\""

############# Doc 53 ################
doc53b <- SAT[47] 
sentences53 <- sent_detect_nlp(doc53b)
count53 <- nchar(sent_detect_nlp(doc53b))
count53 <- sort(count53) # sentence 2 had largest with 610 char
count53 <- as.table(count53) #table 
count53 #511
sentences53[2] #max for doc 7 
nchar(sentences53[2]) #610 character 

#Find largest word 

word53b <- scan_tokenizer(doc53b)
max(nchar(word53b)) #25
subset(word53b, nchar(word53b)==25) #CGISPLIT=\\\"TRAINING-SET\\\""

############# Doc 26 ################
doc26b <- SAT[25] 
sentences26 <- sent_detect_nlp(doc26b)
count26 <- nchar(sent_detect_nlp(doc26b))
count26 <- sort(count26) # sentence 2 had largest with 610 char
count26 <- as.table(count26) #table 
count26 #619
sentences26[2] #max for doc 7 
nchar(sentences26[2]) #610 character 

#Find largest word 

word26b <- scan_tokenizer(doc26b)
max(nchar(word26b)) #25
subset(word26b, nchar(word26b)==25) #CGISPLIT=\\\"TRAINING-SET\\\""

############# Doc 36 ################
doc36b <- SAT[34]
sentences36 <- sent_detect_nlp(doc36b)
count36 <- nchar(sent_detect_nlp(doc36b))
count36 <- sort(count36) 
count36 <- as.table(count36) #table 
count36 #453
sentences36[2] #max for doc 7 
nchar(sentences36[2]) #610 character 

#Find largest word 

word36b <- scan_tokenizer(doc36b)
max(nchar(word36b)) #25
subset(word36b, nchar(word36b)==30) #"BC-/VIACOM-&lt;VIA&gt;-RECEIVE"

############# Doc 4 ################
doc4b <- SAT[4] 
sentences4 <- sent_detect_nlp(doc4b)
count4 <- nchar(sent_detect_nlp(doc4b))
count4 <- sort(count4) 
count4 <- as.table(count4) #table 
count4 #529
sentences4[2] #max for doc 7 
nchar(sentences4[2]) #610 character 

#Find largest word 

word4b <- scan_tokenizer(doc4b)
max(nchar(word4b)) #25
subset(word4b, nchar(word4b)==30) #"BC-CHEMLAWN-&lt;CHEM&gt;-RISES"

############# Doc 1 ################
doc1b <- SAT[1] 
sentences1 <- sent_detect_nlp(doc1b)
count1 <- nchar(sent_detect_nlp(doc1b))
count1 <- sort(count1) 
count1 <- as.table(count1) #table 
count1 #543
sentences1[2] #max for doc 7 
nchar(sentences1[2]) #610 character 

#Find largest word 

word1b <- scan_tokenizer(doc1b)
max(nchar(word1b)) #25
subset(word1b, nchar(word1b)==25) #"CGISPLIT=\\\"TRAINING-SET\\\""

############# Doc 23 ################
doc23b <- SAT[22]
sentences23 <- sent_detect_nlp(doc23b)
count23 <- nchar(sent_detect_nlp(doc23b))
count23 <- sort(count23) 
count23 <- as.table(count23) #table 
count23 #536
sentences23[2] #max for doc 7 
nchar(sentences23[2]) #610 character 

#Find largest word 

word23b <- scan_tokenizer(doc23b)
max(nchar(word23b)) #25
subset(word23b, nchar(word23b)==25) #"CGISPLIT=\\\"TRAINING-SET\\\""

############# Doc 48 ################

doc48b <- SAT[42]
sentences48 <- sent_detect_nlp(doc48b)
count48 <- nchar(sent_detect_nlp(doc48b))
count48 <- sort(count48) 
count48 <- as.table(count48) #table 
count48 #588
sentences48[2] #max for doc 7 
nchar(sentences48[2]) #610 character 

#Find largest word 

word48b <- scan_tokenizer(doc48b)
max(nchar(word48b)) #25
subset(word48b, nchar(word48b)==25) #"CGISPLIT=\\\"TRAINING-SET\\\""

###########################################################################
#Print a table of the length of each sentence in each of the 10 documents.#
###########################################################################

count7 #doc 7 
count20 #doc 20
count30 #doc 30
count53 #doc 53
count26 #doc 26
count36  #doc 36
count4  #doc 4
count1  #doc 1
count23  #doc 23
count48   #doc 48


#############################################################
#For each sentence of each document, remove the punctuation.#
#############################################################

sentences7
sentences7b <- removePunctuation(sentences7)
sentences20b <- removePunctuation(sentences20)
sentences30b <- removePunctuation(sentences30)
sentences53b <- removePunctuation(sentences53)
sentences26b <- removePunctuation(sentences26)
sentences36b <- removePunctuation(sentences36)
sentences4b <- removePunctuation(sentences4)
sentences1b <- removePunctuation(sentences1)
sentences23b <- removePunctuation(sentences23)
sentences48b <- removePunctuation(sentences48)



doc20c <- SATcl[19]
doc30c <- SATcl[29] 
doc53c <- SATcl[47] 
doc26c <- SATcl[25] 
doc36c <- SATcl[34] 
doc4c <- SATcl[4]  
doc1c <- SATcl[1]  
doc23c <- SATcl[22]
doc48c <- SATcl[42] 


#For each word print its part of speech using the wordnet packages.




#Analyze word frequency using functions from package zipfR.









#doc7b <- SAT[7]
#doc20b <- SAT[19]
#doc30b <- SAT[29] 
#doc53b <- SAT[47] 
#doc26b <- SAT[25] 
#doc36b <- SAT[34] 
#doc4b <- SAT[4]  
#doc1b <- SAT[1]  
#doc23b <- SAT[22]
#doc48b <- SAT[42] 





