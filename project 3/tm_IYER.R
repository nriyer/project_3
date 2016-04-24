library(tm)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(quanteda)
library(wordnet)
library(RColorBrewer)


data("acq")
head(acq)

#compilation of 50 news articles with additional meta information form the 
#Reuters-21578 data se of topic acq. 13 documents
ACQ <- acq
ACQ

#extract one document
text1 <- ACQ[[1]]
text1
#document term matrix
ACQdoc <-DocumentTermMatrix(ACQ)
ACQdoc
nrow(ACQdoc) #50 rows
ncol(ACQdoc) #2103 cols
inspect(ACQdoc[1:6,1:10])

#document term frequency
test1tf <- as.data.frame(termFreq(text1))
#rank words most to least
rank_of_words <- cbind(as.data.frame(rownames(test1tf)),test1tf %>% arrange(desc(termFreq(text1))))

#convert corpus to lowercase
#LAZY = TRUE so that only docs being addressed are transformed, since this is a large body
ACQlow <- tm_map(ACQ, content_transformer(tolower))
ACQlow

#remove anything other than English letters or spaces
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
ACQcl <- tm_map(ACQlow,content_transformer(removeNumPunct))
ACQcl

#remove stopwords from corpus
stopwords <- c(stopwords('english'))
ACQstop <- tm_map(ACQcl, removeWords, stopwords)

inspect(ACQcl[1:2])

#document matrix 2 - inspect once all stopwords, punctuation, numbers taken out
ACQdm2 <- TermDocumentMatrix(ACQstop, control= list(wordLenghts = c(1,Inf)))
ACQdm2


#find terms with a frequency of 5 or more
freq.terms <- findFreqTerms(ACQdm2, lowfreq=5)
freq.terms
#find terms associated with "states"- measure of the co-occurrence of words in multiple documents.
#words that co-occur with the given word
#ex below, all words that co occur with states in 50% of the docs
findAssocs(ACQdm2, "states", .5)

term.freq <- rowSums(as.matrix(ACQdm2))
term.freq <- subset(term.freq, term.freq <= 5)
termdf <- data.frame(term = names(term.freq),freq=term.freq)
term_sort <- termdf %>% arrange(desc(freq))
term_sort[1:50,]
#plot top 50 frequent terms
ggplot(term_sort[1:50,], aes(x=term,y=freq)) + geom_bar(stat = "identity") + 
  xlab("Terms") + ylab("Count") + coord_flip()

#remove sparse terms
tdm2 <- removeSparseTerms(ACQdm2, sparse = .8)
tdm2


#distance matrix for clustering
distMatrix <- dist(scale(tdm2))
fit <- hclust(distMatrix, method="ward.D2")
plot(fit)


#using quanteda for the next few questions
mycorpus <- corpus(acq)
mycorpus_dfm <- dfm(mycorpus,ignoredFeatures = stopwords)
mycorpus_dfm <- removeSparseTerms(mycorpus_dfm, sparse = .5)
docnames(mycorpus_dfm)


summary_acq <- as.data.frame(summary(mycorpus))
sort_top10 <- summary_acq %>% arrange(desc(Tokens))



#10 longest documents in the corpus
top_10_docs <- subset(sort_top10, select=c(id, heading))[1:10,]

#dendogram for top 10
#list of top10 docs
top10 <- top_10_docs[,1]

#dendogram for top 10
acq.mat <- as.matrix(tdm2)
acq.mat <- as.data.frame(acq.mat)
acq.mat <- acq.mat[,top10]
acq.mat <- as.matrix(acq.mat)
distMatrix <- dist(scale(acq.mat))
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit,main = "Top 10 Docs Dendogram")

#word cloud for top 10
dtm <- TermDocumentMatrix(ACQstop)
m <- as.data.frame(as.matrix(dtm))
m <- m[,top10]
m <- as.matrix(m)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))