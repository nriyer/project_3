library(tm)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(quanteda)
getwd()

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
tm::
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
tdm2 <- removeSparseTerms(ACQdoc, sparse = .5)
tdm2

#distance matrix for clustering
distMatrix <- dist(scale(tdm2))
fit <- hclust(distMatrix, method="ward.D2")
plot(fit)

#document term matrix
dtm <- DocumentTermMatrix(ACQstop)
dtm
names(dtm)
nrow(dtm)
wordsperdoc <- rowSums(as.matrix(dtm))
wordsperdoc

mycorpus <- corpus(acq)
summary_acq <- as.data.frame(summary(mycorpus))
sort_top10 <- summary_acq %>% arrange(desc(Tokens))
