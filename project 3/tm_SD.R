library(tm)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(quanteda)
library(wordnet)
library(RColorBrewer)
library(NLP)
library(openNLP)
library(textreuse)
data("acq")
head(acq)

acq <- tm_map(acq, content_transformer(tolower))
acqdm <-TermDocumentMatrix(acq)

# get data about corpus
cor <- summary(corpus(acq))
cor <- as.data.frame(cor)

# Find the 10 longest documents (in number of words).
top10id <- (cor %>% arrange(desc(Tokens)))[1:10,]$id
top10rowid <- which(corpus(acq)$documents$id %in% top10id)


# For each document work through the examples given in Lecture 7 to display the dendrogram and the WordCloud.
dtm <- DocumentTermMatrix(acq)
for (i in 1:10) {
  ind <- top10rowid[i]
  words <- colnames(as.matrix(dtm[ind,])) 
  f <- as.matrix(dtm[ind,])
}

library(tm)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(quanteda)
library(wordnet)
library(RColorBrewer)
library(NLP)
library(openNLP)
library(textreuse)
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
summary_acq <- as.data.frame(summary(mycorpus))
#find top 10 longest docs

#dendogram for top 10
#list of top10 docs
#10 longest documents in the corpus
sort_top10 <- summary_acq %>% arrange(desc(Tokens))
top_10_docs <- subset(sort_top10, select=c(id, heading))[1:10,]
top10 <- top_10_docs[,1]


topdocs <- mycorpus[mycorpus$documents$id %in% top10]

#sumarry of whole doc and top docs as data frame
summary_td <- as.data.frame(summary(topdocs))


#dendogram for top 10

############## NOTE ##############
## needs to be for each doc, not for all ten at once
############## NOTE ##############

acq.mat <- as.matrix(tdm2)
acq.mat <- as.data.frame(acq.mat)
acq.mat <- acq.mat[,top10]
acq.mat <- as.matrix(acq.mat)
distMatrix <- dist(scale(acq.mat))
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit,main = "Top 10 Docs Dendogram")

#word cloud for top 10
tdm <- TermDocumentMatrix(ACQstop)
m <- as.data.frame(as.matrix(tdm))
for (i in 1:10) {
  f <- m[,top10[i]]
  d <- data.frame(word = rownames(m),freq=f)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}

#Prior to removing punctuation find the longest word and longest sentence in each of 10 docs
#my corpus is before removing punctuation
top10_docs <- sort_top10[1:10,]
#tokenize the top 10 docs then look for the longest word
tokened <- tokenize(topdocs,what = c("word"))
names(tokened)


#testing
a <- tokened[[2]]
b <- a[nchar(a) == max(nchar(a))]

#####FIND LONGEST WORD in 10 docs
max_length <- c()
word <- c()
for (i in 1:10){
  words <- tokened[[i]]
  word[i] <- words[nchar(words) == max(nchar(words))]
  max_length[i] <- max(nchar(words))
}

final.longest_word <- data.frame(max_length = max_length,word=word)

#####FIND LONGEST SENTENCE in 10 docs
topdocs
topdocs[[2]]
names(topdocs[1])

#split into sentences
get_sentence_df_func <- function(x){
  sentence_df <- data.frame(sentence = character(0),
                            document = character(0))
  for (i in 1:10){
    temp <- data.frame(sentence=tokenize_sentences(x[[i]][[1]]),id=names(x[i]))
    sentence_df <- rbind(sentence_df,temp)
  }
  return(sentence_df)
}
#data frame of sentences by document 
text_sent <- get_sentence_df_func(topdocs)

#word count for each sentence
text_sent$sentence <- as.character(text_sent$sentence)
count <- c()
sapply(strsplit(text_sent$sentence[23], " "), length)
for (i in 1:nrow(text_sent)){
  count[i] <- sapply(strsplit(text_sent$sentence[i], " "), length)
}
#length of each sentence in each document
count_sentences <- cbind(count,text_sent)
#top 10 lengths 
longest_10 <- count_sentences %>% group_by(id) %>% 
  arrange(desc(count)) %>% top_n(1,count) %>% distinct(id)

#remove punctuation for each sentence




#################################################################################
#for zipfr
library(languageR)
document.spc <- text2spc.fnc(strsplit(x[[1]][1]$content," "))
document.vgc <- growth.fnc(strsplit(acq10[[1]][1]$content," "))
text <- strsplit(document$content, " ")[[1]]

##create spc object
this.spc <- text2spc.fnc(text)

#create growth object
this_growth<- growth.fnc(text)



