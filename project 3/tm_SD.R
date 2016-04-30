library(tm)
library(zipfR)
library(wordcloud)
library(quanteda)
library(wordnet)
library(NLP)
library(openNLP)
data("acq")

acq <- tm_map(acq, content_transformer(tolower))
acqdm <-TermDocumentMatrix(acq)

# get data about corpus
cor <- summary(corpus(acq))
cor <- as.data.frame(cor)

# Find the 10 longest documents (in number of words).
top10id <- (cor %>% arrange(desc(Tokens)))[1:10,]$id
top10rowid <- which(corpus(acq)$documents$id %in% top10id)


# For each document work through the examples given in Lecture 7 to display the dendrogram and the WordCloud.
dtm <- DocumentTermMatrix(acq) #document term matrix
sw <- c(stopwords('english')) # stop words
for (i in 1:10) {
  ind <- top10rowid[i]
  words <- colnames(as.matrix(dtm[ind,])) # all words
  f <- as.matrix(dtm[ind,]) # frequencies for all words
  f <- f[-(which(words %in% sw))] #without stop words
  words <- words[-(which(words %in% sw))] # without stop words

  wordcloud( words = words, freq = f, min.freq = 1,
             max.words=200, random.order=FALSE, rot.per=0.35, 
             colors=brewer.pal(8, "Dark2"))
}

# find the longest word and longest sentence in each document from the 10 largest documents.
word <- c()
sentence <-c()
id <- c()
for (i in 1:10) {
  ind <- top10[i]
  id[i] <- ind
  s <- as.String(acq[[ind]]$content)
  
  ##longest sentence
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  a1 <- sent_token_annotator(s)
  l <- a1$end - a1$start # table of sentence lengths
  ls.i <- which.max(l) #index of longest sentence
  ls <- s[a1][ls.i] #longest sentence
  
  acq[[ind]]$meta$ls <- ls #add longest sentence to metadata of document
  sentence[i] <- ls
  
  # Print a table of the length of each sentence in each of the 10 documents.
  df <- as.data.frame(a1)
  df$length <- l
  #print(df)
  
  ##longest word
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- word_token_annotator(s, a1)
  a2 <- a2[a2$type=="word"]
  lw.i <- which.max(a2$end - a2$start) #index of longest sentence
  lw <- s[a2][lw.i] #longest sentence
  print( c(ind, lw))
  
  acq[[ind]]$meta$lw <- lw #add longest word to metadata of document
  word[i] <- lw
  
}
data.frame(id = id, word=word, sentence=sentence)



# For each sentence of each document, remove the punctuation.
# For each word print its part of speech using the wordnet packages.
d<-data.frame()
for (i in 1:10) {
  
  ind <- top10rowid[i]
  s <- as.String(acq[[ind]]$content)
  s <- tolower(s)
  
  a1 <- sent_token_annotator(s)
  
  # For each sentence of each document, remove the punctuation.
  s.cl <- sapply( s[a1], removePunctuation )
  
  for( j in 1:length(s.cl)) {

    sub.s <- as.String(s.cl[j])
    
    a2 <- word_token_annotator(sub.s, a1)
    a3 <- pos_tag_annotator( sub.s, a = a2)

    words <- as.vector( sub.s[a3] )
    pos <- as.matrix( lapply(a3$features, function(x) x$POS) )
    
    df <- data.frame(words=words, pos=pos)
    d<-rbind(d,df)
    

  }

  
}

#################################################################################
#for zipfr

for(i in 1:10) {
  ind <- top10rowid[i]
  words <- colnames(as.matrix(dtm[ind,])) # all words
  f <- as.matrix(dtm[ind,]) # frequencies for all words

  z <- tfl(f)
  plot(sort(z$f,decreasing=TRUE), xlab="rank",ylab="frequency", main=acq[[ind]]$meta$id)
}
