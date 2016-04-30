library(tm)
library(dplyr)
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
for (i in 1:10) {
  ind <- top10rowid[i]
  s <- as.String(acq[[ind]]$content)
  
  ##longest sentence
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  a1 <- annotate(s, sent_token_annotator)
  l <- a1$end - a1$start # table of sentence lengths
  ls.i <- which.max(l) #index of longest sentence
  ls <- s[a1][ls.i] #longest sentence
  
  acq[[ind]]$meta$ls <- ls #add longest sentence to metadata of document
  
  # Print a table of the length of each sentence in each of the 10 documents.
  df <- as.data.frame(a1)
  df$length <- l
  print(df)
  
  ##longest word
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(s, word_token_annotator, a1)
  a2 <- a2[a2$type=="word"]
  lw.i <- which.max(a2$end - a2$start) #index of longest sentence
  lw <- s[a2][lw.i] #longest sentence
  
  acq[[ind]]$meta$lw <- lw #add longest word to metadata of document
  
}


## DOES NOT USE WORDNET PACKAGE...


# For each sentence of each document, remove the punctuation.
# For each word print its part of speech using the wordnet packages.
for (i in 1:10) {
  
  ind <- top10rowid[i]
  s <- as.String(acq[[ind]]$content)
  
  ##longest sentence
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  a1 <- annotate(s, sent_token_annotator)
  
  # For each sentence of each document, remove the punctuation.
  s.cl <- sapply( s[a1], removePunctuation )
  
  for( j in 1:length(s.cl)) {
    
    print (as.String(s.cl[j])) # show we stripped punctuation
    
    sub.s <- as.String(s[a1][j])
    
    pos_tag_annotator <- Maxent_POS_Tag_Annotator()
    a3 <- annotate(sub.s,
                   list(sent_token_annotator,
                        word_token_annotator,
                        pos_tag_annotator))

    df <- as.data.frame(a3)
    df$words <- sub.s[a3]
    df$pos <- lapply(df$features, function(x) x$POS)
    print(df[2:dim(df)[1],6:7])
    

    
  }

  
}
