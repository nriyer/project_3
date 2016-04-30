library(tm)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(quanteda)
library(wordnet)
library(RColorBrewer)
library(NLP)
library(openNLP)
library(openNLPdata)
library(textreuse)

data("acq")

#compilation of 50 news articles with additional meta information form the
#Reuters-21578 data se of topic acq. 13 documents
ACQ <- acq

#this tell us what information (metadata) about our documents.
# For example, how many chars are within each doc.
alldocs <- inspect(ACQ[1:2]) #just the first 2

# get the first document
text1 <- ACQ[[1]]

# get the id from the second document
id.2 <- ACQ[[1]]$meta$id
id.2 <- meta(ACQ[[1]], "id") #this is another way to reference

ACQdoc <-DocumentTermMatrix(ACQ)
ACQdoc

inspect(ACQdoc[1:6,1:7])

test1tf <- as.data.frame(termFreq(text1))
#rank words most to least
rank_words <- as.data.frame(test1tf[order(test1tf, decreasing = T),])
head(rank_words)

# to lower case
ACQlow <- tm_map(ACQ, content_transformer(tolower))

#the next function removes anything other than English letters or spaces
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
ACQcl <- tm_map(ACQlow,content_transformer(removeNumPunct))

#after converting the text to lower case, and removing punctionation
#we are going to remove stopwords (filler words such as a, an, the, etc.)
stopwords <- c(stopwords('english'))
ACQstop <- tm_map(ACQcl, removeWords, stopwords)

#here we can look at the first two text docs and see how the word count (char) differs
inspect(ACQ[1])
inspect(ACQstop[1])

#now we are putting the terms without punctuation and stopwords into a matrix
ACQdm2 <- DocumentTermMatrix(ACQstop, control= list(wordLenghts = c(1,Inf)))
ACQdm2

#find terms with a frequency between 15 and 18
freq.terms <- findFreqTerms(ACQdm2, lowfreq=15, highfreq = 18)
freq.terms

#the Assocs function finds associations with terms, such as states or year
findAssocs(ACQdm2, "states", 0.6)


#using quanteda for the next few questions
data("acq")
mycorpus <- corpus(acq)
summary_acq <- as.data.frame(summary(mycorpus))

#10 longest documents in the corpus
sort_top10 <- summary_acq %>% arrange(desc(Tokens))
top_10_docs <- subset(sort_top10, select=c(id, heading))[1:10,]
top10 <- top_10_docs[,1]
topdocs <- mycorpus[mycorpus$documents$id %in% top10]

top10



#top 10 dendogram, 1 for each of the top 10 documents
top10.dendogram <- function(doc)
{
  # full dendogram
  acq.mat <- as.matrix( ACQdm2[doc,] )
  distMatrix <- dist(scale(acq.mat[doc,]))
  fit <- hclust(distMatrix, method = "ward.D2")
  plot(fit,main = paste("Full Dendogram for ID: ", doc), xlab ="", sub = "")

  # dendogram with sparse terms removed
  acq.sp <- removeSparseTerms(ACQdm2, sparse = .8)
  acq.mat <- as.matrix( acq.sp[doc,] )
  distMatrix <- dist(scale(acq.mat[doc,]))
  fit <- hclust(distMatrix, method = "ward.D2")
  plot(fit,main = paste("Dendogram without sparse terms for ID: ", doc), xlab = "", sub = "")
}
for (i in 1:10){
  top10.dendogram(top10[i])
}

# full dendogram
acq.mat <- as.matrix(ACQdm2)
distMatrix <- dist(scale( colSums(acq.mat[top10,]) ))
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit,main = paste("Full Dendogram for Top 10 Documents"), xlab ="", sub = "")

# dendogram with sparse terms removed
acq.sp <- removeSparseTerms(ACQdm2, sparse = .8)
acq.mat <- as.matrix(acq.sp)
distMatrix <- dist(scale( colSums(acq.mat[top10,]) ))
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit,main = paste("Dendogram for Top 10 without Sparse Terms"), xlab ="", sub = "")


options(warn=-1)
#word cloud for top 10
wordcloud.func <- function(ACQstop, doc)
{
  dtm <- TermDocumentMatrix(ACQstop)
  v <- as.matrix(dtm[,doc])
  set.seed(1234)

  layout(matrix(c(1, 2), nrow=2), heights=c(0.5, 4.5))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste("Word Cloud for Document ID: ",doc) )
  wordcloud(words = rownames(v), freq = v, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))
}

for (i in 1:10){
  wordcloud.func(ACQstop,top10[i])
}

dtm <- TermDocumentMatrix(ACQstop)
v <- t( as.matrix(dtm[,top10]) )
v <- colSums(v)
set.seed(1234)

layout(matrix(c(1, 2), nrow=2), heights=c(0.5, 4.5))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Word Cloud for Top Documents" )
wordcloud(words = names(v), freq = v, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

options(warn=0)

# find the longest word and longest sentence in each document from the 10 largest documents
for (i in 1:10) {
  ind <- top10[i]
  s <- as.String(acq[[ind]]$content)

  ##longest sentence by characters
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  a1 <- sent_token_annotator(s)
  l <- a1$end - a1$start # table of sentence lengths
  ls.i <- which.max(l) #index of longest sentence by characters
  ls <- as.String( s[a1][ls.i] ) #longest sentence by characters

  ##longest sentence by constituents
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- word_token_annotator(s, a1)
  a2 <- a2[a2$type=="sentence"]
  l.w <- as.matrix( lapply(a2$features, function(x) length(x$constituents)) ) #sent length
  l.w.ind <- which.max( l.w )
  ls.w <- as.String( s[a1][l.w.ind] )

  ##longest word
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- word_token_annotator(s, a1)
  a2 <- a2[a2$type=="word"]
  lw.i <- which.max(a2$end - a2$start) #index of longest sentence
  lw <- s[a2][lw.i] #longest sentence

  # print everything so that it's pretty
  print( as.String( paste("Document ID: ", ind) ) )
  print( as.String( paste( "\tLongest Word:\t", lw) ) )
  if (ls == ls.w) {
    print( as.String( paste( "\tLongest Sentence:\t", ls) ) )
  } else {
    print( as.String( paste( "\tLongest Sentence by nchar:\t", ls) ) )
    print( as.String( paste( "\tLongest Sentence by words:\t", ls.w) ) )
    print( as.String(""))
  }
  print( as.String(""))

}



d.full <- data.frame()
for (i in 1:10) {
  ind <- top10[i]
  s <- as.String(acq[[ind]]$content)

  ##longest sentence by characters
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  a1 <- sent_token_annotator(s)
  l <- a1$end - a1$start # table of sentence lengths

  ##longest sentence by constituents
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- word_token_annotator(s, a1)
  a2 <- a2[a2$type=="sentence"]
  l.w <- as.matrix( lapply(a2$features, function(x) length(x$constituents)) ) #sent length


  d<-data.frame(id=ind,lenbychar=l,lenbyword=l.w,sent=sapply( s[a1], function(x) substr(x,0,45)))
  d.full <- rbind(d.full,d)
}
rownames(d.full) <- 1:dim(d.full)[1]
print(d.full)

d <- data.frame()
pos_tag_annotator = Maxent_POS_Tag_Annotator()
for (i in 1:10) {

  ind <- top10[i]
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
nums <-sapply( d$words, function(x) suppressWarnings(!is.na(as.numeric(as.character(x)))))
d.smaller <- d[!nums,]
d.smaller <- unique(d.smaller)
d.smaller <- d.smaller[order(d.smaller$words),]
head(d.smaller)


## Analyze word frequency using functions from package zipfR
library(zipfR)
for(i in 1:10) {
  ind <- top10[i]
  f <- as.matrix(ACQdm2[ind,]) # frequencies for all words

  z <- tfl(f) #
  plot(sort(z$f,decreasing=TRUE), xlab="rank",ylab="frequency",
       main=paste("Type Frequency Plot for ID: ",acq[[ind]]$meta$id))
}

f <- colSums(as.matrix(ACQdm2[top10,])) # frequencies for all words
z <- tfl(f) #
plot(sort(z$f,decreasing=TRUE), xlab="rank",ylab="frequency",
     main="Type Frequency Plot for Top 10")



     for(i in 1:10) {
       ind <- top10[i]
       f <- as.matrix(ACQdm2[ind,]) # frequencies for all words

       z <- tfl(f) #
       s <- tfl2spc(z)
       plot(s, main=paste("Frequency Spectrum for ID: ",acq[[ind]]$meta$id))
     }
