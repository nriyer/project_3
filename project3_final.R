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
head(acq)

#compilation of 50 news articles with additional meta information form the 
#Reuters-21578 data se of topic acq. 13 documents
ACQ <- acq
inspect(ACQ[1:2])

meta(ACQ[[2]], "id") #this is another way to reference 

#extract one document
text1 <- ACQ[[1]]
text1
names(ACQ)
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
inspect(ACQ[1:2])
inspect(ACQstop[1:2])

#document matrix 2 - inspect once all stopwords, punctuation, numbers taken out
ACQdm2 <- DocumentTermMatrix(ACQstop, control= list(wordLenghts = c(1,Inf)))
ACQdm2


#find terms with a frequency of 5 or more
freq.terms <- findFreqTerms(ACQdm2, lowfreq=5)
freq.terms

#find terms associated with "states"- measure of the co-occurrence of words in multiple documents.
#words that co-occur with the given word
#ex below, all words that co occur with states in 50% of the docs
findAssocs(ACQdm2, "states", 0.25)
findAssocs(ACQdm2, "year", 0.25)

term.freq <- rowSums(as.matrix(ACQdm2))
term.freq <- subset(term.freq, term.freq <= 5)
termdf <- data.frame(term = names(term.freq),freq=term.freq)
term_sort <- termdf %>% arrange(desc(freq))
term_sort[1:50,]
#plot top 50 frequent terms
ggplot(term_sort[1:50,], aes(x=term,y=freq)) + geom_bar(stat = "identity") + 
  xlab("Terms") + ylab("Count") + coord_flip()

#########################################################################################
inspect(ACQcl)

#we're going to start with the document after punctuation has been removed
dtm <- DocumentTermMatrix(ACQcl)
#remove sparse terms, leaving a matrix that is 10% empty space, max
dtms <- removeSparseTerms(dtm, 0.1)
inspect(dtms)
removeWords
top10 <- sort(rowSums(as.matrix(dtms)), decreasing = TRUE)[1:10] #use dtm for with sparsity
top10 <- as.data.frame(top10)
top10
myStopwords <- c(stopwords('english'))
inspect(ACQcl) #without punctuation 
doc7 <- ACQcl[1] #doc7 has 73 words 
doc7 <- tm_map(doc7, removeWords, myStopwords) #remove stopwords 
doc7m <- DocumentTermMatrix(doc7) 
doc7m <- removeSparseTerms(doc7m, 0.25)
inspect(doc7m)
doc7m


################################################

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

top10
topdocs <- mycorpus[mycorpus$documents$id %in% top10]
str(topdocs[[1]])

#sumarry of whole doc and top docs as data frame
summary_td <- as.data.frame(summary(topdocs))


#dendogram for top 10
acq.mat <- as.matrix(t(tdm2))
acq.mat <- as.data.frame(acq.mat)
acq.mat <- acq.mat[,top10]
acq.mat <- as.matrix(acq.mat)

distMatrix <- dist(scale(acq.mat[,1]))
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit,main = "Dendogram")

#top 10 dendogram, 1 for each of the top 10 documents
top10.dendogram <- function(tdm2,doc)
{
  acq.mat <- as.matrix(t(tdm2))
  acq.mat <- as.data.frame(acq.mat)
  acq.mat <- acq.mat[,top10]
  acq.mat <- as.matrix(acq.mat)
  distMatrix <- dist(scale(acq.mat[,doc]))
  fit <- hclust(distMatrix, method = "ward.D2")
  print(plot(fit,main = "Dendogram"))
}
for (i in 1:10){
top10.dendogram(tdm2,i)
}
#word cloud for top 10
wordcloud.func <- function(ACQstop, doc)
{
  dtm <- TermDocumentMatrix(ACQstop)
  m <- as.data.frame(as.matrix(dtm))
  m <- m[,top10]
  m <- as.matrix(m)
  v <- sort(m[,doc],decreasing=TRUE)
  d <- data.frame(word = row.names(m),freq=v)
  set.seed(1234)
  print(wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2")))
}
for (i in 1:10){
  wordcloud.func(ACQstop,i)
}

#####FIND LONGEST WORD in 10 docs
max_length <- c()
word <- c()
id <- c()
for (i in 1:10){
  words <- tokenize_words(topdocs[[i]][[1]])
  word[i] <- words[nchar(words) == max(nchar(words))]
  max_length[i] <- max(nchar(words))
  id[i] <- names(topdocs[i])
}

final.longest_word <- data.frame(max_length = max_length,word=word,id = id)

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
#remove punctuation from topdocs
str(count_sentences$sentence)
count_sentences$sentence_nopunct[4] <- ( gsub("[[:punct:]]", "", count_sentences$sentence[4]) )
#loop to remove punctuation from each sentence
nopunct <- c()
for (i in 1:nrow(count_sentences)){
  nopunct[i] <-  ( gsub("[[:punct:]]", "", count_sentences$sentence[i]) )
}
#bind final df together
final_nopunct_df <- cbind(nopunct,count_sentences)

########################################
#part of speech                     ####
########################################
str(topdocs)
library(koRpus)
# perform POS tagging
corpus_top10 <- Corpus(VectorSource(topdocs))
writeCorpus(corpus_top10)
text.tagged <- treetag("1.txt", treetagger="manual", lang="en",
                       TT.options=list(path="/Applications/tree-tagger-MacOSX-3.2-intel", preset="en"))
#find out why lib is not in tree tagger...then the above should work.


########
#parts of speech - not complete, we have a list of words. all tht is left is tagging parts of speech
all_words <- c()
id.all <- c()

for (i in 1:10){
  all_words <- tokenize_words(topdocs[[i]][[1]])
  id.all[i] <- names(topdocs[i])
}

all.words <- as.data.frame(all_words)
all.words$all_words <- as.character(all.words$all_words)


#################################################################################
##not sure this works, backup

#for zipfr
library(languageR)
document.spc <- text2spc.fnc(strsplit(x[[1]][1]$content," "))
document.vgc <- growth.fnc(strsplit(acq10[[1]][1]$content," "))
text <- strsplit(document$content, " ")[[1]]

##create spc object
this.spc <- text2spc.fnc(text)

#create growth object
this_growth<- growth.fnc(text)
all.words[1,]

