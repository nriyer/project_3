library(tm)
library(dplyr)

data("acq")
head(acq)

#compilation of 50 news articles with additional meta information form the 
#Reuters-21578 data se of topic acq. 13 documents
ACQ <- VCorpus(DirSource(".", ignore.case = TRUE, mode="text"))
ACQ

#extract one document
text1 <- ACQ[[1]]
text1
#document term frequency
test1tf <- as.data.frame(termFreq(text1))
#rank words most to least
rank_of_words <- cbind(as.data.frame(rownames(test1tf)),test1tf %>% arrange(desc(termFreq(text1))))

#convert corpus to lowercase
#LAZY = TRUE so that only docs being addressed are transformed, since this is a large body
ACQlow <- tm_map(ACQ, content_transformer(tolower),lazy = TRUE)
ACQlow

#remove anything other than English letters or spaces
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
ACQcl <- tm_map(ACQlow,content_transformer(removeNumPunct(ACQlow)), lazy = TRUE)

#remove stopwords from corpus
stopwords <- c(stopwords('english'))
ACQstop <- tm_map(ACQcl, removeWords, stopwords, lazy = TRUE)
