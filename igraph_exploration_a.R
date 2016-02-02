library(igraph)
library(sna)
igraph <- load("~/Downloads/astrocollab.Rdata")
load("football")
summary(astrocollab)
#how to simplify

#make igraph an adjacency matrix to do ego.extract
graph.a <- graph.adjacency(igraph, mode = "undirected")
ego.extract(graph.a)
