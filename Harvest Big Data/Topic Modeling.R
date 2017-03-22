library(tm)
library(topicmodels)
library(SnowballC)
library(dplyr)
dirname <- file.path(getwd(),"data")
docs <- Corpus(DirSource(dirname, encoding = "UTF-8"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)
tfm <- DocumentTermMatrix(docs)
results <- LDA(tfm, control=list(seed=2), k = 6, method = "Gibbs")
Terms <- terms(results, 5,0.015) 
Terms
for(i in c(0.005,0.01,0.015,0.02,0.025)){
  print(paste(i,mean(as.numeric(summary(terms(results, 20,i))[,1]))))
}
posterior <- posterior(results)[[2]]

type(posterior)
rownames(posterior) = as.numeric(gsub(".txt", "", rownames(posterior), fixed = TRUE))

posterior1 <- as.data.frame(posterior)
posterior1 <- posterior1[order(as.numeric(rownames(posterior1))),]
posterior1 <- posterior1[1:997,]
posterior1$max <- apply(posterior1, 1, which.max)

score <- read.csv("dd.csv",header = FALSE)
score$V2 <- posterior1$max
score <- rename(score,Score=V1)
score <- rename(score,Topic=V2)
write.csv(score,"score.csv")
