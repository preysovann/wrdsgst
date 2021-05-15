# Initialize necessary libraries.
#install.packages("stringi")

library(ggplot2)
library(stringi)
library(dplyr)
library(tm)
library(NLP)
library(RWeka)
library(tidyr)



# load data
ds_twitter <- readLines("./Data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", 
                        encoding="UTF-8", skipNul = TRUE)
ds_blog <- readLines("./Data/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", 
                     encoding="UTF-8", skipNul = TRUE)
ds_news <- readLines("./Data/Coursera-SwiftKey/final/en_US/en_US.news.txt", 
                     encoding="UTF-8", skipNul = TRUE)

num_words_twitters <- stri_stats_latex(ds_twitter)[4]
num_words_blogs <- stri_stats_latex(ds_blog)[4]
num_words_news <- stri_stats_latex(ds_news)[4]

nchar_twitter<-sum(nchar(ds_twitter))
nchar_blog<-sum(nchar(ds_blog))
nchar_news<-sum(nchar(ds_news))

data.frame("Doc_Name" = c("twitter", "blogs", "news"),
           "Num_Lines" = c(length(ds_twitter),length(ds_blog), length(ds_news)),
           "Num_Words" = c(sum(num_words_twitters), sum(num_words_blogs), sum(num_words_news)),
           "Num_Character"=c(nchar_twitter, nchar_blog, nchar_news))

set.seed(1980)
blogs_c<-iconv(ds_blog,"latin1","ASCII",sub="")
twitter_c<-iconv(ds_twitter,"latin1","ASCII",sub="")
news_c<-iconv(ds_news,"latin1","ASCII",sub="")

# sampledata<-c(sample(twitter_c,length(twitter_c)*0.1),
#               sample(blogs_c,length(blogs_c)*0.1),
#               sample(news_c,length(news_c)*0.1))

sampledata<-c(sample(twitter_c, size = 1500, replace = TRUE),
              sample(blogs_c, size = 1500, replace = TRUE),
              sample(news_c, size = 1500, replace = TRUE))


corpus <- VCorpus(VectorSource(sampledata))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

corpusresult<-data.frame(text=unlist(sapply(corpus,'[',"content")),
                         stringsAsFactors = FALSE)
str(corpusresult)

unigram<-function(x) NGramTokenizer(x,Weka_control(min=1,max=1))
unigramtab<-TermDocumentMatrix(corpus,control=list(tokenize=unigram))
unigramcorpus<-findFreqTerms(unigramtab,lowfreq=0)
unigramcorpusnum<-rowSums(as.matrix(unigramtab[unigramcorpus,]))
unigramcorpustab<-data.frame(word=names(unigramcorpusnum),freq=unigramcorpusnum)
unigramcorpussort<-unigramcorpustab[order(-unigramcorpustab$freq),]
row.names(unigramcorpussort) <- NULL
unigram.cp <- unigramcorpussort

bigram<-function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
bigramtab<-TermDocumentMatrix(corpus,control=list(tokenize=bigram))
bigramcorpus<-findFreqTerms(bigramtab,lowfreq=0)
bigramcorpusnum<-rowSums(as.matrix(bigramtab[bigramcorpus,]))
bigramcorpustab<-data.frame(word=names(bigramcorpusnum),freq=bigramcorpusnum)
bigramcorpussort<-bigramcorpustab[order(-bigramcorpustab$freq),]


bigramcorpussort <- separate(data = bigramcorpussort, col = word, 
                             into = c("word1", "word2"), sep = " ")
row.names(bigramcorpussort) <- NULL

bigram.cp <- merge(bigramcorpussort, unigram.cp, by.x = 'word1', by.y = 'word')
bigram.cp <- cbind(bigram.cp, bigram.cp[, 3]/bigram.cp[,4] ) 
bigram.cp <- bigram.cp[, c(1,2,3,5)]
names(bigram.cp) <- c("word1", "word2", "freq", "probability")
bigram.cp <- bigram.cp[order(bigram.cp$probability, decreasing=TRUE), ];
head(bigram.cp)


trigram<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
trigramtab<-TermDocumentMatrix(corpus,control=list(tokenize=trigram))
trigramcorpus<-findFreqTerms(trigramtab,lowfreq=0)
trigramcorpusnum<-rowSums(as.matrix(trigramtab[trigramcorpus,]))
trigramcorpustab<-data.frame(word=names(trigramcorpusnum),freq=trigramcorpusnum)
trigramcorpussort<-trigramcorpustab[order(-trigramcorpustab$freq),]


trigramcorpussort <- separate(data = trigramcorpussort, col = word, 
                              into = c("word1", "word2", "word3"), sep = " ")
row.names(trigramcorpussort) <- NULL

trigram.cp <- merge(trigramcorpussort, bigram.cp[,c("word1","word2","freq")], 
                    by=c("word1", "word2"))
trigram.cp <- cbind(trigram.cp, trigram.cp[, 4]/trigram.cp[,5] ) 
trigram.cp <- trigram.cp[, c(1, 2, 3, 4, 6)]
names(trigram.cp) <- c("word1", "word2", "word3", "freq", "probability")
trigram.cp <- trigram.cp[order(trigram.cp$probability, decreasing=TRUE), ];
head(trigram.cp)

#------------------------------------------------------------

quadrigram<-function(x) NGramTokenizer(x,Weka_control(min=4,max=4))
quadrigramtab<-TermDocumentMatrix(corpus,control=list(tokenize=quadrigram))
quadrigramcorpus<-findFreqTerms(quadrigramtab,lowfreq=0)
quadrigramcorpusnum<-rowSums(as.matrix(quadrigramtab[quadrigramcorpus,]))
quadrigramcorpustab<-data.frame(word=names(quadrigramcorpusnum),
                                freq=quadrigramcorpusnum)
quadrigramcorpussort<-quadrigramcorpustab[order(-quadrigramcorpustab$freq),]


quadrigramcorpussort <- separate(data = quadrigramcorpussort, col = word, 
                              into = c("word1", "word2", "word3", "word4"), sep = " ")
row.names(quadrigramcorpussort) <- NULL

quadrigram.cp <- merge(quadrigramcorpussort, trigram.cp[,c("word1","word2", 
                                                           "word3", "freq")], 
                    by=c("word1", "word2", "word3"))
quadrigram.cp <- cbind(quadrigram.cp, quadrigram.cp[, 5]/quadrigram.cp[,6] ) 
quadrigram.cp <- quadrigram.cp[, c(1, 2, 3, 4, 5, 7)]
names(quadrigram.cp) <- c("word1", "word2", "word3", "word4", "freq", "probability")
quadrigram.cp <- quadrigram.cp[order(quadrigram.cp$probability, decreasing=TRUE), ];
head(quadrigram.cp)


#------------------------------------------------------------

names(bigram.cp) <- c("word1", "predicted", "freq", "probability")
names(trigram.cp) <- c("word1", "word2", "predicted", "freq", "probability")
names(quadrigram.cp) <- c("word1", "word2", "word3","predicted", "freq", "probability")

dim(unigram.cp)
dim(bigram.cp)
dim(trigram.cp)
dim(quadrigram.cp)

save(unigram.cp, bigram.cp, trigram.cp, quadrigram.cp, file = "ngrams.Rda")

