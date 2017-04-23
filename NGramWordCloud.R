setwd("~/Desktop/MSBAPM/R/R files")

# sudo R CMD javareonf
# sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

library(stringr)
library(wordcloud)
library(ggmap)
library(plyr)
library(dplyr)
install.packages("rJava",type='source')
library(rJava)
install.packages("RWeka")
library(RWeka)
library(tm)
library(koRpus)
library(ggplot2)
library(wordcloud)

## N Gram Wordcloud
unitedtweets = read.csv("~/Desktop/MSBAPM/R/R files/all_tweets.csv",stringsAsFactors = FALSE)
Rev = unitedtweets$text

# Transformations
# remove retweet entities
Review = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Rev)
# remove Atpeople
Review = gsub("@\\w+", "", Review)
# remove punctuation symbols
Review = gsub("[[:punct:]]", "", Review)
# remove numbers
Review = gsub("[[:digit:]]", "", Review)
# remove links
Review = gsub("http\\w+", "", Review)
# Remove unicode character
Review = gsub("^\\s*<U\\+\\w+>\\s*", "", Review)

## N-Gram Wordcloud
# After gsub, creating a VCorpus (which is a volatile Corpus) instead of Corpus to generate n grams.
# We create this towards the end of all transformations because the meta data of the corpus 
# would get corrupted and we would face difficulties in generatating ngrams.
corpus = Corpus(VectorSource(Review))
getTransformations()
corpus = tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus,removeNumbers)
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,c("united","airlines","flight",
                                     "eduaubdedubu","unitedairlines","united airlines",
                                     "daos","uthis", "dao",
                                     "dude","rosa",stopwords("english")))
corpus[[50]]$content
a = list()
for (i in seq_along(corpus)) 
  {
  a[i] = gettext(corpus[[i]][[1]]) #Do not use $content here!
  }

Review = unlist(a) 
corpus = VCorpus(VectorSource(Review))

# Bi Grams
options(mc.cores=1)
BigramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) # create n-grams
tdm.bigram = TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)) # create tdm from n-grams
tdm.bigram
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df,20)

library(wordcloud2)
figpath = "~/Desktop/MSBAPM/R/R files/t.png"
wordcloud2(freq.df, figPath = figpath, size = 3,color = 'random-dark',fontFamily = 'cursive',
           backgroundColor = "black")


findFreqTerms(tdm.bigram, lowfreq = 500)
