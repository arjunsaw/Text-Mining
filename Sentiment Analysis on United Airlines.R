setwd("~/Desktop/MSBAPM/R/R files")

# just checking the hashtags
unitedtweets = read.csv("~/Desktop/MSBAPM/R/R files/all_tweets.csv")

# load packages
library(stringr)
library(wordcloud)
library(wordcloud2)
library(tm)

unitedtweets = unitedtweets$text
#user = getUser('united')
# get the hashtags
united_hashtags = str_extract_all(unitedtweets, "#\\w+")
# put tags in vector
united_hashtags = unlist(united_hashtags)
# calculate hashtag frequencies
united_hashtags_freq = table(united_hashtags)

hashcorpus = Corpus(VectorSource(united_hashtags))
hashdtm = DocumentTermMatrix(hashcorpus)
hashfrequency = colSums(as.matrix(hashdtm))
hash_df= data.frame(term=names(hashfrequency),occurences=hashfrequency)

letterCloud(hash_df, word = "#",size = 4,minRotation = -pi/4,maxRotation = pi/4,
            ellipticity = 0.65, fontWeight = 'Bold')

# united hashtags wordcloud
wordcloud(names(united_hashtags_freq), united_hashtags_freq, random.order=FALSE, 
          colors="#1B9E77")



#title("\n\nHashtags in tweets from @United",
     # cex.main=1.5, col.main="gray50")



# Sentimental Analysis

# packages used are
# for text mining
library(tm)
# required for stemming
library(SnowballC)
# to plot
library(ggplot2)
# wordcloud
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)

unitedtweets = read.csv("~/Desktop/MSBAPM/R/R files/all_tweets.csv")
Rev = unitedtweets$text
View(unitedtweets)

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

# Corpus - it is the collection of documents
# converting to corpus
corpus = Corpus(VectorSource(Review))
corpus
# it shows that total number of documents is 3000 i.e total number of tweets we have
# we can explore more by using summary but it doesn't give us much insights here
summary(corpus)
# lets explore a particular document, say 50
writeLines(as.character(corpus[[50]]))
# OR by using the below command 
corpus[[50]]$content
# now we need to clean the data 
# tm package provides many options to do the same
# to check the options
getTransformations()
# output is "removeNumbers","removePunctuation","removeWords","stemDocument","stripWhitespace"
# we need to decide what we have to use
# we have already done the cleaning part using gsub above but still we will write the code
corpus = tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus,removeNumbers)
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removePunctuation)
# here i have added extra words after i checked in the word cloud
corpus = tm_map(corpus,removeWords,c("united","airlines","flight",
                                     "eduaubdedubu","unitedairlines",
                                     "daos","uthis", "dao",
                                     "dude","rosa",stopwords("english")))
# lets see the corpus content now
corpus[[50]]$content
# Before it was 
# A United Airlines passenger was reportedly stung by a scorpion on a return flight to Canada 
# Now it is
# passenger  reportedly stung   scorpion   return   canada 


# STEMMING
# now we will do stemming
# so normally there are words that have a common root for ex - here reportedly comes from report
# Stemming is the process of reducing such related words to their common root, which in this case 
# would be the word report
# here we will use snowballC for stemming
corpus = tm_map(corpus, stemDocument)

# stem completion
#copy = corpus ## creating a copy to be used as a dictionary
#x = c("compan", "entit", "suppli", "passenge")
#stemCompletion(x, copy)

# lets check the content again
corpus[[50]]$content
# Before - passenger  reportedly stung   scorpion   return   canada 
# After - passeng report stung scorpion return canada
# So in tm package, stemming algorithms work by chopping of the ends and is a crude process. 
# This can cause problem for ex here passenger becomes passeng
# But the overall benefit gained from stemming more than makes up for the downside of such cases.

# DOCUMENT TERM MATRIX
# a matrix that lists all occurrences of words in the corpus, by document.
# In the DTM, the documents are represented by rows and the terms (or words) by columns
# If a word occurs in a particular document, then the matrix entry for corresponding to that 
# row and column is 1, else it is 0 
dtm = DocumentTermMatrix(corpus)
# Let's checkout the summary
dtm
# it shows that it has 2198 terms
# this is a 3000 x 2198 matrix and according to the results 100% of the rows are zeros
# so let's inspect a small section of dtm
inspect(dtm[2:10,1:10])
# output is 
# Docs apolog bin ceo fall fiasco fix promis tourist travel wanderlust
# 10      0   0   0    0      0   0      0       0      0          0
# 2       0   1   0    1      0   0      0       0      0          0
# this means that in doc 2, bin shows up once and fall shows up once


# MINING THE CORPUS

# now we should get the frequency of each word in the corpus
frequency = colSums(as.matrix(dtm))
# so the total number of words we have are
length(frequency)
# the length is 2198
# now we should sort the frequency in descending order so that we can get those words that occured 
# most of the times
ordered = order(frequency, decreasing = TRUE)
# lets list most and the least occuring frequencies
frequency[head(ordered)]
# output is 
# passeng    today     imag     drag scorpion     fall 
# 970      399      322      302      291      201 
frequency[tail(ordered)]
# output is 
#faster shouldu firstclass privatecharteru
#  1      1         1         1
 

# now lets see those terms that exist for 100 times atleast
findFreqTerms(dtm, lowfreq = 100)
# output is "bin","fall","overhead","passeng","scorpion","sting","drag","lawyer","say"
# "custom","overbook","remov","incid","shoulder","tap","today","nose","man"   
# "imag","like","new","watch","video" 

# Now that we have the most frequently occurring terms in hand, we can check for correlations between 
# some of these and other terms that occur in the corpus.In this context, correlation is a quantitative 
# measure of the co-occurrence of words in multiple documents.

# let's see for word overhead
findAssocs(dtm,"overhead", 0.6)
# it shows 
# bin     fall    sting scorpion 
# 0.99     0.94     0.93     0.78 
# this means overhead shows up along with these terms quite a lot and are correlated


# PLOTS

# lets plot for frequency first
wf= data.frame(term=names(frequency),occurences=frequency)
library(ggplot2)
View(wf)
ggplot(data = subset(wf, frequency>180)) +
  geom_bar(mapping = aes(x = term, y = occurences),stat = "identity")

# lets create a word cloud
library(wordcloud)
wordcloud(names(frequency),frequency, min.freq = 50,colors=brewer.pal(8,"Dark2"), random.color=TRUE)
figpath = "~/Desktop/MSBAPM/R/R files/t.png"
figpath1 = "~/Desktop/MSBAPM/R/R files/angry.png"

wordcloud2(wf, figPath = figpath, size = 3,color = 'random-dark',fontFamily = 'cursive')
# font families - "serif", "sans-serif", "cursive", "fantasy", "monospace"
letterCloud(wf, word = "JOSE", size = 2)
#Score  =  Number of positive words  -  Number of negative words
#If Score > 0, this means that the sentence has an overall 'positive opinion'
#If Score < 0, this means that the sentence has an overall 'negative opinion'
#If Score = 0, then the sentence is considered to be a 'neutral opinion'

# stemcompletion(stemmed corpus,dictionary )
# n- grams, bi grams udsing tokenizer
# rweka package

# Tokenization - process of converting a text into tokens
# Tokens - words or entities present in the text
# Text object - a sentence or a phrase or a word or an article

library(NLP)

# Text Preprocessing
# The entire process of cleaning and standardization of text, making it noise-free and ready  
# for analysis is known as text preprocessing
# It is predominantly comprised of three steps:
# 1) Noise Removal
# 2) Lexicon Normalization
# 3) Object Standardization
# Raw text --> Stopwords,URL's,Punctuations,Mentions,etc --> Tokenization, Lemmatization, Stemming
#                 (Noisy entities removal)                        (Word Normalization)
# --> Regular Expression, Lookup Tables --> Cleaned Text
#       (Word Standardization)

# Noise Removal - We have already done that
# Lexicon Normalization - The most common lexicon normalization practices are :
# a) Stemming:  Stemming is a rudimentary rule-based process of stripping the 
# suffixes (???ing???, ???ly???, ???es???, ???s??? etc) from a word.
# b) Lemmatization: Lemmatization, on the other hand, is an organized & step by step procedure
#  of obtaining the root form of the word, it makes use of vocabulary (dictionary importance 
# of words) and morphological analysis (word structure and grammar relations).
# for lemmetization 
# I tried to do lemmtization bit somehow wordnet package is not working here, moving on

# Object Standardization
# Text data often contains words or phrases which are not present in any standard lexical  
# dictionaries. These pieces are not recognized by search engines and models.
# for ex - RT - retweet, awsm - awesome, luv - love



