

library(ROAuth)
library(twitteR)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "#########################"
consumerSecret <- "##############################"

my_oauth = OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

save(my_oauth, file="oauth_token.Rdata")

accessToken = '########-#################'
accessSecret = '##################'


setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret,
                    access_token=accessToken, access_secret=accessSecret)

# profile information
user = getUser('united')
timeline = userTimeline("united", n=1000)
united = twListToDF(timeline)
View(united)
write.csv(x = united,file = "~/Desktop/MSBAPM/R/R files/unitedtimeline.csv")

Rev = unitedtweets$text

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

# Creating corpus, apply transformations and get document term matrix

# corpus
corpus = Corpus(VectorSource(Review))

# convert to lower case
corpus = tm_map(corpus, tolower)
# remove stoprwords
corpus = tm_map(corpus, removeWords, c(stopwords("english"), "united"))
# remove extra white-spaces
corpus = tm_map(corpus, stripWhitespace)

# term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
m = as.matrix(tdm)

# now we will keep the most frequent terms, here we keep the terms that have frequency >90

# remove sparse terms (word frequency > 90% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]

# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]

# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1

# cluster analysis to check the group of words
# distance matrix with binary distance
m1dist = dist(m1, method="binary")

# cluster with ward method
clus1 = hclust(m1dist, method="ward")

# plot dendrogram
plot(clus1, cex=0.7)

#For a better visualization, we can apply a
#Correspondence Analysis (using package FactoMineR)
library(FactoMineR)
# correspondance analysis
united_ca = CA(m1, graph=FALSE)

# default plot of words
plot(united_ca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
text(united_ca$row$coord[,1], united_ca$row$coord[,2], labels=rownames(m1),
     col=hsv(0,0,0.6,0.5))
title(main="@UNITED Correspondence Analysis of tweet words", cex.main=1)

#To improve the correspondance analysis plot, we can apply a clustering method
#like k-means or partitioning around medoids (pam)
install.packages("pam")
library(pam)
# partitioning around medoids with 6 clusters
k = 6

# pam clustering
united_kmeans = kmeans(united_ca$row$coord[,1:2], k)

# get clusters
clusters = united_kmeans$cluster

# create data frame
united_words_df = data.frame(
  words = rownames(m1),
  dim1 = united_ca$row$coord[,1],
  dim2 = united_ca$row$coord[,2],
  freq = rowSums(m1),
  cluster = as.factor(clusters))

library(ggplot2)
# plot
ggplot(united_words_df, aes(x=dim1, y=dim2, label=words)) +
  geom_text(aes(size=freq, colour=cluster), alpha=0.7) +
  scale_size_continuous(breaks=seq(20,80,by=10), range=c(3,8)) +
  scale_colour_manual(values=brewer.pal(8, "Dark2")) +
  labs(x="", y="") 
  
