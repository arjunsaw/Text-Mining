setwd("~/Desktop/MSBAPM/R/R files")


## INSTALLING PACKAGES THAT WE WILL USE TODAY
doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ROAuth", "twitteR", "streamR", "ggplot2", "stringr",
               "tm", "RCurl", "maps", "Rfacebook", "topicmodels", "devtools")

#####################################
### CREATING YOUR OWN OAUTH TOKEN ###
#####################################

## Step 1: go to apps.twitter.com and sign in
## Step 2: click on "Create New App"
## Step 3: fill name, description, and website (it can be anything, even google.com)
##			(make sure you leave 'Callback URL' empty)
## Step 4: Agree to user conditions
## Step 5: copy consumer key and consumer secret and paste below

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "33###############"
consumerSecret <- "####################"


my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## now you can save oauth token for use in future sessions with twitteR or streamR
save(my_oauth, file="oauth_token.Rdata")


### NOTE (added February 17, 2015)
### The twitteR package just changed its authentication method
### (streamR remains the same)
### New code to authenticate with twitteR now requires access token and access secret,
### which can be found in 'Keys and Access Tokens' tab in apps.twitter.com

accessToken = '########-################'
accessSecret = '##################'

## testing that it works
library(twitteR)
setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret,
access_token=accessToken, access_secret=accessSecret)
unitedtweets = searchTwitter('united airlines',  n = 3000, lang = "en")
unitedtweets = twListToDF(unitedtweets)
View(unitedtweets)
#write.csv(x = unitedtweets,file = "~/Desktop/MSBAPM/R/R files/unitedtweets.csv")
unitedtweets = read.csv("~/Desktop/MSBAPM/R/R files/unitedtweets.csv")
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


library(NLP)
library(tm)
# converting to corpus
corpus = Corpus(VectorSource(Review))
corpus[[1]]$content
# converting to lower
corpus = tm_map(corpus,tolower)
corpus[[1]]$content
#corpus = tm_map(corpus,removePunctuation)
# remove the common words
corpus = tm_map(corpus,removeWords,c("united","airlines","flight",stopwords("english")))
corpus[[1]]$content

corpus = tm_map(corpus,stemDocument)
corpus[[1]]$content

frequencies = DocumentTermMatrix(corpus)
inspect(frequencies[1:5,505:515])

findFreqTerms(frequencies,lowfreq = 20)
sparse = removeSparseTerms(frequencies,0.995)
tweetsparse = as.data.frame(as.matrix(sparse))

colnames(tweetsparse) = make.names(colnames(tweetsparse))
View(tweetsparse)


count = colnames(tweetsparse)
freq = colSums(tweetsparse)
library(RColorBrewer)
library(wordcloud)


wordcloud(count, freq, min.freq = sort(freq, decreasing = TRUE)[[100]],  colors=brewer.pal(8,"Dark2"), random.color=TRUE)


png("wordcloud.png")


# Text Parsing
# Text 
library(sentimentr)
# taking out the average sentiment score
sentiment = with(Rev)












library(twitteR)

# profile information
user <- getUser('united')
# from a Windows machine
# user <- getUser('barackobama', cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

View(user$toDataFrame())

# followers
user$getFollowers(n=10)
# (10 most recent followers)

# from a Windows machine
# user$getFollowers(n=10, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# friends (who they follow)
user$getFriends(n=10)

