library(stringr)
library(wordcloud)
library(ggmap)
library(dplyr)
library(plyr)
#Score  =  Number of positive words  -  Number of negative words
#If Score > 0, this means that the sentence has an overall 'positive opinion'
#If Score < 0, this means that the sentence has an overall 'negative opinion'
#If Score = 0, then the sentence is considered to be a 'neutral opinion'

positives= readLines("~/Desktop/MSBAPM/R/R files/positive-words.txt")
negatives = readLines("~/Desktop/MSBAPM/R/R files/negative-words.txt")

sentiment_scores = function(unitedtweets, positive_words, negative_words, .progress='none')
  {
    scores = laply(unitedtweets,
                 function(unitedtweets, positive_words, negative_words)
                   {
                    #unitedtweets = gsub("[[:punct:]]", "", unitedtweets)    # remove punctuation
                    #unitedtweets = gsub("[[:cntrl:]]", "", unitedtweets)   # remove control characters
                   #unitedtweets = gsub('\d+', '', unitedtweets)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   unitedtweets = sapply(unitedtweets, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(unitedtweets, "^\\s+")
                   words = unlist(word_list)
                   # compare words to the dictionaries of positive & negative terms
                   positive_matches = match(words, positives)
                   negative_matches = match(words, negatives)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive_matches = !is.na(positive_matches)
                   negative_matches = !is.na(negative_matches)
                   # final score
                   score = sum(positive_matches) - sum(negative_matches)
                   return(score)
                 }, positive_matches, negative_matches, .progress=.progress )
  return(scores)
}

score = sentiment_scores(unitedtweets, positives, negatives, .progress='text')
score
#Let?s plot a histogram of the sentiment score:
  
  hist(score,xlab=" ",main="Sentiment of United Airlines Tweet ",
       border="black",col="blue")