library(twitteR)
library(tm) #text manipulation library
library(wordcloud)
library(RColorBrewer)

#insert your secret codes from your twitter account visit apps.twitter.com

consumer_key <- 'XXXXXXXXXXXXX'
consumer_secret <- 	'XXXXXXXXXXXXX'
access_token <- 'XXXXXXXXXXXXX'
access_secret <- 'XXXXXXXXXXXXX'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

search.tweets <- searchTwitter("USA", n=2000, lang="en") #search the term in twitter & returns tweet data
search.text <- sapply(search.tweets, function(x) x$getText()) #grab text data from tweet data

#clean data
search.text <- iconv(search.text, 'UTF-8', 'ASCII') # remove emoticons
search.corpus <- Corpus(VectorSource(search.text)) # create a corpus

#create document term matrix
term.doc.matrix <- TermDocumentMatrix(search.corpus,
                                      control = list(removePunctuation = TRUE,
                                                     stopwords = c("https", stopwords("english")),
                                                     removeNumbers = TRUE,tolower = TRUE))

term.doc.matrix <- as.matrix(term.doc.matrix) #convert object into a matrix

word.freqs <- sort(rowSums(term.doc.matrix), decreasing=TRUE) 
dm <- data.frame(word=names(word.freqs), freq=word.freqs) #create data frame with words and counts
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")) #create the word cloud

