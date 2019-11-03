#Sentiment Analisis 

#Tesla Twitter Timeline
TSLATwitterTimeline.df




TSLATwitterTimeline.df.Corpus <- SentimentAnalysis::preprocessCorpus(
  corpus = TSLATwitterTimeline.df$text,
  language = "english",
  stemming = TRUE,
  removeStopwords = TRUE
)




#Followers Twitter Timeline
TSLAFollowerTweets.df

