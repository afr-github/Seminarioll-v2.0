#Sentiment Analisis 

#Tesla Twitter Timeline
TSLATwitterTimeline.df

TSLATwitterTimeline.df.corpus <- VCorpus(VectorSource(TSLATwitterTimeline.df$text))

TSLATwitterTimeline.df.SA <- SentimentAnalysis::preprocessCorpus(
  corpus = TSLATwitterTimeline.df.corpus,
  language = "english",
  stemming = TRUE,
  removeStopwords = TRUE
)

TSLATwitterTimeline.df.AS <- analyzeSentiment(TSLATwitterTimeline.df.corpus)

View(TSLATwitterTimeline.df.SA)

#Procesar Texto de twitter timeline de tesla ..








#Followers Twitter Timeline
TSLAFollowerTweets.df


#Procesar Texto de twitter timeline de Followers






