#Sentiment Analisis 

#Tesla Twitter Timeline
#Aqui se realiza todo el proceso del procesado del texto
TSLASentimentAnalysis.Procesar <- function(){
  TSLATwitterTimeline.df.corpus <- VCorpus(VectorSource(TSLATwitterTimeline.df$text))
  
  TSLATwitterTimeline.df.SA <- SentimentAnalysis::preprocessCorpus(
    corpus = TSLATwitterTimeline.df.corpus,
    language = "english",
    stemming = TRUE,
    removeStopwords = TRUE
  )
  
  TSLATwitterTimeline.df.corpus <- tm_map(
    x = TSLATwitterTimeline.df.corpus,
    FUN = content_transformer(tolower)
  )
  
  TSLATwitterTimeline.df.corpus.df <- data.frame(
    text = sapply(
      X = TSLATwitterTimeline.df.corpus,
      FUN = as.character
    ),
    stringsAsFactors = FALSE
  )

  
  #Dont run just yet
  TSLATwitterTimeline.df.corpus <- sub(
    pattern = "rt",
    replacement = '',
    x = TSLATwitterTimeline.df.corpus
  )
  
  View(TSLATwitterTimeline.df.corpus)
  
  return()#Objeto con loa información del corpus en data.frame
}









#ERROR TSLATwitterTimeline.df.AS <- analyzeSentiment(TSLATwitterTimeline.df.corpus)

View(TSLATwitterTimeline.df.SA)

#Procesar Texto de twitter timeline de tesla ..








#Followers Twitter Timeline
TSLAFollowerTweets.df


#Procesar Texto de twitter timeline de Followers






