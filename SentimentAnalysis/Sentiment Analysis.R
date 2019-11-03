#Sentiment Analisis 

#Tesla Twitter Timeline
#Aqui se realiza todo el proceso del procesado del texto
TSLASentimentAnalysis.Procesar <- function(){
  TSLATwitterTimeline.df.SA <- SentimentAnalysis::preprocessCorpus(
    corpus = VCorpus(VectorSource(TSLATwitterTimeline.df$text[1:1500])),
    language = "english",
    stemming = TRUE,
    removeStopwords = TRUE
  )
  
  #Todas las palabras a minisculas
  TSLATwitterTimeline.df.SA <- tm_map(
    x = TSLATwitterTimeline.df.SA,
    FUN = content_transformer(tolower)
  )
  
  #Remover toda las puntuaciónes 
  TSLATwitterTimeline.df.SA <- tm_map(
    x = TSLATwitterTimeline.df.SA,
    FUN = removePunctuation
  )
  
  #Remover las URL's 
  removeURL <- function(x){gsub('http[[:alnum::]]*', '', x)}
  TSLATwitterTimeline.df.SA <- tm_map(
    x = TSLATwitterTimeline.df.SA,
    FUN = content_transformer(removeURL)
  )
  
  #Remover los espacios blancos
  TSLATwitterTimeline.df.SA <- tm_map(
    x = TSLATwitterTimeline.df.SA,
    FUN = stripWhitespace
  )

  return(TSLATwitterTimeline.df.SA)
}

#Aqui se convierte el corpus en un data.frame
TSLASentimentAnalysis.Transformar <- function(corpus){
  #corpus to data.frame
  TSLATwitterTimeline.df.corpus.df <- data.frame(
    text = sapply(
      X = TSLATwitterTimeline.df.corpus,
      FUN = as.character
    ),
    stringsAsFactors = FALSE
  )
  return(TSLATwitterTimeline.df.corpus.df)
}




TSLASentimentAnalysis.SentimentAnalysis <- function(corpus){
  TSLASentimentAnalysis.corpus <- analyzeSentiment(TSLATwitterTimeline.df.corpus)
  TSLASentimentAnalysis.corpus <- subset(
    x = convertToDirection(
      sentiment = TSLASentimentAnalysis.corpus
    ),
    select = c("WordCount", "SentimentGI", "PositivityLM", "NegativityLM", "RatioUncertaintyLM")
  )
  
  #TSLASentimentAnalysis.corpus <- 
  
  
  
  View(TSLASentimentAnalysis.corpus)
  
  return(TSLASentimentAnalysis.corpus)
}







#ERROR TSLATwitterTimeline.df.AS <- analyzeSentiment(TSLATwitterTimeline.df.corpus)

View(TSLATwitterTimeline.df.SA)

#Procesar Texto de twitter timeline de tesla ..








#Followers Twitter Timeline
TSLAFollowerTweets.df


#Procesar Texto de twitter timeline de Followers






