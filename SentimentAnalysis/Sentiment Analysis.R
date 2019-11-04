#Sentiment Analisis 

#Tesla Twitter Timeline
#Aqui se realiza todo el proceso del procesado del texto
TSLASentimentAnalysis.Procesar <- function(dfText){
  TSLATwitterTimeline.df.SA.corpus <- VCorpus(VectorSource(dfText$text[1:length(dfText$text)]))
  
  #Todas las palabras a minisculas
  TSLATwitterTimeline.df.SA.corpus <- tm_map(
    x = TSLATwitterTimeline.df.SA.corpus,
    FUN = content_transformer(tolower)
  )
  
  #Remover toda las puntuaciónes 
  TSLATwitterTimeline.df.SA.corpus <- tm_map(
    x = TSLATwitterTimeline.df.SA.corpus,
    FUN = removePunctuation
  )
  
  #Remover las URL's 
  removeURL <- function(x){gsub('http\\S+\\s*', '', x)}
  TSLATwitterTimeline.df.SA.corpus <- tm_map(
    x = TSLATwitterTimeline.df.SA.corpus,
    FUN = content_transformer(removeURL)
  )
  
  #Remover los RT's
  removeRT <- function(x){gsub('rt', '' , x)}
  TSLATwitterTimeline.df.SA.corpus <- tm_map(
    x = TSLATwitterTimeline.df.SA.corpus,
    FUN = content_transformer(removeRT)
  )
  
  #Remover &'s
  removeAmps <- function(x){gsub('amp', '', x)}
  TSLATwitterTimeline.df.SA.corpus <- tm_map(
    x = TSLATwitterTimeline.df.SA.corpus,
    FUN = content_transformer(removeAmps)
  )
  
  #Remover palabras que no tengan utilidad del contexto
  TSLATwitterTimeline.df.SA.corpus <- tm_map(
    x = TSLATwitterTimeline.df.SA.corpus,
    FUN = removeWords,
    stopwords("english")
  )
  
  #Simplificar palabras similares (Stem)
  TSLATwitterTimeline.df.SA.corpus <- tm_map(
    x = TSLATwitterTimeline.df.SA.corpus,
    FUN = stemDocument
  )
  
  #Remover los espacios blancos
  TSLATwitterTimeline.df.SA.corpus <- tm_map(
    x = TSLATwitterTimeline.df.SA.corpus,
    FUN = stripWhitespace
  )
  
  return(TSLATwitterTimeline.df.SA.corpus)
} 

#Aqui se convierte el corpus en un data.frame
TSLASentimentAnalysis.Transformar <- function(xCorpus){
  TSLATwitterTimeline.df.SA.df <- data.frame(
    text = sapply(
      X = xCorpus,
      FUN = as.character
    ),
    stringsAsFactors = FALSE
  )
  
  return(TSLATwitterTimeline.df.SA.df)
}

#Aqui se realiza el proceso de analizar el sentimiento
TSLASentimentAnalysis.SentimentAnalysis.s <- function(xCorpus){
  TSLASentimentAnalysis.corpus <- SentimentAnalysis::analyzeSentiment(xCorpus[1:length(xCorpus)])
  
  TSLASentimentAnalysis.corpus <- subset(
    x = convertToDirection(
      sentiment = TSLASentimentAnalysis.corpus
    ),
    select = c("WordCount", "SentimentGI", "PositivityLM", "NegativityLM", "RatioUncertaintyLM")
  )
  
  #Muestra los el sentimiento del texto
  return(TSLASentimentAnalysis.corpus)
}

#Aqui se se pueden ver las palabras mas utilizadas dentro de los tweets
TSLASentimentAnalysis.SentimentAnalysis.top <- function(xDataFrame, cant){
  TSLASentimentAnalysis.words <- xDataFrame[,c("created_at", "text")] %>%
    tidytext::unnest_tokens("words", text)
  
  data("stop_words")
  
  TSLASentimentAnalysis.words.top <- TSLASentimentAnalysis.words %>%
    dplyr::count(words) %>%
    dplyr::arrange(dplyr::desc(n))
  
  TSLASentimentAnalysis.words.topCant <- TSLASentimentAnalysis.words.top[1:cant,]
  
  TSLASentimentAnalysis.words.top$words <- factor(
    x = TSLASentimentAnalysis.words.top$words,
    levels = TSLASentimentAnalysis.words.top$words[
      order(
        TSLASentimentAnalysis.words.top$n,
        decreasing = TRUE
      )
      ]
  )
  
  return(ggplot2::ggplot(data = TSLASentimentAnalysis.words.topCant,
                         mapping = ggplot2::aes(x = words, y = n)
  ) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Cantidad de veces que aparecen palabras") +
    xlab("") +
    guides(fill = FALSE))
  
}

#Union de Sentimeinto y TSLATwitterTimeline.df 
UnionTablas <- function(){
  TSLATwitterTimeline.df.SA.U <- TSLATwitterTimeline.df
  
  TSLATwitterTimeline.df.SA.U <- add_column(
    .data = TSLATwitterTimeline.df.SA.U,
    TSLATwitterTimeline.df.SA.df$SentimentGI,
    .before = 5
  )
  
  names(TSLATwitterTimeline.df.SA.U)[5] <- "Sentiment"
  
  return(TSLATwitterTimeline.df.SA.U)
}



#Hasta aqui
IdentificacionMensual <- function(){
  meses <- as.Date(
    c("2014/06/01", "2014/07/01", "2014/08/01", "2014/09/01", "2014/10/01", "2014/11/01", "2014/12/01", "2015/01/01", "2015/02/01", "2015/03/01", "2015/04/01",
      "2015/05/01", "2015/06/01", "2015/07/01", "2015/08/01", "2015/09/01", "2015/10/01", "2015/11/01", "2015/12/01", "2016/01/01", "2016/02/01", "2016/03/01",
      "2016/04/01", "2016/05/01", "2016/06/01", "2016/07/01", "2016/08/01", "2016/09/01", "2016/10/01", "2016/11/01", "2016/12/01", "2017/01/01", "2017/02/01",
      "2017/03/01", "2017/04/01", "2017/05/01", "2017/06/01", "2017/07/01", "2017/08/01", "2017/09/01", "2017/10/01", "2017/11/01", "2017/12/01", "2018/01/01",
      "2018/02/01", "2018/03/01", "2018/04/01", "2018/05/01", "2018/06/01", "2018/07/01", "2018/08/01", "2018/09/01", "2018/10/01", "2018/11/01", "2018/12/01",
      "2019/01/01", "2019/02/01", "2019/03/01", "2019/04/01", "2019/05/01", "2019/06/01"
    )
  )
  
  TSLATwitterTimeline.df.SA.U.TEMP <- TSLATwitterTimeline.df.SA.U
  
  TSLATwitterTimeline.df.SA.U.TEMP$created_at <- as.Date(
    x = TSLATwitterTimeline.df.SA.U$created_at,
    optional = FALSE
  )
  #2014/06/01
  fecha20140601 <- data.frame(
    text = sapply(
      X = subset(
        x = TSLATwitterTimeline.df.SA.U.TEMP,
        subset = (TSLATwitterTimeline.df.SA.U.TEMP$created_at > meses[1]) &
          (TSLATwitterTimeline.df.SA.U.TEMP$created_at <= meses[2])
      ),
      FUN = as.character
    ),
    stringsAsFactors = FALSE
  )
  #2014/07/01
  fecha20140701 <- data.frame(
    text = sapply(
      X = subset(
        x = TSLATwitterTimeline.df.SA.U.TEMP,
        subset = (TSLATwitterTimeline.df.SA.U.TEMP$created_at > meses[2]) &
          (TSLATwitterTimeline.df.SA.U.TEMP$created_at <= meses[3])
      ),
      FUN = as.character
    ),
    stringsAsFactors = FALSE
  )
  #2014/08/01
  fecha20140801 <- data.frame(
    text = sapply(
      X = subset(
        x = TSLATwitterTimeline.df.SA.U.TEMP,
        subset = (TSLATwitterTimeline.df.SA.U.TEMP$created_at > meses[3]) &
          (TSLATwitterTimeline.df.SA.U.TEMP$created_at <= meses[4])
      ),
      FUN = as.character
    ),
    stringsAsFactors = FALSE
    
    #por que no medir el sentimiento con los mismos parametros por el mes en el que estas interesado, envez de crear tantas variables.
    #la variables [meses] esta bien, pero hay que empezar a analizar el texto para ver el sentimiento de cada mes y ver como se relacióna
    #con los movimientos en la bolsa de valores.
  )
  
}



#hacer una agrupación por meses los movimientos y comparar los
#movimiento en la bolsa de valores.








#Followers Twitter Timeline
TSLAFollowerTweets.df






