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

#Identificación del rango de fecha en que se esta interesado
IdentificacionMensual <- function(fecha1, fecha2){
  TSLATwitterTimeline.df.SA.U.TEMP <- TSLATwitterTimeline.df.SA.U
  
  TSLATwitterTimeline.df.SA.U.TEMP$created_at <- as.Date(
    x = TSLATwitterTimeline.df.SA.U$created_at,
    optional = FALSE
  )
  
  TSLATwitterTimeline.df.SA.U.TEMP <- data.frame(
    subset(
      x = TSLATwitterTimeline.df.SA.U.TEMP,
      subset = (TSLATwitterTimeline.df.SA.U.TEMP$created_at > as.Date(fecha1, tryFormat = "%d/%m/%Y")) &
               (TSLATwitterTimeline.df.SA.U.TEMP$created_at <= as.Date(fecha2, tryFormat = "%d/%m/%Y"))
    ),
    stringsAsFactors = FALSE
  )

  return(TSLATwitterTimeline.df.SA.U.TEMP)
}


Resultados <- function(rango){
  rango <- TSLATwitterTimeline.df.SA.im
  
  TSLAStockData.TEMP <- read.csv(
    file = "TSLAStockData/TSLAStockData.csv",
    stringsAsFactors = FALSE
  )
  
  TSLAStockData.TEMP <- TSLAStockData.TEMP[,1:7]
  
  fecha1 <- min(rango$created_at)
  fecha2 <- max(rango$created_at)
  
  TSLAStockData.TEMP$Date <- as.Date(TSLAStockData.TEMP$Date, tryFormats = "%d/%m/%Y")
  fecha1 <- as.Date(fecha1, tryFormats = "%Y/%m/%d")
  fecha2 <- as.Date(fecha2, tryFormats = "%d/%m/%Y")
  
  TSLAStockData.TEMP <- subset(
    x = TSLAStockData.TEMP,
    subset = (TSLAStockData.TEMP$Date > as.Date(fecha1, tryFormats = "%d/%m/%Y")) &
             (TSLAStockData.TEMP$Date <= as.Date(fecha2, tryFormats = "%d/%m/%Y"))
  )
  
  for(i in 1:length(TSLAStockData.TEMP$Date)){
    if(length(rango$created_at == TSLAStockData.TEMP$Date[i]) > 0){
      #buscar por dia la cantidad de cada tipo de sentimiento que tiene
    }else{
      
    }else{
      
    } 
  }
  
  
  
  TSLAStockData.TEMP.fechas <- as.character(
    x =  c(
      TSLAStockData.TEMP$Date,
      "Sentimiento Positivo" = SP,
      "Sentimiento Neutro" = SN,
      "Sentimiento Negativo" = SNeg
    )
  )
  
  
  View(TSLAStockData.TEMP.fechas)
  
  View(rango)
  View(TSLAStockData.TEMP)
  
  #unir los diferentes sentimientos que existen en la tabla por fechas ej. 2014/06/09 : 5 positivo, 3 neutro, 2 negativo
  #comparar con precio de las acciones - 2014/06/09: open 229.4 - close - 232.3
  
  
  
  #No coicide la cantidad de filas [mas filas de rango$sentiminet]
  TSLAStockData.TEMP <- add_column(
    .data = TSLAStockData.TEMP,
    rango$Sentiment,
    .after = 7
  )
  
}

#por que no medir el sentimiento con los mismos parametros por el mes en el que estas interesado, envez de crear tantas variables.
#la variables [meses] esta bien, pero hay que empezar a analizar el texto para ver el sentimiento de cada mes y ver como se relacióna
#con los movimientos en la bolsa de valores.



#hacer una agrupación por meses los movimientos y comparar los
#movimiento en la bolsa de valores.








#Followers Twitter Timeline
TSLAFollowerTweets.df






