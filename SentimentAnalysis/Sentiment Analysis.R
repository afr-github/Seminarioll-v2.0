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

#Union entre los movimientos de la bolsa de valores y el sentimiento
Resultados <- function(rango){
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
  
  SP <- data.frame(Fecha = as.Date(c(""), format = "%d/%m/%Y"), Cant = "", stringsAsFactors = FALSE) 
  SN <- data.frame(Fecha = as.Date(c(""), format = "%d/%m/%Y"), Cant = "", stringsAsFactors = FALSE) 
  SNeg <- data.frame(Fecha = as.Date(c(""), format = "%d/%m/%Y"), Cant = "", stringsAsFactors = FALSE) 
  
  #identificación de dias positivos, neutros o negativos
  for(i in 1:length(TSLAStockData.TEMP$Date)){
    if(length(rango$Sentiment[TSLAStockData.TEMP$Date[i] == rango$created_at]) > 0){
      val <- (rango$Sentiment[TSLAStockData.TEMP$Date[i] == rango$created_at] == "positive")
      SP <- add_row(SP, Fecha = unique(rango$created_at[TSLAStockData.TEMP$Date[i] == rango$created_at]), Cant = length(val[val == TRUE]))
      
      val2 <- (rango$Sentiment[TSLAStockData.TEMP$Date[i] == rango$created_at] == "neutral")
      SN <- add_row(SN, Fecha = unique(rango$created_at[TSLAStockData.TEMP$Date[i] == rango$created_at]), Cant = length(val2[val2 == TRUE]))
      
      val3 <- (rango$Sentiment[TSLAStockData.TEMP$Date[i] == rango$created_at] == "negative")
      SNeg <- add_row(SNeg, Fecha = unique(rango$created_at[TSLAStockData.TEMP$Date[i] == rango$created_at]), Cant = length(val3[val3 == TRUE]))
    }
  }
  
  SP <- SP[2:length(SP$Cant),]
  SN <- SN[2:length(SN$Cant),]
  SNeg <- SNeg[2:length(SNeg$Cant),]
  
  #Data frame con las fechas y resultados
  TSLAStockData.TEMP.S <- data.frame(
    Fecha = c(SP$Fecha),
    Sentimiento_Positivo = c(SP$Cant),
    Sentimiento_Neutro = c(SN$Cant),
    Sentimiento_Negativo = c(SNeg$Cant),
    stringsAsFactors = FALSE
  )
  
  #Union de los datos recopilados por 
  names(TSLAStockData.TEMP) <- c("Fecha","Open","High","Low","Close", "Adj.Close", "Volume")
  
  TSLAStockData.TEMP <- left_join(
    x = TSLAStockData.TEMP, 
    y = TSLAStockData.TEMP.S,
    by = c("Fecha" = "Fecha")
  )
  
  return(TSLAStockData.TEMP)
}

#Tomando en cuenta todos los dias
Analysis.C <- function(){
  TSLATwitterTimeline.df.SA.r$Sentimiento_Neutro <- as.numeric( TSLATwitterTimeline.df.SA.r$Sentimiento_Neutro)
  TSLATwitterTimeline.df.SA.r$Sentimiento_Negativo <- as.numeric(TSLATwitterTimeline.df.SA.r$Sentimiento_Negativo)
  
  TSLATwitterTimeline.df.SA.r.OC <- add_column(
    .data = TSLATwitterTimeline.df.SA.r,
    "Neutro+Negtivo" = TSLATwitterTimeline.df.SA.r$Sentimiento_Neutro+TSLATwitterTimeline.df.SA.r$Sentimiento_Negativo,
    .after = 10
  )
  
  TSLATwitterTimeline.df.SA.r.OC <- subset(
    x = TSLATwitterTimeline.df.SA.r.OC,
    subset = TSLATwitterTimeline.df.SA.r.OC$Open > TSLATwitterTimeline.df.SA.r.OC$Close,
    select = c("Fecha", "Open", "Close", "Sentimiento_Positivo", "Neutro+Negtivo")
  )

  TSLATwitterTimeline.df.SA.r.OC.E <- ((TSLATwitterTimeline.df.SA.r.OC$Open > TSLATwitterTimeline.df.SA.r.OC$Close) & (TSLATwitterTimeline.df.SA.r.OC$Sentimiento_Positivo <= TSLATwitterTimeline.df.SA.r.OC$`Neutro+Negtivo`))
  
  registro <- 1
  countT.temp <- 0
  countF.temp <- 0
  
  while(registro <= length(TSLATwitterTimeline.df.SA.r.OC$Fecha) ){
    ifelse(
      test = TSLATwitterTimeline.df.SA.r.OC.E[registro] == TRUE,
      yes = countT.temp <- countT.temp +1,
      no = if(TSLATwitterTimeline.df.SA.r.OC.E[registro] == FALSE){
        countF.temp <- countF.temp + 1
      }
    )
    registro <- registro + 1
  }
  
  #628
  countT.temp2 <- countT.temp/length(TSLATwitterTimeline.df.SA.r.OC$Fecha)
  #.3964%
  
  countF.temp2 <- countF.temp/length(TSLATwitterTimeline.df.SA.r.OC$Fecha)
  #.25%
  
  countT.temp2
  countF.temp2
  return(c(countT.temp2,countF.temp2))
}

#Tomando en cuenta los dias que se publican
Analysis.R <- function(){
  TSLATwitterTimeline.df.SA.r$Sentimiento_Neutro <- as.numeric( TSLATwitterTimeline.df.SA.r$Sentimiento_Neutro)
  TSLATwitterTimeline.df.SA.r$Sentimiento_Negativo <- as.numeric(TSLATwitterTimeline.df.SA.r$Sentimiento_Negativo)
  TSLATwitterTimeline.df.SA.r$Sentimiento_Positivo <- as.numeric(TSLATwitterTimeline.df.SA.r$Sentimiento_Positivo)
  
  TSLATwitterTimeline.df.SA.r.OC <- add_column(
    .data = TSLATwitterTimeline.df.SA.r,
    "Neutro+Negtivo" = TSLATwitterTimeline.df.SA.r$Sentimiento_Neutro+TSLATwitterTimeline.df.SA.r$Sentimiento_Negativo,
    .after = 10
  )
  
  TSLATwitterTimeline.df.SA.r.OC <- subset(
    x = TSLATwitterTimeline.df.SA.r.OC,
    subset = TSLATwitterTimeline.df.SA.r.OC$Open > TSLATwitterTimeline.df.SA.r.OC$Close,
    select = c("Fecha", "Open", "Close", "Sentimiento_Positivo", "Neutro+Negtivo")
  )
  
  TSLATwitterTimeline.df.SA.r.OC.E <- ((TSLATwitterTimeline.df.SA.r.OC$Open > TSLATwitterTimeline.df.SA.r.OC$Close) & (TSLATwitterTimeline.df.SA.r.OC$Sentimiento_Positivo <= TSLATwitterTimeline.df.SA.r.OC$`Neutro+Negtivo`))
  
  registro <- 1
  countT.temp <- 0
  countF.temp <- 0
  
  while(registro <= length(TSLATwitterTimeline.df.SA.r.OC$Sentimiento_Positivo)){
    ifelse(
      test = TSLATwitterTimeline.df.SA.r.OC.E[registro] == TRUE,
      yes = countT.temp <- countT.temp +1,
      no = if(TSLATwitterTimeline.df.SA.r.OC.E[registro] == FALSE){
        countF.temp <- countF.temp + 1
      }
    )
    registro <- registro + 1
  }
  
  total <- countT.temp + countF.temp #406
  
  countT.temp2 <- countT.temp/total #.6133%
  countF.temp2 <- countF.temp/total #.3866%
  
  return(c(countT.temp2, countF.temp2))
}



#Agregar el sentiemint que expresan los followers y ver en que dias compraten con los que existen en la bolsa de valores y 
#como resulta la tabla 

#Followers Twitter Timeline
TSLAFollowerTweets.df






