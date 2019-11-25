#Rsetup file

#### Cargar librerias requeridas ####
cargarLibrerias <- function(){
  paqueteriasRequeridas <- c("twitteR", "rtweet", "influenceR", 
                             "igraph", "SentimentAnalysis", 
                             "syuzhet", "quanteda", "devtools", "remotes",
                             "tweetbotornot", "tm", "tidytext", "dplyr",
                             "ggplot2", "tibble")
  
  tryCatch({
    for(i in 1:length(paqueteriasRequeridas)){
      
      #check if the paqueteria is installed not loaded, it won't be loaded
      if(require(paqueteriasRequeridas[i],character.only = TRUE)){
        cat(sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] ha sido cargado.\n"))
      }else{
        cat(sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] no esta cargado.\n"))
        install.packages(paqueteriasRequeridas[12])
        require(paqueteriasRequeridas[i],character.only = TRUE)
        cat(sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] ha sido cargado.\n"))
      }
    }
  }
  )
}
cargarLibrerias()

#### Cargar conexion con twitter ####
cargarConexion <- function(){
  
  #Llaves
  apiKey <- '1RWlYtAbUBTXv4j0i4CW4BV17'
  apiSecret <- 'dWDUEPiCLLj87Xo5YApiq9s5BrxE54q66TROf60hsC0tCuZBQn'
  accessToken <- '1099884980451328001-WMfdGjHsqNwbhc4ALULCovkLXYDmrp'
  accessTokenSecret <- '1ROVnec1VWW0JUp9GD0VhLejdsIVjVZPjOA0pRU89TLPW'
  
  #Conexion para la libreria twitte
  setup_twitter_oauth(apiKey, apiSecret, accessToken, accessTokenSecret)
  #Seleccionar 1
  
  #rtweet conection
  if(!exists("token")){
    token <- create_token(
      app = "Proyect_SeminarioDeTitulacion",
      consumer_key = apiKey,
      consumer_secret = apiSecret,
      access_token = accessToken,
      access_secret = accessTokenSecret
    )
  } else{
    #rtweet access when token already generated
    get_token()
  }
  
}
cargarConexion()

#### TSLATwitterTimeline ####
#Se carga el archivo con los tweets de Tesla Motors Inc.
TSLATwitterTimeline <- function(){
  if(file.exists("TSLATwitterTimeline/TSLATwitterTimelineSearch.R")){
    source("TSLATwitterTimeline/TSLATwitterTimelineSearch.R")
    TSLATwitterTimeline.df <- TSLATwitterTimeline.Load()
    TSLAInfo.Search()
    TSLAInfo.Write()
  }
  return(TSLATwitterTimeline.df)
}
#View(TSLAInfo.Load())

TSLATwitterTimeline.df <- TSLATwitterTimeline()
#View(TSLATwitterTimeline.df)

#### SentimentAnalysis Tesla (Procesamiento) ####
TSLASentimentAnalysis.ProcesarCorpus <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")
    TSLATwitterTimeline.df.SA.PC <- TSLASentimentAnalysis.Procesar(
      dfText = TSLATwitterTimeline.df
    )
  }
  return(TSLATwitterTimeline.df.SA.PC)
}
TSLATwitterTimeline.df.SA.PC <- TSLASentimentAnalysis.ProcesarCorpus()

#### SentimentAnalysis Tesla (Transformación) ####
TSLASentimentAnalysis.TransformarCorpus <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")
    TSLATwitterTimeline.df.SA.TC <- TSLASentimentAnalysis.Transformar(
      xCorpus = TSLATwitterTimeline.df.SA.PC
    )
  }
  return(TSLATwitterTimeline.df.SA.TC)
}

TSLATwitterTimeline.df.SA.TC <- TSLASentimentAnalysis.TransformarCorpus()
TSLATwitterTimeline.df.SA.df <- TSLASentimentAnalysis.TransformarCorpus()
#### Inyectar TSLATwitterTimeline.df.SA.TC en TSLATwitterTimeline.df$text ####
TSLATwitterTimeline.df$text <- TSLATwitterTimeline.df.SA.TC$text

#### SentimentAnalysis Tesla ####
TSLASentimentAnalysis <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")
    TSLATwitterTimeline.df.SA.df <- TSLASentimentAnalysis.SentimentAnalysis.s(
      xCorpus = TSLATwitterTimeline.df.SA.PC
    )
  }
  return(TSLATwitterTimeline.df.SA.df)
}
TSLATwitterTimeline.df.SA.df <- TSLASentimentAnalysis()
#View(TSLATwitterTimeline.df.SA.df)

#### SentimentAnalyis Tesla Union Sentiment ####
TSLATwitterTimeline.df.SA.union <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")  
    TSLATwitterTimeline.df.SA.U <- UnionTablas()
  }
  
  return(TSLATwitterTimeline.df.SA.U)
}
TSLATwitterTimeline.df.SA.U <- TSLATwitterTimeline.df.SA.union()

#### SentimentAnalysis Tesla Top Words ####
TSLASentimentAnalysis.top <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")
    TSLATwitterTimeline.df.SA.df <- TSLASentimentAnalysis.SentimentAnalysis.top(
      xDataFrame = TSLATwitterTimeline.df,
      cant = 10 #Recomended 20
    )
  }
  return(TSLATwitterTimeline.df.SA.df)
}
TSLASentimentAnalysis.top()

#### SentimentAnalysis Tesla Identificación Mensual ####
TSLATwitterTimeline.df.SA.imensual <- function(fecha1, fecha2){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")  
    TSLATwitterTimeline.df.SA.im <- IdentificacionMensual(fecha1 = fecha1, fecha2 = fecha2)
  }
  return(TSLATwitterTimeline.df.SA.im)
}

#Rango de fechas permitidas
#01/06/2014 - 05/31/2019
TSLATwitterTimeline.df.SA.im <- TSLATwitterTimeline.df.SA.imensual("01/06/2014", "31/05/2019")
View(TSLATwitterTimeline.df.SA.im)

#### SentimentAnalysis Tesla Resultado ####
TSLATwitterTimeline.df.SA.resultdo <- function(rango){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")  
    TSLATwitterTimeline.df.SA.r <- Resultados(rango = rango)
  }
  return(TSLATwitterTimeline.df.SA.r)
}
TSLATwitterTimeline.df.SA.r <- TSLATwitterTimeline.df.SA.resultdo(TSLATwitterTimeline.df.SA.im)
View(TSLATwitterTimeline.df.SA.r)

write.csv(
  x = TSLATwitterTimeline.df.SA.r,
  file = "TSLATwitterTimeline/TSLATwitterTimeline.df.SA.r.csv"
)











##### No es requerido ####
#Cuando el precio de cierre termina menor que el precio de apertura existen mas sentimientos negativos y neutros que positivos *628 registos
TSLATwitterTimeline.df.SA.AnalysisCompleto <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")  
    TSLATwitterTimeline.df.SA.AC <- Analysis.C()
  }
  return(TSLATwitterTimeline.df.SA.AC)
}
TSLATwitterTimeline.df.SA.AC <- TSLATwitterTimeline.df.SA.AnalysisCompleto()

TSLATwitterTimeline.df.SA.AnalysisReal <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")  
    TSLATwitterTimeline.df.SA.AR <- Analysis.R()
  }
  return(TSLATwitterTimeline.df.SA.AR)
}
TSLATwitterTimeline.df.SA.AR <- TSLATwitterTimeline.df.SA.AnalysisReal()
View(TSLATwitterTimeline.df.SA.AR)