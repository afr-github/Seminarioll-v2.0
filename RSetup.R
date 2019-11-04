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
  }
  return(TSLATwitterTimeline.df)
}
TSLATwitterTimeline.df <- TSLATwitterTimeline()

#### TSLAFollowerTwitterTimeline ####
#Se carga el archvivo con los Followers de Tesla
TSLAFollowerTwitterTimeline <- function(){
  if(file.exists("TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearch.R")){
    source("TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearch.R")
    TSLATFollowerTwitterTimeline.df <- TSLAFollowerTwitterTimeline.Load()
  }
  return(TSLATFollowerTwitterTimeline.df)
}
TSLATFollowerTwitterTimeline.df <- TSLAFollowerTwitterTimeline()

#### TSLAFollowerSelect ####
#Se seleccionan los usuarios que tengan almenos 15000 statuses (tweets) 
TSLAFollowerSelect <- function(){
  if(file.exists("TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearch.R")){
    source("TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearch.R")
    TSLATFollowerSelect.df <- TSLAFollowerSelect.Clean(TSLATFollowerTwitterTimeline.df)
  }
  return(TSLATFollowerSelect.df)
}
TSLATFollowerSelect.df <- TSLAFollowerSelect()

#### TSLAFollowerBots ####
#Se eliminan los usuario que tienen mas de .5% de probabilidad de ser bots
TSLAFollowerBot <- function(){
  if(file.exists("TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearch.R")){
    source("TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearch.R")
    TSLAFollowerBots.df <- TSLAFollowerBots.Select()  
  }
  return(TSLAFollowerBots.df)
}
TSLAFollowerBots.df <- TSLAFollowerBot()

#### TSLAFollowersTweets ####
#Todos los tweets de los Followers que cumplieron con los requisitos
TSLAFollowerTweets <- function(){
  if(file.exists("TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearch.R")){
    source("TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearch.R")
    TSLAFollowerTweets.df <- TSLAFollowerTweets.Load()
  }
  return(TSLAFollowerTweets.df[4:length(TSLAFollowerTweets.df)])
}
TSLAFollowerTweets.df <- TSLAFollowerTweets()

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

#### SentimentAnalyis Tesla Union Sentiment ####
TSLATwitterTimeline.df.SA.union <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")  
      TSLATwitterTimeline.df.SA.U <- UnionTablas()
  }
  return(TSLATwitterTimeline.df.SA.U)
}
TSLATwitterTimeline.df.SA.U <- TSLATwitterTimeline.df.SA.union()

#### SentimentAnalysis Tesla Identificación Mensual ####
TSLATwitterTimeline.df.SA.imensual <- function(){
  if(file.exists("SentimentAnalysis/Sentiment Analysis.R")){
    source("SentimentAnalysis/Sentiment Analysis.R")  
    TSLATwitterTimeline.df.SA.im <- IdentificacionMensual()
  }
  return(TSLATwitterTimeline.df.SA.im)
}
TSLATwitterTimeline.df.SA.im <- TSLATwitterTimeline.df.SA.imensual()














#### SentimentAnalysis Followers ####
TSLAFollowersSentimentAnalysis <- function(){
  
}

