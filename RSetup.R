#Rsetup file

#### Checar si existe .Rprofile documento ####
if(file.exists(".Rprofile")){
  #Leer archvio RprofileSetup.txt, transcribir lo a .Rprofile y resetear 
  #para que tomen efecto los cambios realizados.
  if(file.exists("RprofileSetup.txt")){
    source("~/RprofileSetup.txt")
    cat("Archivo ya fue creado/cargado\n")
    file.copy(from = "RprofileSetup.txt", to = ".Rprofile", overwrite = TRUE)
    cat("File read\n")
  }else{
    file.choose()
    cat("Archivo creado\n")
    file.copy(from = "RprofileSetup.txt", to = ".Rprofile", overwrite = TRUE)
    cat("Archivo leido")
  }
}else{
  cat("Creando Archivo.\n")
  file.create(file.path(getwd(), ".Rprofile"))
  cat("Archivo creado.\n")
  if(file.exists("RprofileSetup.txt")){
    source("~/RprofileSetup.txt")
    cat("Archivo ya fue creado/cargado\n")
    file.copy(from = "RprofileSetup.txt", to = ".Rprofile", overwrite = TRUE)
    cat("Archivo leido\n")
  }else{
    file.choose()
    cat("Archivo obtenido\n")
    file.copy(from = "RprofileSetup.txt", to = ".Rprofile", overwrite = TRUE)
    cat("Archivo leido")
  }
}

#Reiniciar session

#### Cargar librerias requeridas ####
cargarLibrerias <- function(){
  paqueteriasRequeridas <- c("twitteR", "rtweet", "influenceR", 
                             "igraph", "SentimentAnalysis", 
                             "syuzhet", "quanteda", "devtools", "remotes",
                             "tweetbotornot")
  
  tryCatch({
      for(i in 1:length(paqueteriasRequeridas)){
        
        #check if the paqueteria is installed not loaded, it won't be loaded
        if(require(paqueteriasRequeridas[i],character.only = TRUE)){
          cat(sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] ha sido cargado.\n"))
        }else{
          cat(sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] no esta cargado.\n"))
          install.packages(paqueteriasRequeridas[i])
          require(paqueteriasRequeridas[i],character.only = TRUE)
          cat(sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] ha sido cargado.\n"))
        }
      }
    },
    error = function(cond){
      ## install tweetbotornot from github
      devtools::install_github("mkearney/tweetbotornot",force = TRUE)
      
      # To fix `by` [ERROR] with newer version of textfeatures
      devtools::install_version('textfeatures', version='0.2.0', repos='http://cran.us.r-project.org')
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








