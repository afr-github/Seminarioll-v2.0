#Rsetup file

#Checar si existe .Rprofile documento
revisarRprofile <- function(){
  if(file.exists(".Rprofile")){
    checarRprofileSetup()
  }else{
    cat("Creando Archivo.\n")
    file.create(file.path(getwd(), ".Rprofile"))
    cat("Archivo creado.\n")
    revisarRprofile()
  }
}

#Leer archvio RprofileSetup.txt, transcribir lo a .Rprofile y resetear 
#para que tomen efecto los cambios realizados.
checarRprofileSetup <- function(){
  if(file.exists("RprofileSetup.txt")){
    source("~/RprofileSetup.txt")
    cat("File already created/loaded\n")
    RsetupFile <- readLines(
      paste(getwd(), "/RprofileSetup.txt", sep = ""),
      warn = FALSE)
    cat("File read\n")
    
  }else{
    file.create("RprofileSetup.txt")
    cat("File created\n")
    RsetupFile <- readLines(
      paste(getwd(), "/RprofileSetup.txt", sep = ""),
      warn = FALSE)
    cat("File read\n")
    
  }
}

file.copy(from = "RprofileSetup.txt", to = ".Rprofile", overwrite = TRUE)
.rs.api.restartSession()

#Cargar librerias requeridas
cargarlibrerias <- function(){
  paqueteriasRequeridas <- c("twitteR", "rtweet", "influenceR", "igraph", "SentimentAnalysis", "syuzhet", "quanteda")
  
  tryCatch({
    for(i in 1:length(paqueteriasRequeridas)){
      
      #check if the paqueteria is installed not loaded, it won't be loaded
      if(require(paqueteriasRequeridas[i],character.only = TRUE)){
        cat(sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] ha sido cargado.\n"))
      }else{
        cat(sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] no esta cargado.\n"))
        install.packages(paqueteriasRequeridas[i])
        cargarlibrerias()
      }
    }
  },
  finally = {
    "Librerias cargadas"
  }
  )
}
