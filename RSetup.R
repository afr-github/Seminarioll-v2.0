#Rsetup file

#Checar si existe .Rprofile documento
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
    cat("Archivo creado\n")
    file.copy(from = "RprofileSetup.txt", to = ".Rprofile", overwrite = TRUE)
    cat("Archivo leido")
  }
}

.rs.restartR()

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
    }
  )
}
