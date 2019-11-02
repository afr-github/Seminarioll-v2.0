#Rsetup file

#Setup .Rprofile Doc

#Leer archvio Rsetup, transcribir lo a .Rprofile y resetear para que tomen
#efecto los cambios realizados.
if(file.exists("~/Rsetup.txt")){
  source("~/Rsetup.txt")
  RsetupFile <- readLines(con = "~/RprofileSetup.txt",warn = FALSE)
  print("File already created/loaded\nFile read")
}else{
  file.create("~/RprofileSetup.txt")
  print("File created")
  
  RsetupFile <- readLines(con = "~/RprofileSetup.txt",warn = FALSE)
  print("File read")
}






#Cargar librerias requeridas
cargarlibrerias <- function(){
  paqueteriasRequeridas <- c("twitteR", "rtweet", "influenceR")
  
  tryCatch({
    for(i in 1:length(paqueteriasRequeridas)){
      if(isNamespaceLoaded(paqueteriasRequeridas[i])){
        require(paqueteriasRequeridas[i])
        sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] ha sido cargado.")
      }else{
        sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] no esta cargado.")
        install.packages(paqueteriasRequeridas[i])
        require(paqueteriasRequeridas[i])
        sub(pattern = "[i]", replacement = paqueteriasRequeridas[i], x = "Paquete [i] ha sido cargado.")
      }
    }
  },
  finally = {
    "Librerias cargadas"
  }
  )
}