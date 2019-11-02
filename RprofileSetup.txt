.First <- function(){
  cat("Bienvenido\n")
  
  #Cargando todas las paqueterias
  if(file.exists(".Rprofile")){
    source(".Rprofile")
    cargarlibrerias()
  }
  
  #function in file
  
  
} 