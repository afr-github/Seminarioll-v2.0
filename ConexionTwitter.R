#Documento que permite la conexion a twitter
cargarllaves <- function(){
  apiKey <- '1RWlYtAbUBTXv4j0i4CW4BV17'
  apiSecret <- 'dWDUEPiCLLj87Xo5YApiq9s5BrxE54q66TROf60hsC0tCuZBQn'
  accessToken <- '1099884980451328001-WMfdGjHsqNwbhc4ALULCovkLXYDmrp'
  accessTokenSecret <- '1ROVnec1VWW0JUp9GD0VhLejdsIVjVZPjOA0pRU89TLPW'  
}

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