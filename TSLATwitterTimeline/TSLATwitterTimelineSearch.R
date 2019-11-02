#Tesla Twitter Timeline search

#Busqueda en twitter de 3200 tweets publicados por Tesla
#Creación de archivo en formato utilizable
write.csv(
  x = twListToDF(
    userTimeline(
      user = "Tesla", n = 3200, sinceID = '2014-06-01', includeRts = TRUE, 
      excludeReplies = FALSE
    )
  ),
  file = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv"
)

#Tweets utiles
TSLATwitterTimelineSearch.f <- function(){
  temp <- read.csv(
    file = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv",
    stringsAsFactors = FALSE,
    header = TRUE,
    sep = ","
  )
  
  #Columnas ajustadas
  temp <- temp[2:length(temp)]
  temp$created <- as.Date.character(x = temp$created, tryformat = "%d/%m/%Y")
  temp <- subset(
    x = temp,
    subset = (created >= as.Date("2014-06-01", tryformat = "%d/%m/%Y")) &
             (created  < as.Date("2019-06-01", tryformat = "%d/%m/%Y"))
  )
  
  write.csv(
    x = temp,
    file = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv"
  )
  
}

