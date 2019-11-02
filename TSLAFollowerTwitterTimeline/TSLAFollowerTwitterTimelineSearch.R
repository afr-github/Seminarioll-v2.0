#TSLAFollowerTwitterTimelineSearch

#### Se realiza la busqueda de los followers ####
TSLAFollowerTwitterTimeline.Search <- function(){
  #Se busca los usuarios
  TSLAFollowers <- twitteR::getUser(
    user = "Tesla"
  )
  
  #Iniciacion de lista
  if(exists("TSLAFollowers.list")){
    #Busqueda de los followers
    TSLAFollowers.list <- append(
      x = TSLAFollowers$getFollowers(n = 30000),
      TSLAFollowers.list,
      after = 1
    )
  } else{
    TSLAFollowers.list <- c("")
    #Busqueda de los followers
    TSLAFollowers.list <- append(
      x = TSLAFollowers$getFollowers(n = 30000),
      TSLAFollowers.list,
      after = 1
    )
  }
  
  #Conversion de twitter object a data.frame
  TSLAFollowers.df <- twitteR::twListToDF(TSLAFollowers.list)
  
  write.csv(
    x = TSLAFollowers.df,
    file = "TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearchData.csv"
  )
  
}

##### Se cargan los Followers encontrados ####
TSLAFollowerTwitterTimeline.Load <- function(){
  TSLAFollowerTwitterTimeline.df <- read.csv(
    file = "TSLAFollowerTwitterTimeline/TSLAFollowerTwitterTimelineSearchData.csv",
    stringsAsFactors = FALSE
  )
  return(TSLAFollowerTwitterTimeline.df)
}

#### Se seleccionan los usuarios con al menos 15000 statuses (tweets) ####
TSLAFollowerSelect.Clean <- function(x){
  TSLATFollowerTwitterTimeline.df <- x
  #Eliminar duplicados de la lista
  TSLATFollowerTwitterTimeline.df <- TSLATFollowerTwitterTimeline.df[!duplicated(TSLATFollowerTwitterTimeline.df$id),]
  #Los Followers tiene que tener mas de 1 status (tweet)
  TSLATFollowerTwitterTimeline.df <- subset(
    x = TSLATFollowerTwitterTimeline.df,
    subset = statusesCount > 1 & followersCount > 1
  )
  #Se seleccionan los usuarios que tengan mas de 15000 statuses (tweets)
  TSLATFollowerTwitterTimeline.df <- subset(
    x = TSLATFollowerTwitterTimeline.df,
    subset = statusesCount >= 15000
  )
  
  return(TSLATFollowerTwitterTimeline.df)
}

#### Se busca cuales Followers son Bots ####
TSLAFollowerBots.Search <- function(x){
  TSLATFollowerTwitterTimeline.df <- x
  
  for(i in 1:length(TSLATFollowerSelect.df$screenName)){
      if(exists("TSLAFollowerBots.df")){
        TSLAFollowerBots.df <- tweetbotornot::tweetbotornot(
          x = TSLATFollowerTwitterTimeline.df$screenName[i,i+150]
        )
        i <- (i+150)
        Sys.sleep(16*60)
      } else{
        ##Append what already exists into the continuance of the same variable to able 
        # create the file at the end.
      }
  }


  
}









