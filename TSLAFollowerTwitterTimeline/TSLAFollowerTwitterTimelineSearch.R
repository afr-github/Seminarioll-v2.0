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
TSLAFollowerBots.Search <- function(){
  
  tryCatch(
    {
      i <- 1
      while(i < 151){
        if(exists("TSLAFollowerBots.df")){
          TSLAFollowerBots.df.TEMP <- read.csv(
            file = "TSLAFollowerTwitterTimeline/TSLAFollowerBots.csv"
          )
          TSLAFollowerBots.df <- tweetbotornot::tweetbotornot(
            x = TSLATFollowerTwitterTimeline.df$screenName[i:(i+150)],
            fast = FALSE
          )
          i <- (i+150)
          TSLAFollowerBots.df.TEMP <- rbind(TSLAFollowerBots.df.TEMP,TSLAFollowerBots.df)
          TSLAFollowerBots.df <- TSLAFollowerBots.df.TEMP
          write.csv(
            x = TSLAFollowerBots.df,
            file = "TSLAFollowerTwitterTimeline/TSLAFollowerBots.csv"
          )
          #Sys.sleep(16*60)
        }else{
          TSLAFollowerBots.df <- tweetbotornot::tweetbotornot(
            x = TSLATFollowerTwitterTimeline.df$screenName[i:(i+150)],
            fast = FALSE
          )
          i <- (i+150)
          write.csv(
            x = TSLAFollowerBots.df,
            file = "TSLAFollowerTwitterTimeline/TSLAFollowerBots.csv"
          )
          #Sys.sleep(16*60)
        }
      }
    },
    error = function(cond){
      TSLAFollowerBots.df.TEMP <- read.csv(
        file = "TSLAFollowerTwitterTimeline/TSLAFollowerBots.csv"
      )
        
      TSLAFollowerBots.df <- tweetbotornot::tweetbotornot(
        x = TSLATFollowerTwitterTimeline.df$screenName[i:i+150],
        fast = FALS
      )
      i <- (i+150)
      TSLAFollowerBots.df.TEMP <- rbind(TSLAFollowerBots.df.TEMP,TSLAFollowerBots.df)
      TSLAFollowerBots.df <- TSLAFollowerBots.df.TEMP
      write.csv(
        x = TSLAFollowerBots.df,
        file = "TSLAFollowerTwitterTimeline/TSLAFollowerBots.csv"
      )
      #Sys.sleep(16*60)
  
    }
  )
  
}

#### Se agarran los followers dentro de los parametros ####
TSLAFollowerBots.Select <- function(){
  TSLAFollowerBots.df <- read.csv(
    file = "TSLAFollowerTwitterTimeline/TSLAFollowerBots.csv"
  )
  
  TSLAFollowerBots.df <- subset(
    x = TSLAFollowerBots.df,
    subset = prob_bot <.5
  )
  
  TSLAFollowerBots.df <- subset(
    x = TSLAFollowerBots.df,
    select = c("screen_name","user_id", "prob_bot")
  )
  
  write.csv(
    x = TSLAFollowerBots.df,
    file = "TSLAFollowerTwitterTimeline/TSLAFollowerBots.csv"
  )
  
  return(TSLAFollowerBots.df)
}

#### Se agarraron los statuses de los followers que no se consideran como bots ####
TSLAFollowerTweets.Search <- function(id){
  if(exists("TSLAFollowerTweets.df")){
    temp <- read.csv(
      file = "TSLAFollowerTwitterTimeline/TSLAFollowerTweets.csv",
      stringsAsFactors = FALSE
    )
    
    id <- setdiff(id, unique(temp$screenName))
    
    TSLAFollowerTweets.df <- twitteR::userTimeline(
      user = id,
      n = 3200,
      includeRts = TRUE,
      excludeReplies = FALSE,
      Sys.sleep(120)
    )
    temp <- rbind(temp, TSLAFollowerTweets.df)
    TSLAFollowerTweets.df <- temp
    
  }else{
    TSLAFollowerTweets.df <- twitteR::userTimeline(
      user = id,
      n = 3200,
      includeRts = TRUE,
      excludeReplies = FALSE,
      Sys.sleep(120)
    )
  }
  
  TSLAFollowerTweets.df <- twitteR::twListToDF(TSLAFollowerTweets.df)
  
  write.csv(
    x = TSLAFollowerTweets.df,
    file = "TSLAFollowerTwitterTimeline/TSLAFollowerTweets.csv"
  )
  
  return(TSLAFollowerTweets.df)
}

#### Se agarran los tweets de los follower y se cargan ####
TSLAFollowerTweets.Load <- function(){
  TSLAFollowerTweets.df <- read.csv(
    file = "TSLAFollowerTwitterTimeline/TSLAFollowerTweets.csv",
    stringsAsFactors = FALSE
  )
  
  return(TSLAFollowerTweets.df)
}

















TSLAFollowerTweets.Clean <- function(df){
  TSLAFollowerTweets.df <- df
  
  
  
  
}



