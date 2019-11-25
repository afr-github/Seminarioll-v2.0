#Tesla Twitter Timeline search

##### Busqueda de informacion de Tesla ####
TSLAInfo.Search <- function(){
  TSLAInformation <- twitteR::getUser(
    user = "Tesla"
  )
  
  TSLAInformation.df <- as.data.frame(TSLAInformation)
  
  return(TSLAInformation.df)
}

##### Escritura de informacion encontrada de Tesla ####
TSLAInfo.Write <- function(){
  write.csv(
    x = TSLAInfo.Search(),
    file = "TSLATwitterTimeline/TSLAData.csv"
  )
}

##### Cargar la informacion de Tesla ####
TSLAInfo.Load <- function(){
  if(file.exists("TSLATwitterTimeline/TSLAData.csv")){
    TSLAData.csv <- read.csv(
      file = "TSLATwitterTimeline/TSLAData.csv",
      stringsAsFactors = FALSE
    )  
  }
  return(TSLAData.csv[2:length(TSLAData.csv)])
}

##### Busqueda en twitter de 3200 tweets publicados por Tesla #####
TSLATwitterTimeline.Search <- function(){
  TSLATwitterTimelineSearch <- rtweet::get_timeline(
      user = "Tesla", n = 3200, sinceID = '2014-06-01', includeRts = TRUE, 
      excludeReplies = FALSE
  )
  
  rtweet::write_as_csv(
    x = TSLATwitterTimelineSearch,
    file_name = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv",
    prepend_ids = FALSE
  ) 
}

#### Tweets en las fechas deseadas ####
TSLATwitterTimeline.Search.fechas <- function(){
  temp <- rtweet::read_twitter_csv(
    file = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv"
  )

  #Seleccion de fechas utiles
  temp$created_at <- as.Date(x = temp$created_at, tryformat = "%d/%m/%Y")
  temp <- subset(
    x = temp,
    subset = (created_at >= as.Date("2014-06-01", tryformat = "%d/%m/%Y")) &
             (created_at  < as.Date("2019-06-01", tryformat = "%d/%m/%Y"))
  )
  
 rtweet::write_as_csv(
    x = temp,
    file_name = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv",
    prepend_ids = FALSE
  )
  
 return(temp)
}

#### Tweets con las columnas deseadas ####
TSLATwitterTimeline.Search.limpieza <- function(TSLATwitterTimeline.Search.df){
TSLATwitterTimeline.Search.df.columnas <- c("user_id", "status_id", "created_at", "screen_name", "text", "reply_to_status_id",
                                           "reply_to_user_id", "reply_to_screen_name", "is_quote", "is_retweet", "favorite_count",
                                           "retweet_count", "hashtags", "symbols", "urls_expanded_url", "media_expanded_url",
                                           "media_type", "ext_media_expanded_url", "mentions_user_id", "mentions_screen_name",
                                           "lang", "quoted_status_id", "quoted_text", "quoted_created_at", "quoted_favorite_count",
                                           "quoted_retweet_count", "quoted_user_id", "quoted_screen_name", "quoted_name",
                                           "quoted_followers_count", "quoted_friends_count", "quoted_statuses_count",
                                           "quoted_location", "quoted_verified", "retweet_status_id", "retweet_text",
                                           "retweet_created_at", "retweet_favorite_count", "retweet_retweet_count",
                                           "retweet_user_id", "retweet_screen_name", "retweet_name", "retweet_followers_count",
                                           "retweet_friends_count", "retweet_statuses_count", "retweet_location", "retweet_verified",
                                           "country")
  #Seleccion de columnas anteriores
  TSLATwitterTimeline.Search.df <- subset(
    x = TSLATwitterTimeline.Search.df,
    select = TSLATwitterTimeline.Search.df.columnas
  )
  
  #Guardar las columnas deseadas
  rtweet::write_as_csv(
    x = TSLATwitterTimeline.Search.df,
    file_name = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv",
    prepend_ids = FALSE
  )
}

#### Cargar tweets ####
TSLATwitterTimeline.Load <- function(){
  TSLATwitterTimeline.Search.limpieza(TSLATwitterTimeline.Search.fechas())
  
  TSLATwitterTimeline.df <- read.csv(
    file = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv",
    stringsAsFactors = FALSE
  )
  
  return(TSLATwitterTimeline.df)
}
