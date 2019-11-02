#Tesla Twitter Timeline search

##### Busqueda en twitter de 3200 tweets publicados por Tesla #####

#TSLATwitterTimelineSearch <- twListToDF(
#  userTimeline(
#    user = "Tesla", n = 3200, sinceID = '2014-06-01', includeRts = TRUE, 
#    excludeReplies = FALSE
#  )
#)

#Creación de archivo en formato utilizable
#write.csv(
#  x = TSLATwitterTimelineSearch,
#  file = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv"
#)


#Tweets utiles
TSLATwitterTimelineSearch.fechas <- function(){
  temp <- utils::read.csv(
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
  
 utils::write.csv(
    x = temp,
    file = "TSLATwitterTimeline/TSLATwitterTimelineSearchData.csv"
  )
 
 temp
 return(temp)
}


#### Procesamiento de la información de Tesla recopilada ####

TSLATwitterTimelineSearch.limpieza <- function(TSLATwitterTimelineSearch.df){
TSLATwitterTimelineSearch.df.columnas <- c("user_id", "status_id", "created_at", "screen_name",
                                           "text", "reply_to_status_id", "reply_to_user_id",
                                           "reply_to_screen_name", "is_quote", "is_retweet",
                                           "favorite_count", "retweet_count", "hashtags", "symbols",
                                           "urls_expanded_url", "media_expanded_url", "media_type",
                                           "ext_media_expanded_url", "mentions_user_id",
                                           "mentions_screen_name", "lang", "quoted_status_id",
                                           "quoted_text" , "quoted_created_at", "quoted_favorite_count",
                                           "quoted_retweet_count", "quoted_user_id",
                                           "quoted_screen_name", "quoted_name", "quoted_followers_count",
                                           "quoted_friends_count", "quoted_statuses_count",
                                           "quoted_location", "quoted_verified", "retweet_status_id",
                                           "retweet_text", "retweet_created_at",
                                           "retweet_favorite_count", "retweet_retweet_count",
                                           "retweet_user_id", "retweet_screen_name", "retweet_name",
                                           "retweet_followers_count", "retweet_friends_count",
                                           "retweet_statuses_count", "retweet_location",
                                           "retweet_verified", "country")

#Seleccion de columnas anteriores
TSLATwitterTimelineSearch.df <- subset(
  x = TSLATwitterTimelineSearch.df,
  select = TSLATwitterTimelineSearch.df.columnas
)

#Limpieza de columnas




}


















