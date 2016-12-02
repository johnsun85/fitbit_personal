library("httr")
library("jsonlite")
library("ggplot2")
library("quantreg")

oauth_code <- function(key, secret){
  
  if(missing(key) || missing(secret)){
    stop("Both Key and Secret must be provided")
  }
  
  request <- "https://api.fitbit.com/oauth2/token"
  authorize <- "https://www.fitbit.com/oauth2/authorize"
  access <- "https://api.fitbit.com/oauth2/token"
  endpoint <- httr::oauth_endpoint(request, authorize, access)
  myapp <- httr::oauth_app("r-fitbit", key, secret)
  scope <- c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight")
  user_params <- list(response_type = "code", redirect_uri = "http://localhost:1410/")
  #  httr::init_oauth2.0(endpoint, myapp, scope=scope, user_params = user_params, use_basic_auth=TRUE)
  httr::oauth2.0_token(endpoint, myapp, scope=scope, type = NULL, use_oob = FALSE, user_params = user_params, use_basic_auth=TRUE, cache=TRUE)
  
}


fitbit_key <- "ENTER YOUR KEY HERE"
fitbit_secret <- "ENTER YOUR SECRET HERE"

code <- oauth_code(fitbit_key, fitbit_secret)
token <- oauth_token(fitbit_key, fitbit_secret, code)


url <- "https://api.fitbit.com/1/user/-/activities/heart/date/2016-10-11/1d/1min/time/16:30/17:30.json"
url2 <- "https://api.fitbit.com/1/user/-/activities/heart/date/2016-10-11/1d.json"
url3 <- "https://api.fitbit.com/1/user/-/activities/calories/date/2016-10-11/1d/1min/time/16:30/17:30.json"
url4 <- "https://api.fitbit.com/1/user/-/activities/steps/date/2016-10-11/1d/1min/time/16:30/17:30.json"
url5 <- "https://api.fitbit.com/1/user/-/activities/distance/date/2016-10-11/1d/1min/time/16:30/17:30.json"
url6 <- "https://api.fitbit.com/1/user/-/activities/elevation/date/2016-10-11/1d/1min/time/16:30/17:30.json"

#heart rate
response <- httr::GET(url, config = (token = code))

data <- jsonlite::fromJSON(httr::content(response, as = "text"))
date <- as.Date(data$`activities-heart`$dateTime)

df <- transform(as.data.frame(data$`activities-heart-intraday`$dataset),datetime = as.POSIXct(strptime((paste(data$`activities-heart`$dateTime, data$`activities-heart-intraday`$dataset$time, sep = " ")), "%Y-%m-%d %H:%M:%S")))

#calories

response <- httr::GET(url3, config = (token = code))
data <- jsonlite::fromJSON(httr::content(response, as = "text"))

df2 <- transform(as.data.frame(data$`activities-calories-intraday`$dataset),datetime = as.POSIXct(strptime((paste(data$`activities-calories`$dateTime, data$`activities-calories-intraday`$dataset$time, sep = " ")), "%Y-%m-%d %H:%M:%S")))

#calories

response <- httr::GET(url4, config = (token = code))
data <- jsonlite::fromJSON(httr::content(response, as = "text"))

df3 <- transform(as.data.frame(data$`activities-steps-intraday`$dataset),datetime = as.POSIXct(strptime((paste(data$`activities-steps`$dateTime, data$`activities-steps-intraday`$dataset$time, sep = " ")), "%Y-%m-%d %H:%M:%S")))

#elevation

response <- httr::GET(url6, config = (token = code))
data <- jsonlite::fromJSON(httr::content(response, as = "text"))

df4 <- transform(as.data.frame(data$`activities-elevation-intraday`$dataset),datetime = as.POSIXct(strptime((paste(data$`activities-elevation`$dateTime, data$`activities-elevation-intraday`$dataset$time, sep = " ")), "%Y-%m-%d %H:%M:%S")))



df_all <- as.data.frame(df$datetime)
names(df_all)[1] <- "datetime"
df_all$hr <- df$value
df_all$calories <- df2$value
df_all$steps <- df3$value
df_all$elevation <- df4$value