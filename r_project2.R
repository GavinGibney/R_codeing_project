install.packages("tidyverse")
library(tidyverse)               #needed for the import statements
library(dplyr)                   #used for the joining of dataframes
library(readr)                   #to be safe as not 100% certain if this is imported with tidyverse


campaigns <- read_csv('_campaigns.csv')     
advertiser <- read_csv('_advertiser.csv')   # used this so assumptions aren't amde about data
impressions <- read_tsv('_impressions.tsv')
clicks <- read_tsv('clicks.tsv')            # tsv import paths

view(campaigns)
view(advertiser)
view(impressions)
view(clicks)             #examine data we are working with

clicks$date<-as.Date(clicks$date, "%d-%m-%Y")                         # converting string dates to date format
impressions$date<-as.Date(impressions$date, "%d-%m-%Y")

clicks_converted <- c(date, time, timezone) {                                  # converts times to UTC timezone
  if (clicks$timezone == "Eastern time" & clicks$time >= 19:00:00) {
    clicks$time + 18000 & as.date(clicks$date) + 1                             #number of seconds time needs to be inceased by and adding 1 day to date
  } else if (clicks$timezone == "Eastern time" & clicks$time < 19:00:00) {
    clicks$time + 18000                                                        #number of seconds added to time
  } else if (clicks$timezone == "Pacific time" & clicks$time >= 16:00:00) {
    clicks$time + 28000 & as.date(clicks$date) + 1
  }else if (clicks$timezone == "Pacific time" & clicks$time < 16:00:00) {
    clicks$time +28000
  } else {
    print("This date and time are already in UTC")
  }
}


impressions_converted <- c(date, time, timezone) {                                       # converts times to UTC timezone
    if (impressions$timezone == "Eastern time" & impressions$time >= 19:00:00) {
      impressions$time + 18000 & as.date(impressions$date) + 1                           #number of seconds time needs to be inceased by and adding 1 day to date
    } else if (impressions$timezone == "Eastern time" & impressions$time < 19:00:00) {
      impressions$time + 18000                                                           #number of seconds added to time
    } else if (impressions$timezone == "Pacific time" & impressions$time >= 16:00:00) {
      impressions$time + 28000 & as.date(impressions$date) + 1
    }else if (impressions$timezone == "Pacific time" & impressions$time < 16:00:00) {
      impressions$time +28000
    } else {
      print("This date and time are already in UTC")
    }
}

clicks_join <- left_join(clicks_converted, advertiser, by = NULL, match = "all")   #joins columns of advertiser to converted clicks

clicks_join_final <- left_join(clicks_join, campaigns, by = NULL, match = "all")   #joins all columns of campaigns to the already joined data frame


impressions_join <- left_join(impressions_converted, advertiser, by = NULL, match = "all")   #joins columns of advertiser to converted impressions

impressions_join_final <- left_join(impressions_join, campaigns, by = NULL, match = "all")   #joins all columns of campaigns to the already joined data frame


write.csv(clicks_join_final, file = "clicks_processed.csv")           #writes the processed file to csv
write.csv(impressions_join_final, file = "impressions_processed.csv") #writes the processed file to csv
