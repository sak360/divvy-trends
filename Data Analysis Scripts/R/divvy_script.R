
rm(list = ls())

setwd("C:\\Users\\skhan231\\Desktop\\IDS560\\Modeling\\R")

if (Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")

#Load Libraries
library(dplyr)
library(RODBC)
library(gdata)
library(ggplot2)
library(plotly)

channel <- odbcConnect(dsn="sql_tdi_db")

divvyData <- sqlQuery(channel,"Select * from [Divvy_Trips]")

close(channel)

#write.csv(initData, 'latestData.csv')

row.names(divvyData) <- divvyData$TRIP_ID

divvyData <- select(divvyData, -TRIP_ID)

#some stations have moved....different lat longs. looking at just from and to ids, we see 104739 different combos
distinct.stations <- distinct(divvyData, FROM_STATION_ID, FROM_LATITUDE, FROM_LONGITUDE, TO_STATION_ID, TO_LATITUDE, TO_LONGITUDE)
#nrow is now 108127



#function two compute great circle distance between two points, in KM
gcd <- function (lat1, lon1, lat2, lon2)
{
  R <- 6371 #assume radius of earth to be 6371 km
  # function to convert degrees to radians
  deg2rad <- function(deg) return(deg*pi/180)
  
  #convert degrees to radians
  lat1 <- deg2rad(lat1)
  lon1 <- deg2rad(lon1)
  lat2 <- deg2rad(lat2)
  lon2 <- deg2rad(lon2)
  
  dlon = lon2 - lon1 
  dlat = lat2 - lat1 
  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2 
  c = 2 * atan2( sqrt(a), sqrt(1-a) ) 
  d = R * c
  return(d)
}


distinct.stations <- mutate(distinct.stations, trip_distance = gcd(FROM_LATITUDE, FROM_LONGITUDE, TO_LATITUDE, TO_LONGITUDE))

#join distinct table with divvyData

divvyData <- left_join(divvyData, distinct.stations, by=c("FROM_STATION_ID","FROM_LATITUDE", "FROM_LONGITUDE","TO_STATION_ID", "TO_LATITUDE", "TO_LONGITUDE"))

divvyData.filtered.distance <- filter(divvyData, trip_distance > 0) %>% mutate(from_to =  paste0(FROM_LOCATION, TO_LOCATION), km_per_hour = trip_distance / (TRIP_DURATION / 60))
# ^ #7,560,067 (full is 7867397; 307330 start and end at same station)

#group data by YEAR_MONTH
divvyData.filtered.grouped.month2 <- group_by(divvyData.filtered.distance, YEAR_MONTH =  paste0(getYear(START_TIME),getMonth(START_TIME)))

summary.grouped.month2 <- summarise(divvyData.filtered.grouped.month2, avgTrip_km_per_hour = mean(km_per_hour), num_rides = n())

summary.grouped.month2$YEAR_MONTH <- as.double(summary.grouped.month2$YEAR_MONTH)

month.vs.km.line <- ggplot(summary.grouped.month2, aes(x=YEAR_MONTH,y=avgTrip_km_per_hour))

month.vs.km.line <- month.vs.km.line + geom_line(data=summary.grouped.month2) + xlab('Time [YYYYMM]') + ylab('Average km/hr') + ggtitle('Average km/hr vs. time')


ggplotly(month.vs.km.line)


month.vs.rides.area <- ggplot(summary.grouped.month2, aes(x=YEAR_MONTH, y = num_rides/1000)) + geom_area()  + xlab('Time [YYYYMM]') + ylab('Number of rides (in 000s)') + ggtitle('Number of rides vs. time')

ggplotly(month.vs.rides.area)

#rides vs. avg. km/hr

#####normalize data
scale_vector <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))}

summary.grouped.month2.norm <- summary.grouped.month2

summary.grouped.month2.norm$num_rides <- scale_vector(summary.grouped.month2$num_rides)

summary.grouped.month2.norm$avgTrip_km_per_hour <- scale_vector(summary.grouped.month2.norm$avgTrip_km_per_hour)

#write.csv(summary.grouped.month2.norm,'summary.grouped.month2.norm.csv')

month.vs.km.vs.rides.line.norm <- ggplot(summary.grouped.month2.norm, aes(x=YEAR_MONTH)) + geom_area(aes(y = avgTrip_km_per_hour, fill = "avgTrip_km_per_hour"), alpha = 0.5) + geom_area(aes(y = num_rides, fill = "num_rides"), alpha = 0.5) + theme(legend.position = "bottom")

month.vs.km.vs.rides.line.norm <- month.vs.km.vs.rides.line.norm + xlab('Time [YYYYMM]') + ylab('Average km/hr and number of rides [Normalized]') + ggtitle('Average km/hr AND number of rides vs. time')

ggplotly(month.vs.km.vs.rides.line.norm)

