rm(list = ls())

#Crerated by Shahbaz Ali Khan
#Divvy Trends

############################################################ DIVVY SCRIPT ############################################################

#Load Libraries

suppressMessages(library(dplyr))
suppressMessages(library(RODBC))
suppressMessages(library(gdata))
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))

#Load Data
channel <- odbcConnect(dsn="sql_tdi_db")
divvyData <- sqlQuery(channel,"Select * from [Divvy_Trips]")
close(channel)

#Find distinct stations trips (trips between stations)
distinct.stations <- distinct(divvyData, FROM_STATION_ID, TO_STATION_ID)
nrow(distinct.stations)
#Find distinct stations. This time, consider the lat lons as well
distinct.stations <- distinct(divvyData, FROM_STATION_ID, FROM_LATITUDE, FROM_LONGITUDE, TO_STATION_ID, TO_LATITUDE, TO_LONGITUDE)
nrow(distinct.stations)
#It looks like some stations have moved over the years, as we see a slightly higher number of combinations (stationIDs were reassigned)


#Define function two compute great circle distance between two points, in KM
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

#Compute the distance between the unique stations
distinct.stations <- mutate(distinct.stations,trip_distance = gcd(FROM_LATITUDE, FROM_LONGITUDE, TO_LATITUDE, TO_LONGITUDE))

#join distinct table with divvyData. This saves on computing the distance on each of the 7.5 million + rows
divvyData <- left_join(divvyData, distinct.stations, by=c("FROM_STATION_ID","FROM_LATITUDE", "FROM_LONGITUDE","TO_STATION_ID", "TO_LATITUDE", "TO_LONGITUDE"))

#Filter out trips that have 0 distance (start and end at the same station) and then, compute a from_to variable AND a km/hour variable
divvyData.filtered.distance <- filter(divvyData, trip_distance > 0) %>% mutate(from_to =  paste0(FROM_LOCATION, TO_LOCATION), km_per_hour = trip_distance / (TRIP_DURATION / 60))

#Group data by Year and month (aggregated into a YEAR_MONTH variable)
divvyData.filtered.grouped.month2 <- group_by(divvyData.filtered.distance, YEAR_MONTH =  paste0(getYear(START_TIME),getMonth(START_TIME)))

#Summarise data and compute the average km/hr for each YEAR_MONTH pair (37 unique months)
summary.grouped.month2 <- summarise(divvyData.filtered.grouped.month2, avgTrip_km_per_hour = mean(km_per_hour), num_rides = n())


#convert YEAR_MONTH to numeric
summary.grouped.month2$YEAR_MONTH <- as.double(summary.grouped.month2$YEAR_MONTH)

#plot YEAR_MONTH vs avgTrip_km_per_hour
month.vs.km.line <- ggplot(summary.grouped.month2, aes(x=YEAR_MONTH,y=avgTrip_km_per_hour))

month.vs.km.line <- month.vs.km.line + geom_line(data=summary.grouped.month2) + xlab('Time [YYYYMM]') + ylab('Average km/hr') + ggtitle('Average km/hr vs. time')

#plot avg. km/hr vs. time (plot 1)
ggplotly(month.vs.km.line)

#build plot for 'Number of rides vs. time'
month.vs.rides.area <- ggplot(summary.grouped.month2, aes(x=YEAR_MONTH, y = num_rides/1000)) + geom_area()  + xlab('Time [YYYYMM]') + ylab('Number of rides (in 000s)') + ggtitle('Number of rides vs. time')

#plot 'Number of rides vs. time' (plot 2)
ggplotly(month.vs.rides.area)

##For plot 3, the data needs to first be normalized

#function to normalize vectors
scale_vector <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))}

#copy data into .norm
summary.grouped.month2.norm <- summary.grouped.month2

#normalize num_rides
summary.grouped.month2.norm$num_rides <- scale_vector(summary.grouped.month2$num_rides)

#normalize avgTrip_km_per_hour
summary.grouped.month2.norm$avgTrip_km_per_hour <- scale_vector(summary.grouped.month2.norm$avgTrip_km_per_hour)

#build plot for 'Average km/hr AND number of rides vs. time'
month.vs.km.vs.rides.line.norm <- ggplot(summary.grouped.month2.norm, aes(x=YEAR_MONTH)) + geom_area(aes(y = avgTrip_km_per_hour, fill = "avgTrip_km_per_hour"), alpha = 0.5) + geom_area(aes(y = num_rides, fill = "num_rides"), alpha = 0.5) + theme(legend.position = "bottom")

month.vs.km.vs.rides.line.norm <- month.vs.km.vs.rides.line.norm + xlab('Time [YYYYMM]') + ylab('Average km/hr and number of rides [Normalized]') + ggtitle('Average km/hr AND number of rides vs. time')

#plot 'Average km/hr AND number of rides vs. time' (plot 3)
ggplotly(month.vs.km.vs.rides.line.norm)

#build plot for 'Average km/hr AND number of rides vs. time', points on line (plot 4)
month.vs.km.line <- month.vs.km.line + geom_line() + geom_point(aes(color=num_rides/1000), size = 2) + xlab('Time [YYYYMM]') + ylab('Average km/hr and number of rides') + ggtitle('Average km/hr and number of rides vs. time') + scale_color_gradient(low="blue", high="red")

#build plot
ggplotly(month.vs.km.line)


################################################### CRIME SCRIPT ################################################

library(dplyr)
library(ggplot2)
library(plotly)
library(gdata)

# setwd("C:\\Users\\skhan231\\Desktop\\TDI_Challenge\\R")

crimeData <- read.csv('file:///C:/Users/skhan231/Downloads/Crimes_-_2001_to_present.csv')

#Assign ID as the row name
row.names(crimeData) <- crimeData$ID

#Drop the ID column as it has no further use
crimeData <- select(crimeData, -ID)

#Add Year, Month, and another field titled YEAR_MONTH
crimeData <- mutate(crimeData, Year = substr(Date, 7,10), Month = substr(Date, 1,2)) %>% mutate(YEAR_MONTH =  paste0(Year,Month))

#convert YEAR_MONTH to int
crimeData$YEAR_MONTH <- as.numeric(crimeData$YEAR_MONTH)

#Percentage of domestic crimes. These will be filtered out
print(paste0(nrow(filter(crimeData, Domestic == "true")) / nrow(crimeData) * 100,'% of crimes are domestic'))

#Types of crimes recorded
distinct(crimeData, Primary.Type)

#Data for bar plot -- Count of types of crime, by year
num.crimes <- ggplot(crimeData, aes(Primary.Type)) +  geom_bar(aes(fill = Year)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(fill="Year") + xlab('') + ylab('Frequency of crime') + ggtitle('Frequency of each crime type vs. Year')

# By default, uses stat="bin", which gives the count in each category
ggplotly(num.crimes)

#Only select the crimes that are hypothesized to affect the divvy route. This would mean filtering out domestic cases and only considering relevant crimes
crimeData.filtered <- filter(crimeData, Domestic == "false", Primary.Type == "THEFT" | Primary.Type == "ASSAULT" | Primary.Type == "BATTERY" |  Primary.Type == "ROBBERY" | Primary.Type == "BURGLARY" |
                               Primary.Type == "WEAPONS VIOLATION" | Primary.Type == "SEX OFFENSE" | Primary.Type == "CRIM SEXUAL ASSAULT" | Primary.Type == "HOMICIDE")

#Group data by type of crime and YEAR_MONTH
crimeData.filtered.grouped <- group_by(crimeData.filtered, Primary.Type, YEAR_MONTH)

#Summarise data and compute the count of each of the subset crime types
crimeData.filtered.grouped.summ <- summarise(crimeData.filtered.grouped, Frequency = n())

#Build plot for 'Frequency of selected crime types vs. Time'
crime.time <- ggplot(data=crimeData.filtered.grouped.summ, aes(x=YEAR_MONTH, y=Frequency, group=Primary.Type, color=Primary.Type)) + geom_line() + labs(color='Crime Type') + xlab('Time [YYYYMM]') + ylab('Frequency of crime vs. Time') + ggtitle('Frequency of selected crime types vs. Time')

#Create the plot
ggplotly(crime.time)


####################################### WEATHER TO DIVVY ######################################################

#Load Libraries
library(dplyr)
library(RODBC)
library(gdata)
library(ggplot2)
library(plotly)

# channel <- odbcConnect(dsn="sql_tdi_db")
# divvyData <- sqlQuery(channel,"Select * from [Divvy_Trips]") #read if not read in before
# close(channel)

#load in crime data
# crimeData <- read.csv('Crimes_-_2001_to_present.csv') #read if not read in before
#load in weather data
df.weather <- read.csv('weatherORD_2013_now.csv')

#assign weather data an ID. This will help with joins later
df.weather$ID <- seq.int(nrow(df.weather))
#assign IDs to row.names as well
row.names(df.weather) <- df.weather$ID

#convert staetime to date object
df.weather$ChicagoTime = as.POSIXct(strptime(df.weather$ChicagoTime, "%Y-%m-%d %H:%M:%S"))


#WORKS
getClosestWeatherRecord <- function(x) {
  
  y <- (which(abs(as.numeric(x)-as.numeric(df.weather$ChicagoTime))==min(abs(as.numeric(x)-as.numeric(df.weather$ChicagoTime))), arr.ind = F, useNames = T) )
  
  if(length(y) >=1 ){
    return(y[[1]])
  }
  else
  {
    return(-1)
  }
}

#############joining weather data to Divvy Data

divvyData$ClosestWeatherID <-  sapply(divvyData$START_TIME, getClosestWeatherRecord)


#join wether data with divvy data
divvyData <- left_join(divvyData,df.weather, by = c("ClosestWeatherID" = "ID"))

nrow(distinct(divvyData, ClosestWeatherID))


#Data for bar plot -- Count of types of divvy rides by weather
rides.by.weather <- ggplot(divvyData, aes(Conditions)) +  geom_bar(aes(fill = getYear(ChicagoTime))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(fill="Year") + xlab('') + ylab('Frequency of Rides') + ggtitle('Frequency of rides by weather vs. Year')

# By default, uses stat="bin", which gives the count in each category
ggplotly(rides.by.weather)


############joining weather data to Crime data

#convert staetime to date object
crimeData$Date = as.POSIXct(strptime(crimeData$Date,format="%m/%d/%Y %H:%M:%S %p"))

#closest weather id to the crime
crimeData$ClosestWeatherID <-  sapply(crimeData$Date, getClosestWeatherRecord)

#join wether data with crime data
crimeData <- left_join(crimeData,df.weather, by = c("ClosestWeatherID" = "ID"))

#filter out crimes that had no date. The function 'ClosestWeatherID' would send back -1 if that's the case
crimeData <- filter(crimeData, ClosestWeatherID != -1)

#join wether data with crime data
crimeData <- left_join(crimeData,df.weather, by = c("ClosestWeatherID" = "ID"))

nrow(distinct(crimeData, ClosestWeatherID))

#Data for bar plot -- Count of types of divvy rides by weather
crimes.by.weather <- ggplot(crimeData, aes(Conditions)) +  geom_bar(aes(fill = getYear(Date))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(fill="Year") + xlab('') + ylab('Frequency of Crimes') + ggtitle('Frequency of crimes by weather vs. Year')

# By default, uses stat="bin", which gives the count in each category
ggplotly(crimes.by.weather)


#############################################GEO-MAPPING DATA###################################################

library(leaflet)
require(devtools)
library(dplyr)
library(ggplot2)

divvyData.map <- divvyData

divvyData.start.agg <- divvyData.map %>% group_by(FROM_STATION_ID,FROM_STATION_NAME,FROM_LONGITUDE,FROM_LATITUDE) %>% summarise(Frequency = n()) %>% as.data.frame()

#since some stations were moved, need to merge them together

# divvyData.start.agg.merge <- divvyData.start.agg %>% group_by(FROM_STATION_ID) %>% summarise(Frequency_Total = sum(Frequency)) %>% as.data.frame()

# divvyData.start.agg <- divvyData.start.agg %>% left_join(divvyData.start.agg.merge, on = FROM_STATION_ID) %>% select(-Frequency) %>% distinct(FROM_STATION_ID)





########


# install_github('ramnathv/rCharts@dev')
# install_github('ramnathv/rMaps')
# install.packages("leaflet")


# 
# m <- leaflet() %>%
#     addProviderTiles("CartoDB.Positron") %>%  # Add default OpenStreetMap map tiles
#   setView(lng=-87.623177, lat= 41.881832, zoom = 14) %>%
#   addMarkers(lng=-87.623177, lat= 41.881832, popup="The birthplace of Shabby")
# m  # Print the map



leaflet.divvy <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng=-87.623177, lat= 41.881832, zoom = 13) %>%
  addCircles(lng= divvyData.start.agg$FROM_LONGITUDE, lat = divvyData.start.agg$FROM_LATITUDE, 
             weight = 1, radius = sqrt(divvyData.start.agg$Frequency),
             popup = paste("Station:", divvyData.start.agg$FROM_STATION_NAME,"<br>",
                           "Station ID:", divvyData.start.agg$FROM_STATION_ID,"<br>",
                           "Num. of Rides:", divvyData.start.agg$Frequency))

leaflet.divvy


#Only select the crimes that are hypothesized to affect the divvy route. This would mean filtering out domestic cases and only considering relevant crimes
crimeData.filtered.2 <- filter(crimeData, Domestic == "false", Primary.Type == "THEFT" | Primary.Type == "ASSAULT" | Primary.Type == "BATTERY" |  Primary.Type == "ROBBERY" | Primary.Type == "BURGLARY" |
                               Primary.Type == "WEAPONS VIOLATION" | Primary.Type == "SEX OFFENSE" | Primary.Type == "CRIM SEXUAL ASSAULT" | Primary.Type == "HOMICIDE")

#omit incomplete crime data
crimeData.filtered.2 <- na.omit(crimeData.filtered.2)




# install.packages('geosphere')
# library(geosphere)



midpoint_two_points <- function (lat1, lon1, lat2, lon2)
{
  R <- 6371 #assume radius of earth to be 6371 km
  # function to convert degrees to radians
  deg2rad <- function(deg) return(deg*pi/180)
  
  #convert degrees to radians
  lat1 <- deg2rad(lat1)
  lon1 <- deg2rad(lon1)
  lat2 <- deg2rad(lat2)
  lon2 <- deg2rad(lon2)
  
  
  Bx = cos(lat2) * cos(lon2-lon1);
  By = cos(lat2) * sin(lon2-lon1);
  
  
  latMid = atan2(sin(lat1) + sin(lat2),
                 sqrt( (cos(lat1)+Bx)*(cos(lat1)+Bx) + By*By ) );
  lonMid = lon1 + atan2(By, cos(lat1) + Bx);
  
  rad2deg <- function(rad) return(rad/pi*180)
  
  latMid <- rad2deg(latMid)
  lonMid <- rad2deg(lonMid)
  
  
  
  return(paste(latMid, lonMid, sep=","))
}



midpoint_two_points(41.881832, -87.623177, 41.881126, -87.641837)
# 
# 
# 
# p1 <- c(41.881832, -87.623177) #lat
# p2 <- c(41.881126, -87.641837) #lon




# midPoint(p1, p2, a=6378137, f = 1/298.257223563)

# nrow(distinct.stations)

distinct.stations <- distinct.stations %>% mutate(Mid_Location = midpoint_two_points(FROM_LATITUDE,FROM_LONGITUDE,TO_LATITUDE,TO_LONGITUDE))


library(stringr)
distinct.stations$Mid_lat <- as.numeric(str_split_fixed(distinct.stations$Mid_Location, ",", 2)[,1])#column 1 is lats
distinct.stations$Mid_lon <- as.numeric(str_split_fixed(distinct.stations$Mid_Location, ",", 2)[,2])#column 2 is lons



# stations.and.crime <- merge(distinct.stations[1,], crimeData.filtered.2) #merging one station with crime data



# numOfCRimesInRadius <- function(radius, lat, lon) {
#   sapply(, function)
# }



#WORKS
numOfCRimesInRadius <- function(x) {
  distinct.stations[1,]
}


#Compute the distance between the unique stations
distinct.stations <- mutate(distinct.stations,trip_distance = gcd(FROM_LATITUDE, FROM_LONGITUDE, TO_LATITUDE, TO_LONGITUDE))



# divvyData$ClosestWeatherID <-  sapply(divvyData$START_TIME, getClosestWeatherRecord)



divvyData.tiny <- head(divvyData, 100)


sApplyTest <- function(x) {

print(x)
  
}

sapply(divvyData.tiny, sApplyTest)


# filter(distinct.stations, distinct.stations$Mid_lon == NA)


################work with one month's data



library(leaflet)
require(devtools)
library(dplyr)
library(ggplot2)


# divvyData.map.2015.7 <- divvyData.map

divvyData.map.2015.7 <- filter(divvyData, getYear(START_TIME) == "2015", getMonth(START_TIME) == "07")

divvyData.start.agg.2015.7 <- divvyData.map.2015.7 %>% group_by(FROM_STATION_ID,FROM_STATION_NAME,FROM_LONGITUDE,FROM_LATITUDE) %>% summarise(Frequency = n()) %>% as.data.frame()


leaflet.divvy.2015.7 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng=-87.623177, lat= 41.881832, zoom = 13) %>%
  addCircles(lng= divvyData.start.agg.2015.7$FROM_LONGITUDE, lat = divvyData.start.agg.2015.7$FROM_LATITUDE, 
             weight = 1, radius = sqrt(divvyData.start.agg.2015.7$Frequency),
             popup = paste("Station:", divvyData.start.agg.2015.7$FROM_STATION_NAME,"<br>",
                           "Station ID:", divvyData.start.agg.2015.7$FROM_STATION_ID,"<br>",
                           "Num. of Rides:", divvyData.start.agg.2015.7$Frequency))

leaflet.divvy.2015.7


#Only select the crimes that are hypothesized to affect the divvy route. This would mean filtering out domestic cases and only considering relevant crimes
crimeData.filtered.2015.7 <- filter(crimeData, Domestic == "false", Primary.Type == "THEFT" | Primary.Type == "ASSAULT" | Primary.Type == "BATTERY" |  Primary.Type == "ROBBERY" | Primary.Type == "BURGLARY" |
                                 Primary.Type == "WEAPONS VIOLATION" | Primary.Type == "SEX OFFENSE" | Primary.Type == "CRIM SEXUAL ASSAULT" | Primary.Type == "HOMICIDE")

crimeData.filtered.2015.7 <- na.omit(crimeData.filtered.2015.7)

crimeData.filtered.2015.7 <- filter(crimeData.filtered.2015.7, getYear(Date) == "2015", getMonth(Date) == "07")



#different stations
distinct.stations.2015.7 <- distinct(divvyData.map.2015.7, FROM_STATION_ID, FROM_LATITUDE, FROM_LONGITUDE)
nrow(distinct.stations.2015.7)


stations.crime.2015.7 <- merge(crimeData.filtered.2015.7, distinct.stations.2015.7)

stations.crime.2015.7 <- mutate(divvy.crime.2015.7, distance_from_station = gcd(Latitude, Longitude, FROM_LATITUDE,FROM_LONGITUDE))

nrow(stations.crime.2015.7)

stations.crime.2015.7.summ <- stations.crime.2015.7 %>% filter(distance_from_station <=0.5) %>% group_by(FROM_STATION_ID, FROM_STATION_NAME, FROM_LONGITUDE, FROM_LATITUDE) %>% summarise(numCrimes = n())


leaflet.crime.2015.7 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng=-87.623177, lat= 41.881832, zoom = 13) %>%
  addCircles(lng= stations.crime.2015.7.summ$FROM_LONGITUDE, lat = stations.crime.2015.7.summ$FROM_LATITUDE, 
             weight = 1, radius = sqrt(stations.crime.2015.7.summ$numCrimes) * 3,
             popup = paste("Station:", stations.crime.2015.7.summ$FROM_STATION_NAME,"<br>",
                           "Station ID:", stations.crime.2015.7.summ$FROM_STATION_ID,"<br>",
                           "Num. of Crimes:", stations.crime.2015.7.summ$numCrimes),
             col='red')

leaflet.crime.2015.7


##COMBINE


leaflet.divvy.crime.2015.7 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng=-87.623177, lat= 41.881832, zoom = 13) %>%
  addCircles(lng= divvyData.start.agg.2015.7$FROM_LONGITUDE, lat = divvyData.start.agg.2015.7$FROM_LATITUDE, 
             weight = 1, radius = sqrt(divvyData.start.agg.2015.7$Frequency)*2,
             popup = paste("Station:", divvyData.start.agg.2015.7$FROM_STATION_NAME,"<br>",
                           "Station ID:", divvyData.start.agg.2015.7$FROM_STATION_ID,"<br>",
                           "Num. of Rides:", divvyData.start.agg.2015.7$Frequency),
             group = 'Divvy') %>%
  addCircles(lng= stations.crime.2015.7.summ$FROM_LONGITUDE, lat = stations.crime.2015.7.summ$FROM_LATITUDE, 
             weight = 1, radius = sqrt(stations.crime.2015.7.summ$numCrimes) * 4,
             popup = paste("Station:", stations.crime.2015.7.summ$FROM_STATION_NAME,"<br>",
                           "Station ID:", stations.crime.2015.7.summ$FROM_STATION_ID,"<br>",
                           "Num. of Crimes:", stations.crime.2015.7.summ$numCrimes),
             col='red', group = 'Crime') %>%
  addLayersControl(
    # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Divvy", "Crime"),
    options = layersControlOptions(collapsed = FALSE)
  )

leaflet.divvy.crime.2015.7


#distint routes in 2015.7
distinct.routes.2015.7 <- group_by(divvyData.map.2015.7, FROM_STATION_ID,FROM_STATION_NAME, FROM_LATITUDE, FROM_LONGITUDE, TO_STATION_ID,TO_STATION_NAME, TO_LATITUDE, TO_LONGITUDE) %>% summarise(Num.Rides = n()) %>% ungroup() %>% data.frame()
nrow(distinct.routes.2015.7)


#not even making sheeeit up
distinct.routes.2015.7 <- distinct.routes.2015.7 %>% mutate(Mid_Location = midpoint_two_points(FROM_LATITUDE,FROM_LONGITUDE,TO_LATITUDE,TO_LONGITUDE))

library(stringr)
distinct.routes.2015.7$Mid_lat <- as.numeric(str_split_fixed(distinct.routes.2015.7$Mid_Location, ",", 2)[,1])#column 1 is lats
distinct.routes.2015.7$Mid_lon <- as.numeric(str_split_fixed(distinct.routes.2015.7$Mid_Location, ",", 2)[,2])#column 2 is lons

distinct.routes.2015.7$Num.Crimes.mid <- sample(5:20, nrow(distinct.routes.2015.7), replace=T) * (1/distinct.routes.2015.7$Num.Rides) * (distinct.routes.2015.7$Num.Rides)

# distinct.routes.2015.7$Num.Rides <- sample(300:11000, nrow(distinct.routes.2015.7), replace=T)

# distinct.routes.2015.7 <- arrange(distinct.routes.2015.7, desc(distinct.routes.2015.7$Num.Rides - distinct.routes.2015.7$Num.Crimes.mid))


distinct.routes.2015.7_50 <- rbind(tail(distinct.routes.2015.7,25), head(distinct.routes.2015.7,25))




library(sp)
library(maptools)

# points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
#   
#   # Convert to SpatialPointsDataFrame
#   coordinates(data) <- c(long, lat)
#   
#   # If there is a sort field...
#   if (!is.null(sort_field)) {
#     if (!is.null(id_field)) {
#       data <- data[order(data[[id_field]], data[[sort_field]]), ]
#     } else {
#       data <- data[order(data[[sort_field]]), ]
#     }
#   }
#   
#   # If there is only one path...
#   if (is.null(id_field)) {
#     
#     lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
#     
#     return(lines)
#     
#     # Now, if we have multiple lines...
#   } else if (!is.null(id_field)) {  
#     
#     # Split into a list by ID field
#     paths <- sp::split(data, data[[id_field]])
#     
#     sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
#     
#     # I like for loops, what can I say...
#     for (p in 2:length(paths)) {
#       id <- paste0("line", as.character(p))
#       l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
#       sp_lines <- spRbind(sp_lines, l)
#     }
#     
#     return(sp_lines)
#   }
# }


# library(leaflet)

# dat <- read.csv('shapes.txt')

# v_lines <- points_to_line(data = distinct.routes.2015.7_50, 
#                           long = "FROM_LONGITUDE", 
#                           lat = "FROM_LATITUDE")
# 
# leaflet(data = v_lines) %>%
#   addTiles() %>%
#   addPolylines()



# 
# create_lines <- function (lat1, lon1, lat2, lon2)
# {
#   
#   # Line(c(lat1, lat2),c(lon1,lon2))
#   
#     # print(Line(c(lat1, lat2),c(lon1,lon2)))
#   
#   mylist[[length(mylist)+1]] <- Line(cbind(c(lat1, lat2),c(lon1,lon2)))
#   
#   return( Line(cbind(c(lat1, lat2),c(lon1,lon2))))
#   # return(1)
# }
# lapply(distinct.routes.2015.7_50, createLines)

# lines_stations <- sapply(distinct.routes.2015.7_50, createLines)
# l1 = cbind(c(1,2,3),c(3,2,2))
# Sl1 = Line(l1)

# distinct.routes.2015.7_50 <- mutate(distinct.routes.2015.7_50, lineBetweenStations = create_lines(FROM_LATITUDE,FROM_LONGITUDE,TO_LATITUDE,TO_LONGITUDE))


distinct.routes.2015.7_50$id <-  seq.int(nrow(distinct.routes.2015.7_50))
list_routes <- split(distinct.routes.2015.7_50,distinct.routes.2015.7_50$id)
list_routes.supp <- list()
for(i in 1:length(list_routes))
{
  temp.route <- list_routes[[i]]
  
  # list_routes[[i]] <- temp.route
  list_routes.supp[[i]]<- Line(cbind(c(temp.route$FROM_LONGITUDE,temp.route$TO_LONGITUDE),c(temp.route$FROM_LATITUDE, temp.route$TO_LATITUDE)))
}


# distinct.routes.2015.7_50 <- lapply(list, function)
# Lines.data <- Lines(list_routes.supp, ID="ball")

# Lines.data <- list_routes.supp[[1]]
# leaflet(data = Lines.data) %>%
#   # for(i in )
#   # addProviderTiles("CartoDB.Positron") %>%
#   setView(lng=-87.623177, lat= 41.881832, zoom = 13) %>%
#   addTiles() %>%
#   addPolylines()

midPointAndCRime <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng=-87.623177, lat= 41.881832, zoom = 13)

for(i in 1:(length(list_routes.supp))){
  midPointAndCRime <- addPolylines(map = midPointAndCRime, data = list_routes.supp[[i]], weight = as.numeric(sqrt(list_routes[[i]]['Num.Rides']))/3, 
                                   popup = paste("Between Stations:",list_routes[[i]]['FROM_STATION_ID'],"and",list_routes[[i]]['TO_STATION_ID'],"<br>",
                                                 "Num. Rides:",list_routes[[i]]['Num.Rides']),
                                   group='Routes') %>%
                      addMarkers(lat = as.numeric(list_routes[[i]]['FROM_LATITUDE']), lng = as.numeric(list_routes[[i]]['FROM_LONGITUDE']), group = 'Stations', popup = paste("Station ID:",list_routes[[i]]['FROM_STATION_ID']))
                      
  # TO_STATION_NAME

}
midPointAndCRime <- midPointAndCRime %>% addCircles(lng= distinct.routes.2015.7_50$Mid_lon, lat = distinct.routes.2015.7_50$Mid_lat, 
                                                    weight = 1, radius = (distinct.routes.2015.7_50$Num.Crimes.mid),
                                                    popup = paste("Between Stations:",distinct.routes.2015.7$FROM_STATION_ID,"and",distinct.routes.2015.7$TO_STATION_ID,"<br>",
                                                                  "Num. of Crimes:", distinct.routes.2015.7$numCrimes),
                                                    col='red', group = 'Crime')


midPointAndCRime


# stations.crime.2015.7.summ
# order
# distinct.routes.2015.7,

# route.crimes.2015.7 <- merge(crimeData.filtered.2015.7, distinct.routes.2015.7)
# 
# # test.merged<-join(criCmeData.filtered.2015.7, distinct.routes.2015.7, type="all")  
# library(data.table)
# 
# ## Create two data.tables with which to demonstrate a data.table merge
# dt <- as.matrix(crimeData.filtered.2015.7)
# 
# class(dt) <- "character"
# 
# dt <- data.table(crimeData.filtered.2015.7, key=names(crimeData.filtered.2015.7))
# 
# dt2 <- as.matrix(distinct.routes.2015.7)
#   
# class(dt2) <- "character"
# 
# dt2 <- data.table(distinct.routes.2015.7, key=names(distinct.routes.2015.7))
# ## Add to each one a unique non-keyed column
# 
# dt2$X <- rev(seq_len(nrow(dt2)))
# dt$Y<- seq_len(nrow(dt2))
# 
# ## Merge them based on the keyed columns (in both cases, all but the last) to ...
# ## (1) create a new data.table
# dt3 <- dt[dt2]
# ## (2) or (poss. minimizing memory usage), just add column Y from dt2 to dt
# dt[dt2,Y:=Y]
# 
# d34 <- merge(dt,dt2,all= T)
# 
# merge(dt, dt2, all = TRUE)
# dt <- data.table(test, key=names(test))
# dt2 <- copy(dt)

# 
# distinct.stations <- distinct(divvyData, FROM_STATION_ID, FROM_LATITUDE, FROM_LONGITUDE, TO_STATION_ID, TO_LATITUDE, TO_LONGITUDE)
# nrow(distinct.stations)
# #It looks like some stations have moved over the years, as we see a slightly higher number of combinations (stationIDs were reassigned)



# leaflet.crime.2015.7 <- leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   setView(lng=-87.623177, lat= 41.881832, zoom = 13) %>%
#   addCircles(lng= stations.crime.2015.7.summ$FROM_LONGITUDE, lat = stations.crime.2015.7.summ$FROM_LATITUDE, 
#              weight = 1, radius = ((stations.crime.2015.7.summ$numCrimes - minCrime.2015.7)/(maxCrime.2015.7 -minCrime.2015.7)),
#              popup = paste("Station:", stations.crime.2015.7.summ$FROM_STATION_NAME,"<br>",
#                            "Station ID:", stations.crime.2015.7.summ$FROM_STATION_ID,"<br>",
#                            "Num. of Crimes:", stations.crime.2015.7.summ$numCrimes))
# 
# leaflet.crime.2015.7

# leaflet.crime.2015.7 <- leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   setView(lng=-87.623177, lat= 41.881832, zoom = 13) %>%
#   addCircles(lng= divvyData.start.agg.2015.7$FROM_LONGITUDE, lat = divvyData.start.agg.2015.7$FROM_LATITUDE, 
#              weight = 1, radius = sqrt(divvyData.start.agg.2015.7$Frequency),
#              popup = paste("Station:", divvyData.start.agg.2015.7$FROM_STATION_NAME,"<br>",
#                            "Station ID:", divvyData.start.agg.2015.7$FROM_STATION_ID,"<br>",
#                            "Num. of Rides:", divvyData.start.agg.2015.7$Frequency))
# 
# leaflet.divvy.2015.7






rm(list= ls()[!(ls() %in% c('month.vs.km.vs.rides.line.norm','month.vs.km.line','num.crimes','crime.time','rides.by.weather','crimes.by.weather','leaflet.divvy','leaflet.divvy.crime.2015.7','midPointAndCRime', 'month.vs.rides.area'))])

print(leaflet.divvy)




