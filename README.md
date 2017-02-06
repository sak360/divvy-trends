# divvy-trends
__Deployed on Heroku at:__http://divvy-trends.herokuapp.com/

The purpose of this project is to study Chicago's Divvy data from June 2013 to June 2016 against weather and crime data between the same 3 year window.

The hypothesis is that crime reports will have an effect on the routes riders will take. The project will study the extent of these changes under different conditions. 

By studying the data, I would like to do the following:
- Infer the path that the rider took. The way to do this would be to look at average cycling speeds and the distance between two stations (start station AND end station) using either the Google Maps API or the Leaflet Routing Machine
- Look at trip trends between the same station. In doing so, I would like to see if there is an increase in trip durations between points where increased criminal activity had been reported (within some time window). Due to Chicago's weather, this will have to be studied seasonally. Additionally, similar days with similar riding conditions (rainy vs. sunny) will be studied separately.
- Look at trip trends at different times. Trip durations are expected to vary depending on the time of the day. By studying data in buckets so that every hour of the day falls into one bucket (less traffic, moderate traffic, lots of traffic), we are able to get more accurate expected durations.
- Next, I would like to find a threshold for each of these buckets to see that depending on the amount of traffic, how much criminal activity (and of what type) has to be reported to cause riders to take a longer path.
- Look at trip trends by gender. By studying the subset of the data related to specific genders, is there a difference in behavior (trip durations by hour and thresholds for taking longer routes).

##Data Sources
To complete this project, I will be extracting data from the following non-exhaustive list of sources:

- [City of Chicago - Crime data from June 2013 - present (210 MB)] (https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present-Map/c4ep-ee5m)
- [Divvy Chicago Data (1,750MB)] (https://www.divvybikes.com/system-data)
- Weather Data (Scraped using Beautiful Soup. Script in _Data Analysis Scripts_ folder)
