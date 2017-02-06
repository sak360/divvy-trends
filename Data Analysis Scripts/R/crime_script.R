library(dplyr)
library(ggplot2)
library(plotly)
library(gdata)

setwd("C:\\Users\\skhan231\\Desktop\\TDI_Challenge\\R")

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
