#Sostpro 2018 Max.Temperatures. Original C.Ruffing, Edited by A.Nicolas Feb 2020.

library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(knitr)

###Read in data and get rows and columns sorted, change the file name and csv output manually
site.name <- "6_Coast"

#Water
Water<-data.frame(read.csv("6Coast_Water.csv",
                           header=F,stringsAsFactors=FALSE,
                           colClasses=c(rep("character",4),rep("NULL",5)),
                           na.string=c("","null","NaN","X")))
colnames(Water) <- c("Record", "DateTime", "Temp.C", "Light.Lux")
Water = Water[-2,]
Water = Water[-1,]

Water$DateTime <- strptime(Water$DateTime, "%m/%d/%y %I:%M:%S %p")
Water$DateTime <- as.POSIXct(Water$DateTime, origin="1970-01-01", tz="GMT")
Water$Temp.C <- as.numeric(as.character(Water$Temp.C))
Water$Light.Lux <- as.numeric(as.character(Water$Light.Lux))


#Subset by deployment dates
dates <-read.csv("LoggerDates.csv")
dates$Start <- strptime(dates$Start, "%m/%d/%Y")
dates$End <- strptime(dates$End, "%m/%d/%Y")
dates$Start <- as.POSIXct(dates$Start, origin="1970-01-01", tz="GMT")
dates$End <- as.POSIXct(dates$End, origin="1970-01-01", tz="GMT")

site.date <- dates %>%
  filter(Site == site.name) %>%
  head

int <- interval(site.date$Start, site.date$End)

Water <- Water[Water$DateTime %within% int,]

#Replace light values = 0 (night time) with NA
Water$Light.Lux[Water$Light.Lux==0] <- NA

#Get summary of data
summary(Water)

####.......

#By day - show range in hourly variables by day.Function 'cut' divides X into intervals
Water$Day <- cut(Water$DateTime, breaks = "day")

#Optional Claire's descriptive stats for all four time series, sent to new file.
WaterTemp_Mean <- mean(Water$Temp.C)
WaterTemp_Min <- min(Water$Temp.C)
WaterTemp_Max <- max(Water$Temp.C)
WaterTemp_SD <- sd(Water$Temp.C)
WaterTemp_CV <- (WaterTemp_SD/WaterTemp_Mean)

WaterLight_Mean <- mean(Water$Light.Lux, na.rm=TRUE)
WaterLight_Min <- min(Water$Light.Lux, na.rm=TRUE)
WaterLight_Max <- max(Water$Light.Lux, na.rm=TRUE)
WaterLight_SD <- sd(Water$Light.Lux, na.rm=TRUE)
WaterLight_CV <- (WaterLight_SD/WaterLight_Mean)

DescStats <- cbind(site.name, AirTemp_Mean, AirTemp_Min, AirTemp_Max, AirTemp_SD, AirTemp_CV,
                   AirLight_Mean, AirLight_Min, AirLight_Max, AirLight_SD, AirLight_CV,
                   WaterTemp_Mean, WaterTemp_Min, WaterTemp_Max, WaterTemp_SD, WaterTemp_CV,
                   WaterLight_Mean, WaterLight_Min, WaterLight_Max, WaterLight_SD, WaterLight_CV)  

###Change name of csv file to match site
write.csv(DescStats, file="O6Coast_HoboSummary.csv")

###### First, calculate daily max temp
WaterDayMax<-aggregate(Temp.C~Day, Water, max)
WaterDayMax$MaxDaily<-WaterDayMax$Temp.C

#Optional quick plot
##Quick view
h20daymax <- qplot(x=Day, y=Temp.C,
                   data=WaterDayMax, na.rm=TRUE,
                   main="Max.Daily Water Temp",
                   xlab="Date", ylab="Temp (°C)")
h20daymax
ggplot(WaterDayMax, aes(x=Day, y=Temp.C)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
##

# Renaming result column
WaterDayMax %>% 
  rename(
    DayTMax = Temp.C,
  ) #prints it

#base R version
names(WaterDayMax)[names(WaterDayMax) == "Temp.C"] <- "T.Max"

#Calculate runing means (zoo pckg)
rollmean(WaterDayMax$Temp.C, 7) #John asked for 7 day means (excludes first 6d).
#Add to dataframe
WaterDayMax$Day7Mean<-rollmean(WaterDayMax$T.Max, 7,na.pad = T, align="right") #align =center gives NAs in first 3 and last 3 values, align left=, at the end and so on.
#Save as file
write.csv(WaterDayMax, file="O6Coast_TMax.csv")

#Repeat for other sites

