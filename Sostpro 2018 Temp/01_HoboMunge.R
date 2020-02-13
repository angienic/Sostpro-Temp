library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(knitr)
###Read in data and get rows and columns sorted, change the file name and csv output manually
site.name <- "6_Coast"

#Air
Air<-data.frame(read.csv("6Coast_Air.csv",
                       header=F,stringsAsFactors=FALSE,
                       colClasses=c(rep("character",4),rep("NULL",5)),
                       na.string=c("","null","NaN","X")))
colnames(Air) <- c("Record", "DateTime", "Temp.C", "Light.Lux")
Air = Air[-2,]
Air = Air[-1,]

Air$DateTime <- strptime(Air$DateTime, "%m/%d/%y %I:%M:%S %p")
Air$DateTime <- as.POSIXct(Air$DateTime, origin="1970-01-01", tz="GMT")
Air$Temp.C <- as.numeric(as.character(Air$Temp.C))
Air$Light.Lux <- as.numeric(as.character(Air$Light.Lux))

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

Air <- Air[Air$DateTime %within% int,]
Water <- Water[Water$DateTime %within% int,]

#Replace light values = 0 (night time) with NA
Air$Light.Lux[Air$Light.Lux==0] <- NA
Water$Light.Lux[Water$Light.Lux==0] <- NA

#Get summary of data
summary(Air)
summary(Water)

#Plot all data
riptemp <- qplot(x=DateTime, y=Temp.C,
     data=Air, na.rm=TRUE,
     main="Riparian Air Temperature",
     xlab="Date", ylab="Temperature (°C)")
riptemp

riplight <- qplot(x=DateTime, y=Light.Lux,
      data=Air, na.rm=TRUE,
      main="Riparian Light Intensity",
      xlab="Date", ylab="Light Intensity (Lux)")
riplight

h20temp <- qplot(x=DateTime, y=Temp.C,
                 data=Water, na.rm=TRUE,
                 main="Stream Water Temperature",
                 xlab="Date", ylab="Temperature (°C)")
h20temp

h20light <- qplot(x=DateTime, y=Light.Lux,
                  data=Water, na.rm=TRUE,
                  main="Water Column Light Intensity",
                  xlab="Date", ylab="Light Intensity (Lux)")
h20light

#Plot light against temp
ggplot(Air, aes(x=Light.Lux, y=Temp.C)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 

ggplot(Water, aes(x=Light.Lux, y=Temp.C)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 

#Plot boxplots by day - show range in hourly variables by day
Air$Day <- cut(Air$DateTime, breaks = "day")
Water$Day <- cut(Water$DateTime, breaks = "day")

riptempbox <- ggplot(Air, aes(x=as.Date(DateTime), y=Temp.C, group=Day)) + 
  geom_boxplot() + 
  scale_x_date(date_breaks = "1 week", date_labels="%Y-%b-%d")
riptempbox

riplightbox <- ggplot(Air, aes(x=as.Date(DateTime), y=Light.Lux, group=Day)) + 
  geom_boxplot() + 
  scale_x_date(date_breaks = "1 week", date_labels="%Y-%b-%d")
riplightbox

h20tempbox <- ggplot(Water, aes(x=as.Date(DateTime), y=Temp.C, group=Day)) + 
  geom_boxplot() + 
  scale_x_date(date_breaks = "1 week", date_labels="%Y-%b-%d")
h20tempbox

h20lightbox <- ggplot(Water, aes(x=as.Date(DateTime), y=Light.Lux, group=Day)) + 
  geom_boxplot() + 
  scale_x_date(date_breaks = "1 week", date_labels="%Y-%b-%d")
h20lightbox

#Plot daily means for air and water
AirMean<-aggregate(cbind(Temp.C, Light.Lux)~Day, Air, mean)
WaterMean<-aggregate(cbind(Temp.C, Light.Lux)~Day, Water, mean)

riptemp <- qplot(x=Day, y=Temp.C,
                 data=AirMean, na.rm=TRUE,
                 main="Riparian Air Temperature",
                 xlab="Date", ylab="Temperature (°C)")
riptemp

riplight <- qplot(x=Day, y=Light.Lux,
                  data=AirMean, na.rm=TRUE,
                  main="Riparian Light Intensity",
                  xlab="Date", ylab="Light Intensity (Lux)")
riplight

h20temp <- qplot(x=Day, y=Temp.C,
                 data=WaterMean, na.rm=TRUE,
                 main="Stream Water Temperature",
                 xlab="Date", ylab="Temperature (°C)")
h20temp

h20light <- qplot(x=Day, y=Light.Lux,
                  data=WaterMean, na.rm=TRUE,
                  main="Water Column Light Intensity",
                  xlab="Date", ylab="Light Intensity (Lux)")
h20light

ggplot(AirMean, aes(x=Light.Lux, y=Temp.C)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 

ggplot(WaterMean, aes(x=Light.Lux, y=Temp.C)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 

#calculate Mean, Min, Max, and CV for all four time series and send to new file.
AirTemp_Mean <- mean(Air$Temp.C)
AirTemp_Min <- min(Air$Temp.C)
AirTemp_Max <- max(Air$Temp.C)
AirTemp_SD <- sd(Air$Temp.C)
AirTemp_CV <- (AirTemp_SD/AirTemp_Mean)

AirLight_Mean <- mean(Air$Light.Lux, na.rm=TRUE)
AirLight_Min <- min(Air$Light.Lux, na.rm=TRUE)
AirLight_Max <- max(Air$Light.Lux, na.rm=TRUE)
AirLight_SD <- sd(Air$Light.Lux, na.rm=TRUE)
AirLight_CV <- (AirLight_SD/AirLight_Mean)

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

