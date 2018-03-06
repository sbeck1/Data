###### Cleaning up and merging Transect data #########

#load packages
library(readxl) #to read xlsx file
library(tidyverse)
library(chron)

#load data
tran.2017 = read.csv("Oyster_data/Transect/LCR_oyster_transect_2017.csv", header = TRUE, stringsAsFactors = FALSE) #this is a combo of 2017 and 2013
tran.2018 = read_excel("Oyster_data/Transect/OysterData_30Jan2018_Transect.xlsx")
tran.2012 = read.csv("Oyster_data/Transect/Transect_data_Nov_2012_bp.csv", stringsAsFactors = FALSE) #actually 2010 data?

tran.2018 = as.data.frame(tran.2018)
#1. Remove Location data from 2017 and 2018 data and make separate location files

loc.2012 = tran.2012 %>% select(Date, Station, StartGPS_E, StartGPS_N, MidGPS_E, MidGPS_N, EndGPS_E, EndGPS_N, Orientation) %>% distinct()
loc.2012 = loc.2012[complete.cases(loc.2012),]
loc.2012[,5] = as.integer(loc.2012[,5])
loc.2012[,6] = as.integer(loc.2012[,6])
loc.2012[,7] = as.integer(loc.2012[,7])
loc.2012[,8] = as.integer(loc.2012[,8])
loc.2012[,9] = as.integer(loc.2012[,9])

loc.2018 = tran.2018 %>% select(Date, Station, StartGPS_E, StartGPS_N, MidGPS_E, MidGPS_N, EndGPS_E, EndGPS_N, Orientation) %>% distinct()
loc.2018 = loc.2018[1,] #it was just a repeat but one row was missing orientation so it was double. 


transect.locations = rbind(loc.2012, loc.2018)
#write.csv(transect.locations, "transect.locations.csv")

tran.2012 = tran.2012[,-c(10:16)] #removing locations 
tran.2018 = tran.2018[,-c(12:18)] #removing locations

## 2. Clean transect data frames to merge

#cleaning 2012 Epoch
tran.2012 = tran.2012[,-c(12:13)] #remove extra counts
colnames(tran.2012)[12] = "Cnt_Live" #change Cnt to cnt live
tran.2012$Cnt_Dead = rep("NA", 2546) #add in Cnt_dead col 
tran.2012$Date = as.Date(tran.2012$Date, format = "%m/%d/%Y") #change Date to date format

stime = tran.2012$Start.time 
stime.1 = strsplit(stime, ":")  #split time by :

time = vector()
for(i in 1: length(stime.1)){   #loop to paste time back together but only the ones that have real time
  
  time[i] = ifelse(length(stime.1[[i]]) == 2, paste0(stime.1[[i]][1], ":", stime.1[[i]][2]), "NA")
  
}

tran.2012$Start.time = time


etime = tran.2012$End.time 
etime.1 = strsplit(etime, ":")  #split time by :

time = vector()
for(i in 1: length(etime.1)){   #loop to paste time back together but only the ones that have real time
  
  time[i] = ifelse(length(etime.1[[i]]) == 2, paste0(etime.1[[i]][1], ":", etime.1[[i]][2]), "NA")
  
}

tran.2012$End.time = time

#clean 2018 data 
tran.2018 = tran.2018[,-c(4,5,13)] #removing Day, year, Recorder to match 2012 file
colnames(tran.2018)[4] = "Start.time" #change col name to match 2012
colnames(tran.2018)[5] = "End.time"   #change col name to match 2012

tran.2018$Start.time= strftime(tran.2018$Start.time, format = "%H:%M") #change time to match 2012
tran.2018$End.time = strftime(tran.2018$End.time, format = "%H:%M")   #change time to match 2012

##### FIX 2017 DATA ####
#missing a lot of cols that the other two have...

#pulling out month to match other transect data frames
Month = rep(NA, dim(tran.2017)[1])
  for(i in 1:dim(tran.2017)[1]){
    
    Month[i] = strsplit(tran.2017$Date, split = "/")[[i]][1]
  }  
tran.2017$Month = Month

#Renaming live and dead count
colnames(tran.2017)[5]= "Cnt_Live"
colnames(tran.2017)[6] = "Cnt_Dead"

#removing tran_width_m

tran.2017 = tran.2017[,-8]

#creating Trip number by unique date

index = cbind(unique(tran.2017$Date), seq(1,9))
index1 = match(tran.2017$Date,index[,1])
tran.2017$Trip = index[index1,2]

#add in empty start, end time, counter
tran.2017$Start.time = rep(NA, 630)
tran.2017$End.time = rep(NA, 630)
tran.2017$Counter = rep(NA, 630)

#rename distance to translength
colnames(tran.2017)[4] = "TransLngth"

#add in Locality, Site, Bar, Station 
tran.2017$Locality = rep("LC", dim(tran.2017)[1]) #create locality
tran.2017$Site = rep("O", dim(tran.2017)[1])  #create site

## Dealing with Treatment, pre vs post; control vs restore 
tran.2017$Treatment = apply(tran.2017, 1, function(x) ifelse(str_detect(x[2], pattern = "LCrestore"), paste0("restore", "_", x[7]), paste0("control", "_", x[7])))

#split bar number from station
tran.2017$Bar = apply(tran.2017, 1, function(x) ifelse(str_detect(x[2], pattern = "LCrestore") , strsplit(x[2], "e")[[1]][3], strsplit(x[2], "l")[[1]][2]))
colnames(tran.2017)[2] = "Station"  #rename location to station
colnames(tran.2017)[3] = "Replicate"  #rename transect number to replicate on bar
tran.2017 = tran.2017[,c(1,8,10,11, 13, 14, 16, 2, 12,4,5,6,3, 15)]#rearrange to match 2012 
tran.2017$Month = month.name[as.numeric(tran.2017$Month)] #change month numbers to month names
tran.2017$Date = as.Date(tran.2017$Date, format = "%m/%d/%Y") #change date from character to actual date


#Combind 2012 and 2018 data 
transect = rbind(tran.2012, tran.2018)
transect = transect[,-2] #remove trip number
transect$Replicate = rep("1", dim(transect)[1])  #add in 1 for replicate because 2017 data has up to 3 replicates
transect$Treatment = rep("control", dim(transect)[1])

transect = rbind(transect, tran.2017)

#write.csv(transect, "transect_combined.csv")

