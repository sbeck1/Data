###### Cleaning up and merging Transect data #########

#set workspace
#setwd("/Users/katiezarada/Desktop/Oysters/Analyses") don't need to set the directory since this is a project
#load packages
library(readxl) #to read xlsx file
library(tidyverse)
library(chron)

#load data
tran.2017 = read.csv("Oyster_data/Transect/LCR_oyster_transect_2017.csv", header = TRUE, stringsAsFactors = FALSE) #this is a combo of 2017 and 2013
tran.2018 = read_excel("Oyster_data/Transect/OysterData_30Jan2018_Transect.xlsx")
tran.2012 = read.csv("Oyster_data/Transect/Transect_data_Nov_2012_bp.csv", stringsAsFactors = FALSE) #actually 2010 data?
tran.2015 = read_excel("Oyster_data/Transect/2015_transect.xlsx")
tran.2018 = as.data.frame(tran.2018)
tran.2015 = as.data.frame(tran.2015)
#1. Remove Location data from 2017 and 2018 data and make separate location files

loc.2012 = tran.2012 %>% select(Date, Station, StartGPS_E, StartGPS_N, MidGPS_E, MidGPS_N, EndGPS_E, EndGPS_N, Orientation) %>% distinct()
loc.2012 = loc.2012[complete.cases(loc.2012),]
loc.2012[,5] = as.integer(loc.2012[,5])
loc.2012[,6] = as.integer(loc.2012[,6])
loc.2012[,7] = as.integer(loc.2012[,7])
loc.2012[,8] = as.integer(loc.2012[,8])
loc.2012[,9] = as.integer(loc.2012[,9])
loc.2012$Date = as.Date(loc.2012$Date, format = "%m/%d/%Y")
loc.2018 = tran.2018 %>% select(Date, Station, StartGPS_E, StartGPS_N, MidGPS_E, MidGPS_N, EndGPS_E, EndGPS_N, Orientation) %>% distinct()
loc.2018 = loc.2018[1,] #it was just a repeat but one row was missing orientation so it was double. 


transect.locations = rbind(loc.2012, loc.2018)

transect.locations[which(transect.locations$Station == "LT1"),2] <- "LTI1"

#changing site names to match the new names 
name = read.csv("station_name_change.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
index = match(transect.locations$Station, name$Current)
index.1 = index[!is.na(index)]
transect.locations$Station[which(index != "NA")] = name[index.1,2] #changing name 

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
tran.2017$Treatment = apply(tran.2017, 1, function(x) ifelse(str_detect(x[2], pattern = "LCrestore"), paste0("restore", "_", x[7]), paste0("control")))
tran.2017$Treatment[which(tran.2017$Treatment == "restore_pre")] <- "control"
tran.2017$Treatment[which(tran.2017$Treatment == "restore_post")] <- "rocks"


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




##################################
###  cleaning up 2015 Data #######
##################################

#create month column 
tran.2015$Month = month.name[as.numeric(format(tran.2015$Date, format = "%m"))]

#add in empty start, end time, counter
tran.2015$Start.time = rep(NA, dim(tran.2015)[1])
tran.2015$End.time = rep(NA, dim(tran.2015)[1])
tran.2015$Counter = rep(NA, dim(tran.2015)[1])

#add in Locality, Site, Bar, Station 
tran.2015$Locality = rep("LC", dim(tran.2015)[1]) #create locality
tran.2015$Site = rep("O", dim(tran.2015)[1])  #create site

#create bar number 
tran.2015$Bar = apply(tran.2015, 1, function(x) ifelse(str_detect(x[2], pattern = "LCcontrol") , strsplit(x[2], "l")[[1]][2], strsplit(x[2], "C")[[1]][2]))

#change names of columns 
colnames(tran.2015)[2] = "Station"
colnames(tran.2015)[4] = "Replicate"
colnames(tran.2015)[5] = "TransLngth"
colnames(tran.2015)[6] ="Cnt_Live"
colnames(tran.2015)[7]= "Cnt_Dead"

#treatment column 

tran.2015$Treatment = apply(tran.2015, 1, function(x) ifelse((x[14] == "01" | x[14] == "02" | x[14] == "05"|x[14] ==  "06"), "rocks", "control"))

#change order 

tran.2015 = tran.2015[,c(1,8,9,10,12,13,14, 2, 11, 5,6, 7, 4, 15)]


#combine 

transect = rbind(transect, tran.2015)


#changing LCrestore to regular site name 
name = read.csv("station_name_change.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
name.1 = name[22:37,1:2]
index = match(transect$Station, name.1$Current)
index.1 = index[!is.na(index)]
transect$Station[which(index != "NA")] = name.1[index.1,2] #changing name 

#create A,B,C designation 
transect$Location = rep(NA, dim(transect)[1])
transect$Location[which(transect$Date < "2013-01-01" & transect$Station == "LCO1")] <- "B"
transect$Location[which(transect$Date < "2013-01-01" & transect$Station == "LCO2")] <- "A"

transect$Location[which(transect$Date > "2013-01-01" & transect$Station == "LCO1")] <- "B"
transect$Location[which(transect$Date > "2013-01-01" & transect$Station == "LCO2")] <- "A"
transect$Location[which(transect$Date > "2013-01-01" & transect$Station == "LCO3")] <- "C"
transect$Location[which(transect$Date > "2013-01-01" & transect$Station == "LCO4")] <- "A"
transect$Location[which(transect$Date > "2013-01-01" & transect$Station == "LCO5")] <- "B"
transect$Location[which(transect$Date > "2013-01-01" & transect$Station == "LCO6")] <- "A"
transect$Location[which(transect$Date > "2013-01-01" & transect$Station == "LCO7")] <- "B"



# changing site names to new format
#problem that sites prior to 2013 have same current name but different later name than post 2013 data

name = read.csv("station_name_change.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
transect.pre13 = subset(transect, transect$Date < "2013-01-01")
transect.post13 = subset(transect, transect$Date > "2013-01-01")

name.pre13 = name[1:3, ]
name.post13 = name[4:21,]

index.pre = match(transect.pre13$Station, name.pre13$Current)
index.pre1 = index.pre[!is.na(index.pre)]
transect.pre13$Station[which(index.pre != "NA")] = name.pre13[index.pre1,2] #changing name 

index.post = match(transect.post13$Station, name.post13$Current)
index.post1 = index.post[!is.na(index.post)]
transect.post13$Station[which(index.post != "NA")] = name.post13[index.post1,2] #changing name 

transect = rbind(transect.pre13, transect.post13)

#check if the switch worked
transect %>% select(Date, Station, Location) %>% distinct()

####### Fix col splits ##########
transect$Locality = substr(transect$Station, 1, 2)
transect$Site = substr(transect$Station, 3,3)
transect$Bar = substr(transect$Station, 4,5)

# Make a Sub station col for combo of Station + Location 

Substation = ifelse(transect$Location != "NA", paste0(transect$Station, transect$Location), paste(NA))
index = which(is.na(transect$Location))
Substation[index]= transect$Station[index]
transect$Substation = Substation

#check if the col split worked

transect %>% select(Locality, Site, Bar, Station, Substation) %>% distinct()

#Noticed issues where 0s where in place of Os in the Site, and also upper and lower cas discrepancies in tran$Site

#Changing the 0s to Os in the Site Column

transect$Site[transect$Site == "0"] <- "O"

#Making all of the letters uppercase, this might cause issues since R is case sensitive

transect$Site<-toupper(transect$Site)

write.csv(transect, "transect_combined.csv")











