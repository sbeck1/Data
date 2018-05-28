#### Updating Quadrat Naming Conventions ##########

#read in data
quadurl = getURL("https://raw.githubusercontent.com/LCRoysterproject/Data/master/Oyster_data/Quadrat/quadrat_combined.csv")
quad = read.csv(text = quadurl)

#change quad$Date to actual date
quad$Date = as.Date(quad$Date, format = "%m/%d/%Y")

#bring in new naming conventions
name = read.csv("/Users/katiezarada/Desktop/Oysters/station_name_change.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

# Some of the LCO's are listed as LC0, which is causing problems. 
#Fixing that by pasting the Locality, Site, and Bar together to replace station 
quad %>% select(Locality,Site, Bar, Station) %>% distinct() #this is to check that the pasting will work

quad$Station = paste0(quad$Locality, quad$Site, quad$Bar)

#Create A, B, C Designations 

quad$Location = rep(NA, dim(quad)[1])
quad$Location[which(quad$Date < "2013-01-01" & quad$Station == "LCO1")] <- "B"
quad$Location[which(quad$Date < "2013-01-01" & quad$Station == "LCO2")] <- "A"

quad$Location[which(quad$Date > "2013-01-01" & quad$Station == "LCO1")] <- "B"
quad$Location[which(quad$Date > "2013-01-01" & quad$Station == "LCO2")] <- "A"
quad$Location[which(quad$Date > "2013-01-01" & quad$Station == "LCO3")] <- "C"
quad$Location[which(quad$Date > "2013-01-01" & quad$Station == "LCO4")] <- "A"
quad$Location[which(quad$Date > "2013-01-01" & quad$Station == "LCO5")] <- "B"
quad$Location[which(quad$Date > "2013-01-01" & quad$Station == "LCO6")] <- "A"
quad$Location[which(quad$Date > "2013-01-01" & quad$Station == "LCO7")] <- "B"


quad.pre13 = subset(quad, quad$Date < "2013-01-01")
quad.post13 = subset(quad, quad$Date > "2013-01-01")

name.pre13 = name[1:3, ]
name.post13 = name[4:21,]

index.pre = match(quad.pre13$Station, name.pre13$Current)
index.pre1 = index.pre[!is.na(index.pre)]
quad.pre13$Station[which(index.pre != "NA")] = name.pre13[index.pre1,2] #changing name 

index.post = match(quad.post13$Station, name.post13$Current)
index.post1 = index.post[!is.na(index.post)]
quad.post13$Station[which(index.post != "NA")] = name.post13[index.post1,2] #changing name 

quad = rbind(quad.pre13, quad.post13)

#check if the switch worked
quad %>% select(Date, Station, Location) %>% distinct()

####### Fix col splits ##########
quad$Locality = substr(quad$Station, 1, 2)
quad$Site = substr(quad$Station, 3,3)
quad$Bar = substr(quad$Station, 4,5)

# Make a Sub station col for combo of Station + Location 

Substation = ifelse(quad$Location != "NA", paste0(quad$Station, quad$Location), paste(NA))
index = which(is.na(quad$Location))
Substation[index]= quad$Station[index]
quad$Substation = Substation



write.csv(quad, "quadrat_combined.csv")

