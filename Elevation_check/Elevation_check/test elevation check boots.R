
#August 2018
#this is a program that can be used to help think about the number of transects to measure on the reef elements. the program reads in the survey data we have in hand as of 8.11.2018 and then you choose which reef you want to then "resample" to calculate the average elevation for a given number of transects.  it then makes a plot that shows the mean elevations of each of the transects that you take and compares this to a reference line that is the target for the construction people to achieve.  

windows(record=T)

library("tidyverse")
library("MASS")
library("lubridate")

#dat<- read_csv("20180731_surv_el_trans.csv")
dat<- read_csv("elevation_check_data_bp.csv")


#dat$Date=mdy("7/31/2018") #need to add the sampling date

dat2<-subset(dat, select =c("Date","Transect", "Elev", "Element_id", "Type"))

dat2.1<-filter(dat2,dat2$Transect!=c("GS DIRT")) 
dat2.2<-filter(dat2.1,dat2.1$Type!=c("GS"))  

dat2<-dat2.2

#create new dataframe dat2 by mutating dat to create new elment id
#name 11c for the most southern 3 transects of 11b.  These transects
#are the new rock size transects. the notation is 7 or 8 or 9
dat3<-mutate(dat2, Element_id_2 = ifelse(Transect > 99, "11c", Element_id))

#table of survey points by transect and reef
n_element_trans<- dat3 %>%
  group_by(Element_id_2, Transect) %>%
  summarize(n())

#ok this is creating the new Element_id_2 as a factor for use in plotting below
dat3$Element_id_3<-factor(dat3$Element_id_2, 
      levels =c ("5", "7", "8a", "9b", "10b", "11b", "13"))

#"5", "7", "8a", "9b", "10b", "11b", "13"

#look at specific reefs
dat_reef<-filter(dat3, Element_id_3 == "7")

num_trans=4

#############################################################

#Below is a function that is just parked here that can be used to do #bootstrap CI using quantile method which is likely a more appropriate general solution to the CI estimation given the distribution of the data

#https://en.wikipedia.org/wiki/Bootstrapping_(statistics)

#change in the i in 1:X, the X is the number of transects
#you would potentially take

bstrap <- c()
for (i in 1:(num_trans)){
  bstrap <- c(bstrap, mean(sample(dat_reef$Elev,(length(dat_reef$Elev)),replace=T)))}

View(bstrap) #there are your XX boot strap samples of cnt
hist(bstrap,xlim=c(-2,0)) #ok what shape are these data now?
abline(v = -1.2, col = "blue", lwd = 2) #add the vertical line

mean(bstrap) 

#here is your mean of the bootstrap
#mean(dat_reef$Elev) #here is the mean of your original

#lower bound
quantile(bstrap,.025)
#upper bound
quantile(bstrap,.975)

quantile(bstrap,0.8)

# p1<-ggplot(data=bstrap) +
#   aes(Elev) + 
#   #super critical to just call Elev here and not dat$Elev or it goofs up facet_wrap
#   labs(x="Elev", y="Frequency", title="Elevation of rock top surface") +
#   geom_histogram(breaks=seq(-2.5, 0.5, by=0.01)) +
#   xlim(c(-2.5,0.5)) +
#   ylim(c(0,10))

##############################################################
##############################################################
#End function
##############################################################
