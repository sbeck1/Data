#This R code is used to take a look at the late night survey data from reef
#elements approximately 2-11 that was collected on LCR the night of July 31
#This is also a "lab guide" for making these types of plots
#I put a lot of notes in this code to highlight "what I'm watching for"

#good ideas here https://ggplot2.tidyverse.org/reference/geom_histogram.html
#https://bbolker.github.io/R-ecology-lesson/04-visualization-ggplot2.html

library("tidyverse")
library("MASS")
library("lubridate")

dat<- read_csv("20180731_surv_el_trans.csv")

dat$Date=mdy("7/31/2018") #need to add the sampling date

dat2<-subset(dat, select =c("Elev", "Transect", "Element_id", "Date"))
#just the columns I want

#if you look through dat2 you see there is a "GS DIRT" listed in transect for row
#217, we don't want to use the "dirt shot" survey points
#for the dirt points use Tom's original data, but that doesn't have the reef elements listed in the data file.

#Two ways to get rid of this row
#Just work with rows 1-216 to not use the last row which is the dirt shot

#dat<- dat[c(1:216),]

#or dplyr way using filter.  Note the != means "is not equal to" so this is #saying to convert dat to a dataframe that only has Transect variables that are
#not equal to GS DIRT

dat2<-filter(dat2,dat2$Transect!=c("GS DIRT"))

#create new dataframe dat2 by mutating dat to create new elment id
#name 11c for the most southern 3 transects of 11b.  These transects
#are the new rock size transects. the notation is 7 or 8 or 9
dat3<-mutate(dat2, Element_id_2 = 
               ifelse(Transect > 6, "11c", Element_id))

#table of survey points by transect and reef
n_element_trans<- dat3 %>%
  group_by(Element_id_2, Transect) %>%
  summarize(n())

#ok this is creating the new Element_id_2 as a factor for use in plotting below
dat3$Element_id_3<-factor(dat3$Element_id_2, 
      levels =c ("5", "7", "8a", "9b", "10b", "11b", "11c"))

#"5", "7", "8a", "9b", "10b", "11b", "11c"

#look at specific reefs
dat_reef<-filter(dat3, Element_id_3 == "5")


#############################################################

#Below is a function that is just parked here that can be used to do #bootstrap CI using quantile method which is likely a more appropriate general solution to the CI estimation given the distribution of the data

#https://en.wikipedia.org/wiki/Bootstrapping_(statistics)

#change in the i in 1:X, the X is the number of transects
#you would potentially take

bstrap <- c()
for (i in 1:4){
  bstrap <- c(bstrap, mean(sample(dat_reef$Elev,(length(dat_reef$Elev)),replace=T)))}

View(bstrap) #there are your XX boot strap samples of cnt
hist(bstrap,xlim=c(-2,-1.0)) #ok what shape are these data now?
abline(v = -1.2, col = "blue", lwd = 2)

mean(bstrap) 

#here is your mean of the bootstrap
#mean(dat_reef8$Elev) #here is the mean of your original

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
