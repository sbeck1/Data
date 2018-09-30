#This R code is used to take a look at the late night survey data from reef
#elements approximately 2-11 that was collected on LCR the night of July 31
#This is also a "lab guide" for making these types of plots
#I put a lot of notes in this code to highlight "what I'm watching for"

#good ideas here https://ggplot2.tidyverse.org/reference/geom_histogram.html
#https://bbolker.github.io/R-ecology-lesson/04-visualization-ggplot2.html

#https://owi.usgs.gov/blog/boxplots/

library("tidyverse")
library("MASS")
library("lubridate")


setwd("~/Git/Data/Elevation_check/Elevation_check")
dat<- read_csv("as_built_20180928.csv")

dat$Date=mdy("09/28/2018") #need to add the sampling/delivery date


dat2<-subset(dat, select =c("Date","Transect", "Elev", "Element", "Type"))

unique(dat2$Element)



#!= is "is not equal to"

# #just filtering out, one at a time to make sure I get this
# #right, the "other" names for GS. I'm assuming these are
# #all GS
# 
# dat2.1<-filter(dat2,dat2$Type!=c("GS/BEG 7"))
# dat2.2<-filter(dat2.1,dat2.1$Type!=c("GS/END 7"))
# dat2.3<-filter(dat2.2,dat2.2$Type!=c("GS/8 BEG"))
# dat2.4<-filter(dat2.3,dat2.3$Type!=c("GS/END 8"))
# dat2.5<-filter(dat2.4,dat2.4$Type!=c("GS"))
# 
# #now in 2.5 we are only working with ROCK TOPS
# #I did it this way incase we have to go back and easily
# #allow some of these random names back in
# 
# #ok this is just renaming dat2.5 back to dat2
# dat2<-dat2.5

#create new dataframe dat2 by mutating dat to create new elment id
#name 11c for the most southern 3 transects of 11b.  These transects
#are the new rock size transects. the notation is 7 or 8 or 9
dat3<-mutate(dat2, Element_id_2 = ifelse(Transect > 999, "11c", Element))

#need to turn this off now simply by setting transect to 99.  This is because with the second date of transect sampling there are now multiple transects with same number

#ok this is creating the new Element_id_2 as a factor for use in plotting below
dat3$Element_id_3<-factor(dat3$Element_id_2, 
          levels =c ("8B","9A","9B","10A","11A","11B","15","16","18"))

###Tables
#just do some summarizing using pipes for fun and practice.  Remember %>% should be thought of as "then"

#simple summary of points by Element_id and Transect
n_element_trans<- dat3 %>%
  group_by(Element_id_2, Transect) %>%
  summarize(n())

#simple summary statistics by each reef
reefs <-  dat3 %>%
  group_by(Element_id_3) %>%
  summarize(
    count=n(),
    mean_elev=mean(Elev),
    max_elev=max(Elev),
    sd_elev=sd(Elev),
    CV_elev=(sd(Elev)/mean(Elev)*100))
names(reefs) <- c("Reef", "Count_rocks", "Mean_elev",
                  "Max_elev", "SD_elev", "CV_elev")

#be careful here as if there are none over, then that reef element is  not returned so it can appear the number of elements is not correct
over_spec <-  dat3 %>%
  group_by(Element_id_3) %>%
  filter(Elev > -1.2) %>%
  summarize(
    count_over=n())

#be careful here as if there are none over, then that reef element is  not returned so it can appear the number of elements is not correct
down_spec <-  dat3 %>%
  group_by(Element_id_3) %>%
  filter(Elev < -1.95) %>%
  summarize(
    count_under=n())

#the above returns X reefs that meet condition, Z not included because none are over

num_rocks <-  dat3 %>%
  group_by(Element_id_3) %>%
    summarize(
    num_rocks=n())
#this returns all 7 reefs 

#so you can look at over_spec and num_rock and get the % that are over by hand


#how many overall are outside of the bounds
total_num<-length(dat3$Elev)
over<-sum(ifelse(dat3$Elev>-1.2,1,0))
under<-sum(ifelse(dat3$Elev<-1.95,1,0))

# #################################################################
# ##What are we trying to do?#######
# #make the simple frequency histogram plots
# #we want to get to histograms by reef element (Element_id)
# 
# 
# ###what do we need to watch for with histograms?
# #be careful to match the precision of the data
# #be very careful with bin size
# 
# 
# ###############
# ##PLOT 1
# ###############
# 
# #this plot will be all reef elements combined
# #this is a good way to check to see what the X and Y axis ranges should be
# #since I am working towards plotting this as a facet wrap by Element_id
# #I need to make sure X and Y axis are all the same for each plot or it will b e #misleading
# 
# #Also we are comparing Elev to a value of -1.45 (or whatever is finally decided). So we #need to bin to the # 5 hundreths, if you do this then the frequencies are going to be #small
# 
# #i'm going to name this basic graph p1 so I don't have to keep typing 
# #the key parts to get the graph every time 
# #to plot the graph you have to type p1 and then call p1 to get the graph to plot
# 
# #remember again, you change bin sizes you change freq on the yaxis,
# #so you don't want to limit your y axis so your max(freq) isn't shown
# 
# #let's start out with really small bin size, 1/100. this is because our elevation#s are measured to the 1/100 of a foot in the survey data. This will require us
# #to make the frequency small to be able to see anything as we will
# #not be putting a lot of observations in each bin


windows(record=T) 
#ok this will open the larger plotting window, mac user will need quartz
#page up/page down to scroll through

#in the code below the first line is defining the data we want to plot
#Elev, then we give the graph labels, then we define the bin size in line 3 and then we define the x and y limits.  note one part of the graph per line


#note sometimes you need to re-run dat2 and dat 3 BUT NOT RUN THE TABLES to get the dat3
#to be correct for making graphs. I don't know what filter in the summaries
#is doing to dat3 so it isn't carrying the values forward.


p1<-ggplot(data=dat3) +
  aes(Elev) + 
  #super critical to just call Elev here and not dat$Elev or it goofs up facet_wrap
  labs(x="Elev", y="Frequency", title="Elevation of rock top surface elements 5-11b") +
  geom_histogram(breaks=seq(-4.5, 0, by=0.001)) +
  #xlim(c(-2.5,0)) +
  ylim(c(0,10))

p1

#ok so p1 looks pretty good, very fine bin scale which we might change
#later, but for now ok

######READ THIS##############################################

#here is something neat, when you are working on the bin size you might get 
#a warning that looks like this
##Warning message:
##Removed 4 rows containing missing values (geom_bar).
#this is because geom_histogram is building a table that goes from the sequence
#you identify in the breaks such as -2.5 to 0 by 0.05.  If you don't have
#a value for a bin that is say -0.05 in your data then it gives this "missing #value" error. So it is a scarry warning, but no data are removed.  here is how
# you can look at the tables within geom_histogram (just showing first 10 rows)

head(ggplot_build(p1)$data[[1]], 10)

##########################################################################

#now make the plot by reef element using facet wrap, these will be out of sequential order meaning the reefs won't be in order which is ok

#ok adding Element_id which is a character
p2<-p1+
  facet_wrap(~Element_id_2, nrow=7) +
  labs(title = "Elevation of rock top surface by reef element")

p2



##OK so the bin size, axis, and everything are the same as p2 and p2.1
##but the difference is wraping as Element_id (a character) or Element_id_2 (a factor)
##you use the factor to order the graphs however you want to order them

#ok just adding some vertical lines to mark the elevation ranges

p3<-p2+
  labs(title = "Elevation of rock top surface by reef element")+
  geom_vline(xintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1, linetype = 2)

p3

p4<-p2+
  labs(title = "Frequency histogram of reef elevation by reef element")+
  geom_vline(xintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1, linetype = 2)

p4

#now let's use facet_grid instead. this is useful for rows~columns, but we only want rows so we use Element_id~. as we don't have a column

p3.1<-p1+
  facet_grid(Element_id_2~.) +
  labs(title = "Frequency histogram of reef elevation by reef element") +
  geom_vline(xintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1, linetype = 2)

p3.1


#some boxplots of elevation
p5<-ggplot(data=dat3) +
          labs(title="Elevation of rock top surface") + 
          geom_boxplot(
          mapping = aes(
            x=Element_id_2,
            y=Elev))+
  geom_hline(yintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_hline(yintercept = -1.95, color = "black", size=1, linetype = 2)
p5


#this is kind of neat, now put in order by mean elevation

p6<-ggplot(data=dat3) +
      labs(title="Reef element elevation in order of mean elevation") +
      geom_boxplot(
      mapping = aes(
        x=reorder(Element_id_2, Elev, FUN=mean),
        y=Elev))+
        geom_hline(yintercept = -1.2, color = "black", 
                   size=1, linetype = 2) +
        geom_hline(yintercept = -1.95, color = "black", 
                   size=1, linetype = 2)
p6
   
# # # add counts of observations DIDN"T WORK  
# p7<-p6+
#   geom_text(
#     data = reefs, aes(
#     y=max,  
#      label=reefs$Count_rocks), 
#            position=position_dodge(width=1.0))

#  p7

 #violin plot so we can better see distribution of data
 p8<-ggplot(data=dat3) +
   labs(title="Reef element elevation from north to south") +
   geom_violin(
     mapping = aes(
       x=Element_id_3,
       y=Elev)
   )
 p8
 
#now scale the violin plot width based on sample size, more samples = fatter violin
 p9.1<-ggplot(data=dat3) +
   labs(title="Reef element elevation from north to south") +
   geom_violin(
     mapping = aes(
       x=Element_id_3,
       y=Elev),
     scale = "count"
   )
 p9.1


##############################
#Density plots################
############################## 

#Color bling palette, with black as the starting color

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p10<-ggplot(data=dat3)+ 
  aes(x = Elev, 
      color=Element_id_3) +
      labs(x="Elevation", 
           y= 'Probaility Density', 
           color= "Element_id", 
           title= "Elevation Probability Density Function") +
  scale_colour_manual(values=cbbPalette) +
  geom_vline(xintercept = -1.2, color = "black", size=1,linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1,linetype = 2) +
  stat_density(aes(group = Element_id_3), position="stack",geom="line", size= 1.5)

p10

########

#END

####
xy<-density(dat3$Elev, na.rm=T)
View(xy)

#this will give you the elevations
xy[["x"]]
#this will give you all the densities for every reef element
xy[["y"]]


################################
################################








=======
#This R code is used to take a look at the late night survey data from reef
#elements approximately 2-11 that was collected on LCR the night of July 31
#This is also a "lab guide" for making these types of plots
#I put a lot of notes in this code to highlight "what I'm watching for"

#good ideas here https://ggplot2.tidyverse.org/reference/geom_histogram.html
#https://bbolker.github.io/R-ecology-lesson/04-visualization-ggplot2.html

#https://owi.usgs.gov/blog/boxplots/

library("tidyverse")
library("MASS")
library("lubridate")


setwd("~/Git/Data/Elevation_check/Elevation_check")
dat<- read_csv("as_built_20180920.csv")

dat$Date=mdy("09/20/2018") #need to add the sampling date


dat2<-subset(dat, select =c("Date","Transect", "Elev", "Element", "Type"))

unique(dat2$Element)



#!= is "is not equal to"

# #just filtering out, one at a time to make sure I get this
# #right, the "other" names for GS. I'm assuming these are
# #all GS
# 
# dat2.1<-filter(dat2,dat2$Type!=c("GS/BEG 7"))
# dat2.2<-filter(dat2.1,dat2.1$Type!=c("GS/END 7"))
# dat2.3<-filter(dat2.2,dat2.2$Type!=c("GS/8 BEG"))
# dat2.4<-filter(dat2.3,dat2.3$Type!=c("GS/END 8"))
# dat2.5<-filter(dat2.4,dat2.4$Type!=c("GS"))
# 
# #now in 2.5 we are only working with ROCK TOPS
# #I did it this way incase we have to go back and easily
# #allow some of these random names back in
# 
# #ok this is just renaming dat2.5 back to dat2
# dat2<-dat2.5

#create new dataframe dat2 by mutating dat to create new elment id
#name 11c for the most southern 3 transects of 11b.  These transects
#are the new rock size transects. the notation is 7 or 8 or 9
dat3<-mutate(dat2, Element_id_2 = ifelse(Transect > 999, "11c", Element))

#need to turn this off now simply by setting transect to 99.  This is because with the second date of transect sampling there are now multiple transects with same number

#ok this is creating the new Element_id_2 as a factor for use in plotting below
dat3$Element_id_3<-factor(dat3$Element_id_2, 
          levels =c ("10B","12","14","17"))

###Tables
#just do some summarizing using pipes for fun and practice.  Remember %>% should be thought of as "then"

#simple summary of points by Element_id and Transect
n_element_trans<- dat3 %>%
  group_by(Element_id_2, Transect) %>%
  summarize(n())

#simple summary statistics by each reef
reefs <-  dat3 %>%
  group_by(Element_id_3) %>%
  summarize(
    count=n(),
    mean_elev=mean(Elev),
    max_elev=max(Elev),
    sd_elev=sd(Elev),
    CV_elev=(sd(Elev)/mean(Elev)*100))
names(reefs) <- c("Reef", "Count_rocks", "Mean_elev",
                  "Max_elev", "SD_elev", "CV_elev")

#be careful here as if there are none over, then that reef element is  not returned so it can appear the number of elements is not correct
over_spec <-  dat3 %>%
  group_by(Element_id_3) %>%
  filter(Elev > -1.2) %>%
  summarize(
    count_over=n())

#be careful here as if there are none over, then that reef element is  not returned so it can appear the number of elements is not correct
down_spec <-  dat3 %>%
  group_by(Element_id_3) %>%
  filter(Elev < -1.95) %>%
  summarize(
    count_under=n())

#the above returns X reefs that meet condition, Z not included because none are over

num_rocks <-  dat3 %>%
  group_by(Element_id_3) %>%
    summarize(
    num_rocks=n())
#this returns all 7 reefs 

#so you can look at over_spec and num_rock and get the % that are over by hand


#how many overall are outside of the bounds
total_num<-length(dat3$Elev)
over<-sum(ifelse(dat3$Elev>-1.2,1,0))
under<-sum(ifelse(dat3$Elev<-1.95,1,0))

# #################################################################
# ##What are we trying to do?#######
# #make the simple frequency histogram plots
# #we want to get to histograms by reef element (Element_id)
# 
# 
# ###what do we need to watch for with histograms?
# #be careful to match the precision of the data
# #be very careful with bin size
# 
# 
# ###############
# ##PLOT 1
# ###############
# 
# #this plot will be all reef elements combined
# #this is a good way to check to see what the X and Y axis ranges should be
# #since I am working towards plotting this as a facet wrap by Element_id
# #I need to make sure X and Y axis are all the same for each plot or it will b e #misleading
# 
# #Also we are comparing Elev to a value of -1.45 (or whatever is finally decided). So we #need to bin to the # 5 hundreths, if you do this then the frequencies are going to be #small
# 
# #i'm going to name this basic graph p1 so I don't have to keep typing 
# #the key parts to get the graph every time 
# #to plot the graph you have to type p1 and then call p1 to get the graph to plot
# 
# #remember again, you change bin sizes you change freq on the yaxis,
# #so you don't want to limit your y axis so your max(freq) isn't shown
# 
# #let's start out with really small bin size, 1/100. this is because our elevation#s are measured to the 1/100 of a foot in the survey data. This will require us
# #to make the frequency small to be able to see anything as we will
# #not be putting a lot of observations in each bin


windows(record=T) 
#ok this will open the larger plotting window, mac user will need quartz
#page up/page down to scroll through

#in the code below the first line is defining the data we want to plot
#Elev, then we give the graph labels, then we define the bin size in line 3 and then we define the x and y limits.  note one part of the graph per line


#note sometimes you need to re-run dat2 and dat 3 BUT NOT RUN THE TABLES to get the dat3
#to be correct for making graphs. I don't know what filter in the summaries
#is doing to dat3 so it isn't carrying the values forward.


p1<-ggplot(data=dat3) +
  aes(Elev) + 
  #super critical to just call Elev here and not dat$Elev or it goofs up facet_wrap
  labs(x="Elev", y="Frequency", title="Elevation of rock top surface elements 5-11b") +
  geom_histogram(breaks=seq(-2.5, 0.5, by=0.01)) +
  xlim(c(-2.5,1.5)) +
  ylim(c(0,15))

p1

#ok so p1 looks pretty good, very fine bin scale which we might change
#later, but for now ok

######READ THIS##############################################

#here is something neat, when you are working on the bin size you might get 
#a warning that looks like this
##Warning message:
##Removed 4 rows containing missing values (geom_bar).
#this is because geom_histogram is building a table that goes from the sequence
#you identify in the breaks such as -2.5 to 0 by 0.05.  If you don't have
#a value for a bin that is say -0.05 in your data then it gives this "missing #value" error. So it is a scarry warning, but no data are removed.  here is how
# you can look at the tables within geom_histogram (just showing first 10 rows)

head(ggplot_build(p1)$data[[1]], 10)

##########################################################################

#now make the plot by reef element using facet wrap, these will be out of sequential order meaning the reefs won't be in order which is ok

#ok adding Element_id which is a character
p2<-p1+
  facet_wrap(~Element_id_2, nrow=7) +
  labs(title = "Elevation of rock top surface by reef element")

p2



##OK so the bin size, axis, and everything are the same as p2 and p2.1
##but the difference is wraping as Element_id (a character) or Element_id_2 (a factor)
##you use the factor to order the graphs however you want to order them

#ok just adding some vertical lines to mark the elevation ranges

p3<-p2+
  labs(title = "Elevation of rock top surface by reef element")+
  geom_vline(xintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1, linetype = 2)

p3

p4<-p2+
  labs(title = "Frequency histogram of reef elevation by reef element")+
  geom_vline(xintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1, linetype = 2)

p4

#now let's use facet_grid instead. this is useful for rows~columns, but we only want rows so we use Element_id~. as we don't have a column

p3.1<-p1+
  facet_grid(Element_id_2~.) +
  labs(title = "Frequency histogram of reef elevation by reef element") +
  geom_vline(xintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1, linetype = 2)

p3.1


#some boxplots of elevation
p5<-ggplot(data=dat3) +
          labs(title="Elevation of rock top surface") + 
          geom_boxplot(
          mapping = aes(
            x=Element_id_2,
            y=Elev))+
  geom_hline(yintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_hline(yintercept = -1.95, color = "black", size=1, linetype = 2)
p5


#this is kind of neat, now put in order by mean elevation

p6<-ggplot(data=dat3) +
      labs(title="Reef element elevation in order of mean elevation") +
      geom_boxplot(
      mapping = aes(
        x=reorder(Element_id_2, Elev, FUN=mean),
        y=Elev))+
        geom_hline(yintercept = -1.2, color = "black", 
                   size=1, linetype = 2) +
        geom_hline(yintercept = -1.95, color = "black", 
                   size=1, linetype = 2)
p6
   
# # # add counts of observations DIDN"T WORK  
# p7<-p6+
#   geom_text(
#     data = reefs, aes(
#     y=max,  
#      label=reefs$Count_rocks), 
#            position=position_dodge(width=1.0))

#  p7

 #violin plot so we can better see distribution of data
 p8<-ggplot(data=dat3) +
   labs(title="Reef element elevation from north to south") +
   geom_violin(
     mapping = aes(
       x=Element_id_3,
       y=Elev)
   )
 p8
 
#now scale the violin plot width based on sample size, more samples = fatter violin
 p9.1<-ggplot(data=dat3) +
   labs(title="Reef element elevation from north to south") +
   geom_violin(
     mapping = aes(
       x=Element_id_3,
       y=Elev),
     scale = "count"
   )
 p9.1


##############################
#Density plots################
############################## 

#Color bling palette, with black as the starting color

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p10<-ggplot(data=dat3)+ 
  aes(x = Elev, 
      color=Element_id_3) +
      labs(x="Elevation", 
           y= 'Probaility Density', 
           color= "Element_id", 
           title= "Elevation Probability Density Function") +
  scale_colour_manual(values=cbbPalette) +
  geom_vline(xintercept = -1.2, color = "black", size=1,linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1,linetype = 2) +
  stat_density(aes(group = Element_id_3), position="stack",geom="line", size= 1.5)

p10

########

#END

####
xy<-density(dat3$Elev, na.rm=T)
View(xy)

#this will give you the elevations
xy[["x"]]
#this will give you all the densities for every reef element
xy[["y"]]


################################
################################








>>>>>>> 4da6027db5727cb72ecc6ea963916d00b03ce13b
