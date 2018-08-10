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

dat2<-subset(dat, select =c("Date","Transect", "Elev", "Element_id"))
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
dat3<-mutate(dat2, Element_id_2 = ifelse(Transect > 6, "11c", Element_id))

#ok this is creating the new Element_id_2 as a factor for use in plotting below
dat3$Element_id_3<-factor(dat3$Element_id_2, 
          levels =c ("5", "7", "8a", "9b", "10b", "11b", "11c"))

###Tables
#just do some summarizing using pipes for fun and practice.  Remember %>% should be thought of as "then"

#simple summary of points by Element_id and Transect
n_element_trans<- dat3 %>%
  group_by(Element_id_2, Transect) %>%
  summarize(n())

reefs <-  dat3 %>%
  group_by(Element_id_3) %>%
  summarize(
    count=n(),
    mean_elev=mean(Elev),na.rm=TRUE,
    max_elev=max(Elev))
names(reefs) <- c("Reef", "Count_rocks", "Mean_elev",
                  "Max_elev")

#be careful here as 11c has none over so it only returns 6 reefs, not 7
over_spec <-  dat3 %>%
  group_by(Element_id_3) %>%
  filter(Elev > -1.2) %>%
  summarize(
    count_over=n())
#the above returns 6 reefs that meet condition

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


# #this was almost working
# x<-dat3 %>% 
#   group_by(Element_id_3) %>%
#   filter(Elev > -1.2) %>%
#   summarize(
#     count_over=n())
# str(x)
# 
# x_spread<-x %>% 
#   spread(key=Element_id_3,value=count_over)
# 
# 
# x %>% 
#   spread(Element_id_3,count_over,fill=0)

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

p1<-ggplot(data=dat3) +
  aes(Elev) + 
  #super critical to just call Elev here and not dat$Elev or it goofs up facet_wrap
  labs(x="Elev", y="Frequency", title="Elevation of rock top surface elements 5-11b") +
  geom_histogram(breaks=seq(-2.5, 0.5, by=0.01)) +
  xlim(c(-2.5,0.5)) +
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
  facet_wrap(~Element_id_3, nrow=7) +
  labs(title = "Elevation of rock top surface by reef element")

p2



##OK so the bin size, axis, and everything are the same as p2 and p2.1
##but the difference is wraping as Element_id (a character) or Element_id_2 (a factor)
##you use the factor to order the graphs however you want to order them

#ok just adding some vertical lines to mark the elevation ranges

p3<-p1+
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
  facet_grid(Element_id_3~.) +
  labs(title = "Frequency histogram of reef elevation by reef element") +
  geom_vline(xintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_vline(xintercept = -1.95, color = "black", size=1, linetype = 2)

p3.1


#some boxplots of elevation
p5<-ggplot(data=dat3) +
          labs(title="Elevation of rock top surface") + 
          geom_boxplot(
          mapping = aes(
            x=Element    _id_3,
            y=Elev))+
  geom_hline(yintercept = -1.2, color = "black", size=1, linetype = 2) +
  geom_hline(yintercept = -1.95, color = "black", size=1, linetype = 2)
p5


#this is kind of neat, now put in order by mean elevation

p6<-ggplot(data=dat3) +
      labs(title="Reef element elevation in order of mean elevation") +
      geom_boxplot(
      mapping = aes(
        x=reorder(Element_id_3, Elev, FUN=mean),
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

#Not working

##need to calculate the number of observations greater than
##the minimum elevation

#use summarize

surveys_sml <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

Over <- dat3 %>%
  filter(Elev <- 1.95)

table(elev_overtran$Month,tran$Locality,tran$Site,tran$Year)           
#n samples by year, locality and site

table(elev_over$Elev, elev_over$Element_id_3, data=elev_over)
  

#%>%
#  mutate(PERCENT = prop.table(n))

max.elev =-1.95
dat3 %>%
  group_by(Element_id_3) %>%
  mutate(n=n()) %>%
  #group_by(Element_id_3) %>%
  filter(n() > max.elev) %>% select(-n)




####
xy<-density(dat3$Elev, na.rm=T)
View(xy)

#this will give you the elevations
xy[["x"]]
#this will give you all the densities for every reef element
xy[["y"]]


################################
################################
##data from Aug 8 Reef 13

dat13<- read_csv("asbuilt_80918_13.csv")

dat13$Date=mdy("8/8/2018") #need to add the sampling date

dat13$Element_id=13 #need to add reef Element

dat13$Element_id_2=13 #need to add reef Element

dat13_2<-filter(dat13,dat13$Type!=c("GS"))
#only keep the rocks, get rid of GS or ground shots != is saying is not equal to


dat13_3<-subset(dat13_2, select =c("Date", "Transect", "Elev", "Element_id", "Element_id_2"))
#just the columns I want


#joint dat 3 and dat 13_3








