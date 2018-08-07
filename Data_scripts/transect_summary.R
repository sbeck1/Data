
###############################################
#       Transect Summary Data 
#       K. Zarada
#       and Bill Pine
###############################################

#load packages 
library(tidyverse)
library(ggplot2)
library(gplots)
library(zoo)
library(RCurl)
library(dplyr)
library(magrittr)

############
#Load the data
############

#Updated this so it's loading data directly from GitHub
#tranurl = getURL("https://raw.githubusercontent.com/LCRoysterproject/Data/master/Oyster_data/Transect/transect_combined.csv")
#tran = read.csv(text = tranurl)

library(readr)
tran <- read.csv("Oyster_data/Transect/transect_combined.csv",header=T)


tran = tran[-which(tran$Site == 1),]
tran$Site = factor(tran$Site)

############
#Manipulate the dates within the data
############

#Deal with date-time variables
tran$Date   = as.Date(tran$Date, format ="%Y-%m-%d")  
#convert date from factor to date

tran$Month  = factor(format(tran$Date,"%B"),levels=c(month.name),ordered=T) #make month an ordered factor, makes plots go in correct order (rather than alphabetic)

tran$Year   = factor(format(tran$Date, "%Y"))   
#pull out year
tran$Month = month.abb[tran$Month]  
#make month abbr.- works better in plots 

#############
#Create new variables related to unique months, years, and "locals" for #"localities".  These are used in the stat summary function and graphing #loops
#############
mos         = levels(factor(tran$Month))  #months as factor
yrs         = unique(tran$Year)           #unique years
locals      = unique(tran$Locality)       #unique localities 

####################
###This is manipulation to the data that is a temporary
###solution to a problem with the dataset
####################

#Below I filter everything not equal (!=) to TransLngth =NA
#This is to remove the July 2011 CK data that are not entered but we have
#datasheets

tran_y <- filter(tran,TransLngth !="NA")

#Making tran and tran_y the same, lazy way
#This is so I don't have to change all the code I've already written to
#tran_y.  Should probably have done this as mutate

tran=tran_y

############
#Make data summary tables
############

table(tran$Month,tran$Year)         
#table to samples (counts in a segment) per month per year
#these numbers are large because these are the number of transect "segments
#that are counted

table(tran$Month,tran$Locality,tran$Site,tran$Year)           
#n samples by year, locality and site

check_tab<-filter(tran, Month=="Jul" & Year=="2011" & Locality =="CK")
#Jul 2011 CK is not showing because I filtered it out earlier

############
#Define function to calculate summary stats
############


###########################################################################
#
# Modified from BP's transect summaries 
#
##########################################################################

#Note from bp need to think about this function more as not sure these
#CI are calculated correctly given distributions of oysters

#Dan Maxwell says "never use a dot at the beginning of a funtion#

.sumstats = function(x) 
  c(Mean=mean(x,na.rm=T), 
    Sd=sd(x,na.rm=T), 
    Var=var(x,na.rm=T), 
    Nobs=length(na.omit(x)),
    CV    = sd(x,na.rm=T)/mean(x,na.rm=T),
    se    = sd(x,na.rm=T)/sqrt(length(na.omit(x))),
    L95se = mean(x,na.rm=T)-1.96*(sd(x,na.rm=T)/
                                    sqrt(length(na.omit(x)))),
    U95se = mean(x,na.rm=T)+1.96*(sd(x,na.rm=T)/
                                    sqrt(length(na.omit(x)))))

#############
##Katie read this
######################################################
#see below to add to the summary function, or make new function
#############################################################

#Below is a function that is just parked here that can be used to do #bootstrap CI using quantile method on the oyster count data
#https://en.wikipedia.org/wiki/Bootstrapping_(statistics)

# bstrap <- c()
# for (i in 1:1000){
#   bstrap <- c(bstrap, mean(sample(cnt$Cnt_Live,(length(cnt$Cnt_Live)),replace=T)))}
# 
# View(bstrap) #there are your 1000 boot strap samples of cnt
# hist(bstrap) #ok what shape are these data now?
# 
# mean(bstrap) #here is your mean of the bootstrap
# mean(cnt$Cnt_Live) #here is the mean of your original
# #lower bound
# quantile(bstrap,.025)
# #upper bound
# quantile(bstrap,.975)
##############################################################
##############################################################
#End function
##############################################################

#############
#Moving into by reef element summaries
###########

#OK this is where I deviate from Katie
#Need to use substation in the creation of CNT not station
#otherwise data from several substations (reef elements) are combined

#Remember Station and Substation are different for the LCO sites that 
#reprent epoch 3.  Only Substation has the "A" and "B" designations

#Get transect length per each reef element sampled
#this is super critical, here it is getting the max by substation

max_tran  = aggregate(TransLngth ~ Month + Year + Substation + Site + Locality,data=tran,max,na.action=na.pass)

# tran1mx and tran2mx are just checks to see if it 
# matters which way you run the aggregate function, it doesn't. 
# This means as long as substation is in
# the aggregate it is aggregating to lowest level

# tran1mx<-filter(max_tran, Month=="Apr" & Year =="2013")
# max_tran2  = aggregate(TransLngth ~ Month + Year + Substation,data=tran,max,na.action=na.pass)
#  
# tran2mx<-filter(max_tran2, Month=="Apr" & Year =="2013")

#############
#Aggregate by transect, not individual segments in transect.
#This is an important aggregation
###########

#Get total counts per each bar sampled for each replicate
cnt = aggregate(Cnt_Live~ Month + Year + Replicate + Substation + Site + Locality,data=tran,sum,na.rm=T,na.action=na.pass)         
cnt = merge(cnt,max_tran) 
cnt = cnt %>% arrange(Year, Month, Substation)
cnt$Trip = paste0(cnt$Year, "-", cnt$Month)

###########################################################

#OK now just checking to see how Peter did some of his calculations
#that were used in the Frederick et al. paper.
#Get the average count per 2.5 m segment as Peter does in his 
#spreadsheet by averaging across segments and replicates
#I don't think this is the way to go, I think need to sum across the
#segments for each transect and divide by length of transect for density
#this is something to think more about

# #Get mean counts per each bar sampled to check Peter's paper
# p_cnt = aggregate(Cnt_Live~ Month + Year + Substation + Site + Locality,data=tran,FUN=mean,na.rm=T,na.action=na.pass)         
# p_cnt = merge(cnt,max_tran) 
# p_cnt = cnt %>% arrange(Year, Month, Substation)
# p_cnt$Trip = paste0(cnt$Year, "-", cnt$Month)
# 
# #this is just checking to see if this is the same as Peter's spreadsheet
# PF_tab1<-filter(p_cnt, Month=="Apr" & Year=="2013")
# #Yes the mean counts are the same as spreadsheet and same as paper. Good.
#############################################################

#####################
##Calculating Density
#####################

cnt$TranArea  = cnt$TransLngth*0.1524     
#0.1524 m is width of transect.  Transect measured as 6 inches in field.

cnt$Density   = cnt$Cnt/cnt$TranArea 

cnt$Trip2 = as.yearmon(cnt$Trip, format = "%Y-%b")
### turning trip into date


#####################
##Some tables tell us the number of transects (not segments)
#####################

table(cnt$Month,cnt$Year) 
table(cnt$Trip2,cnt$Year)

month_yr<- cnt %>%
  group_by(Year, Month) %>%
  summarize(n())
#table samples per month per year

table(cnt$Month,cnt$Substation,cnt$Year)           
#n samples by year, locality and site

#just take a look at a few years

cnt_2014<-filter(cnt, Year == 2014)
table(cnt_2014$Month,cnt_2014$Substation) 

cnt_2015<-filter(cnt, Year == 2015)
table(cnt_2015$Month,cnt_2015$Substation) 

#no samples in 2016

cnt_2017<-filter(cnt, Year == 2017)
table(cnt_2017$Month,cnt_2017$Substation) 

#####################
##Begin summarizing the density on each element using stats function written above
#####################


#Multiple function outputs from 'aggregate' are stored internally as a weird #matrix variable so have to 'flatten' it out using the do.call statement.    
#Density ~by~ Trip + locality + site

# Below aggregate is important. If Substation is included then CI not estimated for many substations because a lot of the substations have only had one sampling event

stats = aggregate(Density~ Trip + Trip2 + Year + Locality + Site + Substation,data=cnt,FUN=.sumstats)

stats = do.call("data.frame",stats)                   #flatten that matrix variable

names(stats) = gsub('Density.','',names(stats))        #remove prefix from names made by 'aggregate'

stats = stats %>% arrange(Trip2) %>% filter(complete.cases(.))   
#removing rows that have NA's

stats$L95se[stats$L95se < 0] = 0  
#making the L95se 0 


#I don't like this, a big deal to convert these lower 95 to 0, suggests again need to think about how we are summarizing these data as the distributions are not normal
#######################################################


#####################
##Could add new variable "season" this way
#Need to define seasons better, at some point revisit what Caleb did for CK
#####################

# cnt%>%mutate(Season= 
#       ifelse(Month== c("Oct","Nov","Dec","Jan"), "Winter", "Summer"))
#this works, but I forget why mutate creates only a "temporary" new #variable
             
cnt$Season<-ifelse(cnt$Month==
        c("Oct","Nov","Dec","Jan"), "Winter", "Summer")

ggplot(data=cnt) +
  labs(title="Transects only: Seasonal density by locality and year. Summer = Apr-Aug & Winter = Oct-Jan")+
  geom_boxplot(
  mapping = aes(
      x=Year,
      y=Density))+
  facet_grid(Locality ~ Season)


################################################################
################################################################

################################################################################
#
#                     Plots modified from BP's transect summary 
#
################################################################################

#Plot density of live oyster counts per trip by Locality 
trips = unique(stats$Trip)
par(mfrow=c(1,1),oma=c(0,0,0,0))
ymax = pretty(max(stats$Mean,na.rm=T))[2]+500                           #this gets a nice number for y-axis range to use on all plots
for(i in trips){
  xy   = stats[stats$Trip== i,]
  if(dim(xy)[1] > 1){   
#if else statement to remove data that only has 2 sites sampled 
#this is a big switch, bp turn to zero so summaries and plots       #calculated even if only one site such as offshore only was sampled at a #locality such as LC
  avg = tapply(xy$Mean,list(xy$Locality,xy$Site),sum)
  low = tapply(xy$L95se,list(xy$Locality,xy$Site),sum)
  low[low<0] = 0                                                              #set minimum lower CI to 0
  up  = tapply(xy$U95se,list(xy$Locality,xy$Site),sum) 
  bp  = barplot(avg,ylim=c(0,ymax),beside=T,main=paste(i),axisnames=F,ylab='Oyster density m^2')
  mtext(1,at=as.vector(bp),text=rep(rownames(avg),3),line=1)
  mtext(1,at=colMeans(bp),text=c('Inshore','Nearshore','Offshore'),line=3)
  arrows(as.vector(bp),as.vector(low),as.vector(bp),as.vector(up),
         angle = 90,code = 3,length=0.06)}else{cat(paste0("Not enough data for", " ", i, " ; "))}
}

 
#Plot density over time at each locality, sites stacked atop one another
#this is designed to only plot when inshore-nearshore-offshore sampled
#for an area

stats = stats %>% arrange(Trip2) %>% filter(complete.cases(.))   #removing rows that have NA's

par(mfrow=c(3,1),oma=c(0,0,4,0))
for(i in locals){
  xy          = stats[stats$Locality == i,]
  label       = ifelse(i=='CK','Cedar Key',ifelse(i=='CR','Corrigans Reef',ifelse(i=='HB','Horseshoe Beach','Lone Cabbage Reef')))
  avg         = tapply(xy$Mean,list(xy$Trip2,xy$Site),sum)
  colnames(avg) = c('Inshore','Nearshore','Offshore')
  avg1 = as.data.frame(avg)
  avg1$Trip2 = rownames(avg)
  avg = avg1 %>% filter(complete.cases(.))
  rownames(avg) = avg$Trip2
  avg = avg[,-4]
  low         = tapply(xy$L95se,list(xy$Trip2,xy$Site),sum)
  low[low<0]  = 0
  low = as.data.frame(low)
  low = low %>% filter(complete.cases(.))
  up          = tapply(xy$U95se,list(xy$Trip2,xy$Site),sum)
  up = as.data.frame(up)
  up = up %>% filter(complete.cases(.))
  ymax        = pretty(max(up,na.rm=T))[2]
  for(j in 1:ncol(avg)){
    bp  = barplot(avg[,j],main=colnames(avg)[j],ylim=c(0,ymax),cex.names=1)
    mtext(1,at=as.vector(bp),text=rownames(avg),line=1)
    arrows(as.vector(bp),as.vector(low[,j]),as.vector(bp),as.vector(up[,j]),angle=90,code=3,length=0.06) 
  }
  title(label,outer=T,cex.main=2)
}

################################################################################
#
#                     BALLOON PLOTS
#
################################################################################
#get full names for plots
stats$Locname  =  ifelse(stats$Locality=='CK','Cedar Key',
                         ifelse(stats$Locality=='CR','Corrigans Reef',
                                ifelse(stats$Locality=='HB','Horseshoe Beach','Lone Cabbage Reef')))
stats$Sitname  = ifelse(stats$Site=='I','Inshore',ifelse(stats$Site=='N','Nearshore','Offshore')) 

#make balloon plot
par(mfrow=c(1,1),oma=c(0,0,0,0))
for(i in trips){
  xy   = stats[stats$Trip==i,]
  if(dim(xy)[1] > 2){
  balloondat  = as.table(with(xy,tapply(Mean,list(Locname,Sitname),sum)))
  balloonplot(balloondat,colmar=.5,rowmar=.75,show.margins=F,cum.margins=F,label.lines=F,ylab='',xlab='',text.size=.65,colsrt=45,
              main= paste(i),sub=expression(paste("Oyster Density (m"^"2",")")))}else{cat(paste0("Not enough data for ", i, "; "))}
}



################################################################################
#
#    Control vs Treatment by year             
#
################################################################################

tran.treat = tran %>% dplyr::select(Month, Year, Locality, Site, Bar, Substation, Cnt_Live, Treatment, TransLngth) %>% 
  filter(Locality == "LC", Site == "O")
max_tran  = aggregate(TransLngth ~ Month + Year+ Locality+ Site+ Bar+ Substation+ Treatment,data=tran.treat,max,na.action=na.pass)
cnt_treat = aggregate(Cnt_Live~ Month + Year + Locality + Site + Bar + Substation +Treatment ,data=tran.treat,sum,na.rm=T,na.action=na.pass)         
cnt_treat = merge(cnt_treat,max_tran) 
cnt_treat = cnt_treat %>% arrange(Year)
cnt_treat$Trip = paste(cnt_treat$Month, cnt_treat$Year)
cnt_treat$Density = cnt_treat$Cnt_Live/cnt_treat$TransLngth

sub = c("LCO11A","LCO10B","LCO8B","LCO9A") ### "LCO11A","LCO10B","LCO8B","LCO9A" are treated sites 

#######################################
#######################################
# Filter to just a few months I'm interested in
cnt_treat_winter <- cnt_treat[cnt_treat$Month %in% c("Oct","Nov","Dec","Jan"),]

#need to be careful because epoch 2 was mostly sampled summer
#so winter filters too much

ggplot(cnt_treat_winter, aes(x=Year,y=Density)) + geom_boxplot() +
  facet_grid(Treatment ~ .)
#######################################
#######################################

par(mfrow = c(2,2))
for(i in sub){
  d = cnt_treat %>% filter(Substation == i)
  bp = barplot(d$Density, main = paste0(i), cex.names = 1 )
  mtext(1, at = as.vector(bp), text = d$Trip, line = 1, las = 2, cex = 0.7)
}




