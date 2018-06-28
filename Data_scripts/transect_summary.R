############################################
#       Transect Summary Data 
#       K. Zarada
#       and bill pine
##############################################

#load packages 
library(tidyverse)
library(ggplot2)
library(gplots)
library(zoo)
library(RCurl)

#load data
#updated this so it's loading data directly from GitHub
tranurl = getURL("https://raw.githubusercontent.com/LCRoysterproject/Data/master/Oyster_data/Transect/transect_combined.csv")
tran = read.csv(text = tranurl)

tran = tran[-which(tran$Site == 1),]
tran$Site = factor(tran$Site)
#tran = read.csv("Oyster_data/Transect/transect_combined.csv",header=T)

################################################################################
#
#                
#
################################################################################

#deal with date-time variables
tran$Date   = as.Date(tran$Date, format ="%Y-%m-%d")  #convert date from factor to date
tran$Month  = factor(format(tran$Date,"%B"),levels=c(month.name),ordered=T)   #make month an ordered factor, makes plots go in correct order (rather than alphabetic)
tran$Year   = factor(format(tran$Date, "%Y"))   #pull out year
tran$Month = month.abb[tran$Month]  #make month abbr.- works better in plots 
table(tran$Month,tran$Year)         #table to samples per month per year
table(tran$Month,tran$Locality,tran$Year)           #n samples by year, locality and site


mos         = levels(factor(tran$Month))  #months as factor
yrs         = unique(tran$Year)           #unique years
locals      = unique(tran$Locality)       #unique localities 

################################################################################
#
#                          Modified from BP's transect summaries 
#
################################################################################
#Function to compute multiple summary stats in aggregate function.
.sumstats = function(x) c(Mean=mean(x,na.rm=T), Sd=sd(x,na.rm=T), Var=var(x,na.rm=T), Nobs=length(na.omit(x)),
                          CV    = sd(x,na.rm=T)/mean(x,na.rm=T),
                          se    = sd(x,na.rm=T)/sqrt(length(na.omit(x))),
                          L95se = mean(x,na.rm=T)-1.96*(sd(x,na.rm=T)/sqrt(length(na.omit(x)))),
                          U95se = mean(x,na.rm=T)+1.96*(sd(x,na.rm=T)/sqrt(length(na.omit(x)))))

#get transect length per each bar sampled
max_tran  = aggregate(TransLngth ~ Month + Year + Station + Site + Locality,data=tran,max,na.action=na.pass)

#Get total counts per each bar sampled
cnt = aggregate(Cnt_Live~ Month + Year + Station + Site + Locality,data=tran,sum,na.rm=T,na.action=na.pass)         
cnt = merge(cnt,max_tran) 
cnt = cnt %>% arrange(Year)
cnt$Trip = paste0(cnt$Year, "-", cnt$Month)

#calculate density
cnt$TranArea  = cnt$TransLngth*0.1524     #0.1524 is width of transect
cnt$Density   = cnt$Cnt/cnt$TranArea 

### turning trip into date
cnt$Trip2 = as.yearmon(cnt$Trip, format = "%Y-%b")



#Multiple function outputs from 'aggregate' are stored internally as a weird 
#matrix variable so have to 'flatten' it out using the do.call statement.    
#Density ~by~ Trip + locality + site
stats = aggregate(Density~ Trip + Trip2 + Year + Locality + Site,data=cnt,FUN=.sumstats)
stats = do.call("data.frame",stats)                                    #flatten that matrix variable
names(stats) = gsub('Density.','',names(stats))                               #remove prefix from names made by 'aggregate'
stats = stats %>% arrange(Trip2) %>% filter(complete.cases(.))   #removing rows that have NA's
stats$L95se[stats$L95se < 0] = 0  #making the L95se 0 

################################################################################
#
#                     Plots modified from BP's transect summary 
#
################################################################################
#Plot density of live oyster counts per trip by Locality 
trips = unique(stats$Trip)
par(mfrow=c(1,1),oma=c(0,0,0,0))
ymax = pretty(max(stats$U95se,na.rm=T))[2]+50                                 #this gets a nice number for y-axis range to use on all plots
for(i in trips){
  xy   = stats[stats$Trip== i,]
  if(dim(xy)[1] > 2){   #if else statement to remove data that only has 2 sites sampled 
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


par(mfrow = c(2,2))
for(i in sub){
  d = cnt_treat %>% filter(Substation == i)
  bp = barplot(d$Density, main = paste0(i), cex.names = 1 )
  mtext(1, at = as.vector(bp), text = d$Trip, line = 1, las = 2, cex = 0.7)
}





