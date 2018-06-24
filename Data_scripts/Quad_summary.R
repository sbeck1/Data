############################################
#       Transect Summary Data 
#       K. Zarada
##############################################

#load packages 
library(tidyverse)
library(ggplot2)
library(gplots)
library(zoo)
library(RCurl)

#Load data
quadurl = getURL("https://raw.githubusercontent.com/LCRoysterproject/Data/master/Oyster_data/Quadrat/quadrat_combined.csv")
quad = read.csv(text = quadurl)
#quad = read.csv("Oyster_data/Quadrat/quadrat_combined.csv")

quad$Date = as.Date(quad$Date, format = '%m/%d/%y')
#quad$Year = format(quad$Date, "%Y")
#quad$Month = month.abb[as.numeric(format(quad$Date, "%m"))] 
## row 12707 says size == 1938? 
#quad = quad[-12707,] #this should be updated on git now 

head(quad)

################################################################################
#
#             Size Distribution with Quadrat Data      
#
################################################################################
#Function to compute multiple summary stats in aggregate function.
.sumstats = function(x) c(Mean=mean(x,na.rm=T), Sd=sd(x,na.rm=T), Var=var(x,na.rm=T), Nobs=length(na.omit(x)),
                          CV    = sd(x,na.rm=T)/mean(x,na.rm=T),
                          se    = sd(x,na.rm=T)/sqrt(length(na.omit(x))),
                          L95se = mean(x,na.rm=T)-1.96*(sd(x,na.rm=T)/sqrt(length(na.omit(x)))),
                          U95se = mean(x,na.rm=T)+1.96*(sd(x,na.rm=T)/sqrt(length(na.omit(x)))))

########## Size Distribution at Density plots ##################

quad$SiteLoc= apply(quad, 1, function (x) paste0(x[3], x[4]))
sites = unique(apply(quad, 1, function (x) paste0(x[3], x[4])))
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#filtered for Oct - Feb 
for(i in 1:length(sites)){
  i = 9 # to just get size for LCO, remove this to get all sites for all years, though most just have 1 year
  d =  quad %>% select(SiteLoc, Size, Year, Month) %>% filter(SiteLoc == sites[i]) %>% filter(Month== "Oct"|Month== "Nov"|Month== "Dec"|Month== "Jan"|Month== "Feb" ) %>%  na.omit()
  d$Year = as.factor(d$Year)
  print(ggplot(d, aes(x=Size, color=Year)) + theme_bw() +
          stat_density(aes(group = Year), position= "stack", geom = "line", size = 1.5)
        + labs(title = paste(sites[i])) + scale_color_manual(values = cbbPalette))
}

#all months
for(i in 1:length(sites)){
  i = 9 # to just get size for LCO, remove this to get all sites for all years, though most just have 1 year
  d =  quad %>% select(SiteLoc, Size, Year, Month) %>% filter(SiteLoc == sites[i])%>%  na.omit()
  d$Year = as.factor(d$Year)
  print(ggplot(d, aes(x=Size, color=Year)) + theme_bw() +
          stat_density(aes(group = Year), position= "stack", geom = "line", size = 1.5)
        + labs(title = paste(sites[i])) + scale_color_manual(values = cbbPalette))
}

#can swtich scale_color_manual(values = cbbPalette) to scale_color_brewer() for the blues





###########  Station Size Dist #############
quad$SiteLoc= apply(quad, 1, function (x) paste0(x[3], x[4]))
sites = unique(apply(quad, 1, function (x) paste0(x[3], x[4])))

par(mfrow = c(3,6))
for(i in 1:length(sites)){
  
  d =  quad %>% dplyr::select(SiteLoc, Size, Year) %>% filter(SiteLoc == sites[i]) %>% na.omit()
  
  hist(d[,2], main= paste0(sites[i]), xlab ="Height(mm)", ylab = "Oyster Size Density", xlim = c(0, 265), freq = FALSE, breaks = seq(0, 265, by = 5), col = 'gray')
  abline(v= 75.4, col = 'red', lwd = 2, lty = 2)
}


########### Locality Size Dist  ################
locality = unique(quad$Locality)

par(mfrow = c(3,3))
for(i in 1:length(locality)){
  
  d =  quad %>% dplyr::select(Locality, Size, Year) %>% filter(Locality == locality[i]) %>% na.omit()
  
  hist(d[,2], main= paste0(locality[i]), xlab ="Height(mm)", ylab = "Oyster Size Density", xlim = c(0, 150), freq = FALSE, breaks = seq(0, 265, by = 5), col = 'gray')
  abline(v= 75.4, col = 'red', lwd = 2, lty = 2)
}

###### Locality + year ##########


for(i in 1:length(locality)){
  
  d =  quad %>% dplyr::select(Locality, Size, Year) %>% filter(Locality == locality[i]) %>% na.omit() %>% arrange(Year)
  index = unique(d$Year)
  if(length(index) > 1){
    par(mfrow = c(length(index),1))
    par(mar = c(2,2,2,2))
    for(j in 1:length(index)){
      d_sub = subset(d, d$Year == index[j])
      hist(d_sub[,2], main= paste0(locality[i], " ", index[j]), xlab ="Height(mm)", ylab = "Oyster Size Density", xlim = c(0, 150), freq = FALSE, breaks = seq(0, 265, by = 5), col = 'gray')
      abline(v= 75.4, col = 'red', lwd = 2, lty = 2)
    }}else{cat(paste0("Only 1 year for ", locality[i], "; "))}
}

###### Live vs Dead at Locality ##########


for(i in 1:length(locality)){
  
  d =  quad %>% dplyr::select(Locality, Size, Live_Dead) %>% filter(Locality == locality[i]) %>% na.omit() 
  
  par(mfrow = c(2,1))
  live = subset(d, d$Live_Dead == "L")
  dead = subset(d, d$Live_Dead == "D")
  
  hist(live[,2], main= paste0(locality[i], " ", "Live"), xlab ="Height(mm)", ylab = "Oyster Live Size Density", xlim = c(0, 150), freq = FALSE, breaks = seq(0, 265, by = 5), col = 'gray')
  abline(v= 75.4, col = 'red', lwd = 2, lty = 2)
  hist(dead[,2], main= paste0(locality[i], " ", "Dead"), xlab ="Height(mm)", ylab = "Oyster Dead Size Density", xlim = c(0, 150), freq = FALSE, breaks = seq(0, 265, by = 5), col = 'gray')
  abline(v= 75.4, col = 'red', lwd = 2, lty = 2)
  
}



###### Live vs Dead at Locality + Year ##########

for(i in 1:length(locality)){
  
  d =  quad %>% dplyr::select(Locality, Size, Year, Live_Dead) %>% filter(Locality == locality[i]) %>% na.omit() %>% arrange(Year)
  index = unique(d$Year)
  if(length(index) > 1){
    par(mfrow = c(length(index),2))
    for(j in 1:length(index)){
      d_sub = subset(d, d$Year == index[j])
      live = subset(d_sub, d_sub$Live_Dead == "L")
      dead = subset(d_sub, d_sub$Live_Dead == "D")
      hist(live[,2], main= paste0(locality[i], " ", index[j], " ", "Live"), xlab ="Height(mm)", ylab = "Oyster Size Density", xlim = c(0, 150), freq = FALSE, breaks = seq(0, 265, by = 5), col = 'gray')
      abline(v= 75.4, col = 'red', lwd = 2, lty = 2)
      hist(dead[,2], main= paste0(locality[i], " ", index[j], " ", "Dead"), xlab ="Height(mm)", ylab = "Oyster Size Density", xlim = c(0, 150), freq = FALSE, breaks = seq(0, 265, by = 5), col = 'gray')
      abline(v= 75.4, col = 'red', lwd = 2, lty = 2)
    }}else{cat(paste0("Only 1 year for ", locality[i], "; "))}
}


################### Density for Quadrat Data ####################

quad.agg = aggregate(Count ~ Month + Year + Locality + Site + Station , data = quad, sum, na.rm =T)

quad.agg$Density = ifelse(quad.agg$Locality == "LT", quad.agg$Count/0.025,quad.agg$Count / 0.0625)

stats        = aggregate(Density~ Site + Locality + Month + Year ,data=quad.agg, FUN=.sumstats)
stats        = do.call("data.frame",stats)                                    #flatten that matrix variable
names(stats) = gsub('Density.','',names(stats))                               #remove prefix from names made by 'aggregate'

stats$L95se[stats$L95se < 0] = 0
stats$Trip = paste0(stats$Year, "-", stats$Month)
stats$Trip2 = as.yearmon(stats$Trip, format = "%Y-%b")

stats = stats %>% arrange(Trip2) %>% filter(complete.cases(.))

#Plot density of oyster counts per trip by Locality 
trips = unique(stats$Trip)
par(mfrow=c(1,1),oma=c(0,0,0,0))
ymax = pretty(max(stats$U95se,na.rm=T))[2]+50                                 #this gets a nice number for y-axis range to use on all plots
for(i in trips){
  xy   = stats[stats$Trip== i,]
  
  if(dim(xy)[1]>2){
    avg = tapply(xy$Mean,list(xy$Locality,xy$Site),sum)
    low = tapply(xy$L95se,list(xy$Locality,xy$Site),sum)
    low[low<0] = 0                                                              #set minimum lower CI to 0
    up  = tapply(xy$U95se,list(xy$Locality,xy$Site),sum) 
    bp  = barplot(avg,ylim=c(0,ymax),beside=T,main=paste(i),axisnames=F,ylab='Oyster density m^2')
    mtext(1,at=as.vector(bp),text=rep(rownames(avg),3),line=1)
    mtext(1,at=colMeans(bp),text=c('Inshore','Nearshore','Offshore'),line=3)
    arrows(as.vector(bp),as.vector(low),as.vector(bp),as.vector(up),
           angle = 90,code = 3,length=0.06)}else{cat(paste0("Not enough data for ",i, "; "))}
}

#Plot density over time at each locality, sites stacked atop one another for quad aggregated data
locals = unique(quad$Locality)
par(mfrow=c(3,1),oma=c(0,0,4,0))
for(i in locals){
  xy          = stats[stats$Locality== i,]
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



#get full names for plots
stats$Locname  =  ifelse(stats$Locality=='CK','Cedar Key',
                         ifelse(stats$Locality=='CR','Corrigans Reef',
                                ifelse(stats$Locality == "GC", "GC", 
                                       ifelse(stats$Locality == "BT", "BT",
                                              ifelse(stats$Locality == "LT", "LT", 
                                                     ifelse(stats$Locality == "NN", "NN",
                                                            ifelse(stats$Locality=='HB','Horseshoe Beach','Lone Cabbage Reef')))))))
stats$Sitname  = ifelse(stats$Site=='I','Inshore',ifelse(stats$Site=='N','Nearshore','Offshore')) 

#make balloon plot

par(mfrow=c(1,1),oma=c(0,0,0,0))
for(i in trips){
  xy   = stats[stats$Trip==i,]
  if(dim(xy)[1]>1){
    balloondat  = as.table(with(xy,tapply(Mean,list(Locname,Sitname),sum)))
    balloonplot(balloondat,colmar=.5,rowmar=.75,show.margins=F,cum.margins=F,label.lines=F,ylab='',xlab='',colsrt=45,
                main= paste(i),sub=expression(paste("Oyster Density (m"^"2",")")))}else{cat(paste0("Not enough data for ", i, "; "))}
}


################################################################################
#
#    Control vs Treatment by year             
#
################################################################################


quad.treat = quad %>%  dplyr::select(Month, Year, Locality, Site, Bar, Substation, Count, Treatment) %>% 
  filter(Locality == "LC", Site == "O")

quad.treat = aggregate(Count ~ Month + Year + Locality + Site + Bar + Substation +  Treatment, data = quad.treat, sum, na.rm =T)
quad.treat$Density = quad.treat$Count / 0.25
quad.treat$Trip = paste(quad.treat$Month, quad.treat$Year)

sub = c("LCO11A","LCO10B","LCO8B","LCO9A") ### "LCO11A","LCO10B","LCO8B","LCO9A" are treated sites 


for(i in sub){
  d = quad.treat %>% filter(Substation == i)
  bp = barplot(d$Density, main = paste0( i), cex.names = 1 )
  mtext(1, at = as.vector(bp), text = d$Trip, line = 1, las = 2, cex = 0.7)
}






