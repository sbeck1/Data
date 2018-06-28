############################################
#       LCO Graphs
#       K. Zarada
##############################################

#load packages 
library(tidyverse)
library(ggplot2)
library(gplots)
library(zoo)
library(RCurl)


#load data 
tranurl = getURL("https://raw.githubusercontent.com/LCRoysterproject/Data/master/Oyster_data/Transect/transect_combined.csv")
tran = read.csv(text = tranurl)

quadurl = getURL("https://raw.githubusercontent.com/LCRoysterproject/Data/master/Oyster_data/Quadrat/quadrat_combined.csv")
quad = read.csv(text = quadurl)

#modify Date, Year, Month 
tran$Date   = as.Date(tran$Date, format ="%Y-%m-%d")  #convert date from factor to date
tran$Month  = factor(format(tran$Date,"%B"),levels=c(month.name),ordered=T)   #make month an ordered factor, makes plots go in correct order (rather than alphabetic)
tran$Year   = factor(format(tran$Date, "%Y"))   #pull out year
tran$Month = month.abb[tran$Month] 

quad$Date = as.Date(quad$Date, format = '%m/%d/%y')

# Subset out LCO 
LCO = tran %>% filter(Locality == "LC" & Site == "O")

LCO.quad.1 = quad %>% filter(Locality == "LC" & Site == "O")



#ColorBlind friendly palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbeleven = c("#fc4d7c",
              "#00c77d",
              "#822295",
              "#3f9317",
              "#c194ff",
              "#f9d059",
              "#01a8f7",
              "#910013",
              "#cbdc87",
              "#ff8bdd",
              "#545400")

##########################################################
#    Summarize data by month and year 
############################################################

#Function to compute multiple summary stats in aggregate function.
.sumstats = function(x) c(Mean=mean(x,na.rm=T), Sd=sd(x,na.rm=T), Var=var(x,na.rm=T), Nobs=length(na.omit(x)),
                          CV    = sd(x,na.rm=T)/mean(x,na.rm=T),
                          se    = sd(x,na.rm=T)/sqrt(length(na.omit(x))),
                          L95se = mean(x,na.rm=T)-1.96*(sd(x,na.rm=T)/sqrt(length(na.omit(x)))),
                          U95se = mean(x,na.rm=T)+1.96*(sd(x,na.rm=T)/sqrt(length(na.omit(x)))))

#get transect length per each bar sampled
max_tran  = aggregate(TransLngth ~ Month + Year + Substation ,data=LCO,max,na.action=na.pass)

#Get total counts per each bar sampled
LCO_cnt = aggregate(Cnt_Live~ Month + Year + Substation,data=LCO,sum,na.rm=T,na.action=na.pass)         
LCO_cnt = merge(LCO_cnt,max_tran) 
LCO_cnt = LCO_cnt%>% arrange(Year)
LCO_cnt$Trip = paste0(LCO_cnt$Year, "-", LCO_cnt$Month)

#calculate density
LCO_cnt$TranArea  = LCO_cnt$TransLngth*0.1524     #0.1524 is width of transect
LCO_cnt$Density   = LCO_cnt$Cnt/LCO_cnt$TranArea 

############################################################################
#
#           Time series of LCO Transect Density by Year 
#
##############################################################################

ggplot(data = LCO_cnt, aes(x= Year, y = Density, fill = Year)) + 
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_manual(values = c(rep("grey54", 7))) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26),legend.position = "none")


############################################################################
#
#           Time series of LCO Transect Density- by Site
#
##############################################################################

LCO_site = LCO_cnt %>% filter(Year != "2010", Year != "2011", Year != "2012" ) #only 3 sites were sampled pre 2013
LCO_site$Year = factor(LCO_site$Year) #remove the extra levels

ggplot(data = LCO_site, aes(x = Year, y = Density, group = Substation, color = Substation)) + 
         geom_line(size = 1.2) + scale_color_manual(values= cbbPalette) + theme_bw() + 
        theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26) , legend.title = element_text(size = 22), legend.text = element_text(size = 20))

############################################################################
#
#           QUADRAT DENSITY 
#
##############################################################################

LCO.quad = aggregate(Count ~ Month + Year +  Substation , data = LCO.quad.1, sum, na.rm =T)

LCO.quad$Density = LCO.quad$Count / 0.0625
LCO.quad$Year = as.factor(LCO.quad$Year)
LCO.quad = LCO.quad %>% arrange(Year)

############################################################################
#
#           Time series of LCO Quadrat Density by Year 
#
##############################################################################

ggplot(data = LCO.quad, aes(x= Year, y = Density, fill = Year)) + 
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_manual(values = c(rep("grey54", 7))) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26),legend.position = "none")

############################################################################
#
#           Time series of LCO Transect Density- by Site
#
##############################################################################

LCO.quad.site = LCO.quad %>% filter(Year != "2010", Year != "2011", Year != "2012", Year != "2018" ) #only 3 sites were sampled pre 2013
LCO.quad.site$Year = factor(LCO.quad.site$Year) #remove the extra levels
LCO.quad.site$Substation = factor(LCO.quad.site$Substation)

ggplot(data = LCO.quad.site, aes(x = Year, y = Density, group = Substation, color = Substation)) + 
  geom_line(size = 1.2) + scale_color_manual(values= cbeleven) +theme_bw() + 
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26) , legend.title = element_text(size = 22), legend.text = element_text(size = 20))

############################################################################
#
#           Size Structure
#
##############################################################################
LCO.quad.1$Year = as.factor(LCO.quad.1$Year)
LCO.quad.1 = LCO.quad.1 %>% arrange(Year)

ggplot(LCO.quad.1, aes(x=Size, color=Year)) + theme_bw() +
  stat_density(aes(group = Year), position= "stack", geom = "line", size = 1.5) + scale_color_manual(values = cbbPalette) + 
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26) , legend.title = element_text(size = 22), legend.text = element_text(size = 20))



LCO.quad.1 %>% select(Substation,  Treatment) %>% distinct()
LCO.quad.1$T2 = ifelse(LCO.quad.1$Substation == "LCO9A"|LCO.quad.1$Substation == "LCO8B"|LCO.quad.1$Substation == "LCO10B"|LCO.quad.1$Substation == "LCO11A", paste("rocks"), paste("norocks"))

LCO.rock = LCO.quad.1 %>% filter(T2 == "rocks")
LCO.norock = LCO.quad.1 %>% filter(T2 == "norocks")

ggplot(LCO.rock, aes(x=Size, color=Year)) + theme_bw() +
  stat_density(aes(group = Year), position= "stack", geom = "line", size = 1.5) + scale_color_manual(values = cbbPalette) + 
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26) , legend.title = element_text(size = 22), legend.text = element_text(size = 20))

ggplot(LCO.norock, aes(x=Size, color=Year)) + theme_bw() +
  stat_density(aes(group = Year), position= "stack", geom = "line", size = 1.5) + scale_color_manual(values = cbbPalette) + 
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26) , legend.title = element_text(size = 22), legend.text = element_text(size = 20))



############################################################################
#
#          2017 & 2018 Samples Transect Density
#
##############################################################################

tran17 = tran %>% filter(Year == "2017"| Year == "2018")
tran17$Year = factor(tran17$Year)

#get transect length per each bar sampled
max_tran  = aggregate(TransLngth ~ Month + Year + Substation ,data= tran17,max,na.action=na.pass)

#Get total counts per each bar sampled
tran17_cnt = aggregate(Cnt_Live~ Month + Year + Substation,data=tran17,sum,na.rm=T,na.action=na.pass)         
tran17_cnt = merge(tran17_cnt,max_tran) 
tran17_cnt = tran17_cnt%>% arrange(Year)
tran17_cnt$Trip = paste0(tran17_cnt$Year, "-", tran17_cnt$Month)

#calculate density
tran17_cnt$TranArea  = tran17_cnt$TransLngth*0.1524     #0.1524 is width of transect
tran17_cnt$Density   = tran17_cnt$Cnt/tran17_cnt$TranArea 


ggplot(data = tran17_cnt, aes(x= Substation, y = Density, fill = Substation)) + 
  geom_bar(stat= "identity") + 
  theme_bw() + 
  scale_fill_manual(values = c(rep("grey54", 9))) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26),legend.position = "none")

############################################################################
#
#          2017 & 2018 Samples Quadrat Density
#
##############################################################################
quad17 = quad %>% filter(Year == "2017"| Year == "2018")
quad17$Year = factor(quad17$Year)

quad17.agg= aggregate(Count ~ Month + Year +  Substation , data = quad17, sum, na.rm =T)

quad17.agg$Density = quad17.agg$Count / 0.0625
quad17.agg$Year = as.factor(quad17.agg$Year)
quad17.agg = quad17.agg %>% arrange(Year)


ggplot(data = quad17.agg, aes(x= Substation, y = Density, fill = Year)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  scale_fill_manual(values = c(rep("grey54", 7))) +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26),legend.position = "none")

############################################################################
#
#          2017 & 2018 Samples Quadrat Size Structure
#
##############################################################################



ggplot(quad17, aes(x=Size, color=Locality)) + theme_bw() +
  stat_density(aes(group = Locality), position= "stack", geom = "line", size = 1.5) + scale_color_manual(values = cbbPalette) + 
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26) , legend.title = element_text(size = 22), legend.text = element_text(size = 20))











