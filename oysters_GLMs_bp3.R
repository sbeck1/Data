#this is the original oyster GLM analyses
#based on the transect data only
#it is mostly the 2009-2011 data but brings in
#the Lone Cabbage restoration epoch for the simulations at the end

#remember what we are trying to do, fit the GLM models to describe counts at spatial locations
#during the 2009-2011 period. Then we want to see how those counts compare to the "Lone Cabbage
#restoration pilot project where all of the Lone Cabbage Reefs were surveyed in winter/spring 2013 and then
#rocks were added in fall.  Lone Cabbage sites were then re-surveyed in 2014 and then again in 2017
#now in 2018 we are starting to revisit more of the 2009-2011 sites as well as the Lone Cabbage
#sites so we want to be able to say "this is what we would predict the counts would look like
#at all these locations based on our 2009-2011 work and now as we revisit in 2017-2018 these
#are the observations of what the counts are at these sites.  This needs to be examined for
#both the transects and the quadrats.  Transect data is in better shape so that is why
#we are working on this.

#oyster GLM
#setwd("C:/Users/Bill/Desktop/oyster glm")
setwd("D:/oyster glm")
library(MASS)
library(coefplot)

# Modeling transect density 
# These are the transect data from the 2009-2011 epoch
transect=read.csv("Transect_data_Nov_2012_bp.csv")
head(transect)
trans=subset(transect,TransLngth!="NA")

# Aggregate data to count of oysters per transect
dat=aggregate(Cnt~Date+Trip+Month+Locality+Site+Bar+Station,dat=trans,sum)
#the above adds up the counts of the oysters in each segment of transect

dat$effort=aggregate(TransLngth~Date+Trip+Month+Locality+Site+Bar+Station,dat=trans,max)[,8]
#the above calculates the total transect length by looking at the max of the transect segments 
#as example May CK inshore bar 1 max is 24.9 and is 24.9 for effort in dat$effort

head(dat)
length(dat[,1])

# Count Histogram, for assessing GLM family fit to count data
hist(dat$Cnt,breaks=40,freq=FALSE,col=8)
theta=c(mu=1,k=0.2)
nb_LL=function(theta)
	{
	-sum(dnbinom(dat$Cnt,mu=theta[1],size=theta[2],log=TRUE))
	}
fit_nb=optim(theta,nb_LL)
lines(seq(0,4000,100),dnbinom(seq(0,4000,100),mu=fit_nb$par[1],size=fit_nb$par[2]),col=2)

#looks pretty good, could evaluate with fitdistr or something like that

#Negative Binomial GLMs of Counts with Effort Offset

#the effort offset is controling for variation in the amount of area
#in the quadrats. Could inlcude effort as a factor and see if it were significang
#as done by Zuur, but this is the "NOAA way".  Might not work well for low 
#densities however as the relationship between effort and counts might be flat

nb_glm=glm.nb(Cnt~Locality+Site+Month+offset(log(effort))+0,data=dat)
summary(nb_glm)

#This is a model of oyster density as a function of individual site
#Working with oyster counts and effort as offset

mod1=glm.nb(Cnt~Site:Locality+offset(log(effort))+0,data=dat)  
#MODEl 1 DENSITY AS A FUNCTION OF SITE
summary(mod1)
coefplot(mod1)
plot(residuals(mod1))
abline(h=0)
#this tells you that the highest counts were LC Inshore, 
#HB Inshore, and CR Inshore then CK Inshore


#PREDICTED MEAN COUNT PER 25 METER TRANSECT
#Can change Locality (LC HB CR CK), Site (O N I), effort
pred_LC_NS=predict(mod1,list(Locality="LC",Site="N",effort=25),type="response")
pred_LC_NS  #predicted mean count/transect in current state
pred_HB_IS=predict(mod1,list(Locality="HB",Site="I",effort=25),type="response")
pred_HB_IS  #predicted mean count of highest density natural reef

#ok let's predict counts LC Offshore and then compare that to the restored
#as that's the site we are really interested in...that's where 2018 restoration will take place
pred_LC_OS=predict(mod1,list(Locality="LC",Site="O",effort=25),type="response")
pred_LC_OS  #predicted mean count/transect in current state


# RANDOM SAMPLE GENERATION, LC NEARSHORE BEFORE RESTORATION
#hist(rnbinom(50,mu=pred_LC_NS,size=mod1$theta),breaks=20,col=8)

# RANDOM SAMPLE GENERATION, LC OFFSHORE BEFORE RESTORATION
hist(rnbinom(50,mu=pred_LC_OS,size=mod1$theta),breaks=20,col=8)


##so this is the counts at those 50 sites, shows the range of possible counts

###################
###################
# NOW MODEL THE ACTUAL DATA FROM LC OFFSHORE BEFORE AND AFTER RESTORATION 
#These are the data from the LC Pilot Project
###################
#Below are LCR pilot project data 2013, 2014, 2017
###################

##working with year
library(lubridate)

LCyear=read.csv("LCR_oyster_transect_2017.csv")
library(lubridate)
LCyear$Date=as.Date(LCyear$Date,"%m/%d/%Y") 
LCyear$year=year(LCyear$Date)

# #rocks = sites 1, 2, 5, 6
# #norocks = sites 3, 4, 7, 8

#2013 = pre addition of rocks
#2014 = post addition of rocks (sites 1, 2, 5, 6) control at sites 3, 4, 7,8
#2017 = same as 2014 just two years later (rocks are still on site)

head(LCyear)
LCRyr=aggregate(live~Date+year+location+transect+condition,dat=LCyear,sum)
LCRyr$dead=aggregate(dead~Date+year+location+transect+condition,dat=LCyear,sum)[,6]
LCRyr$effort=aggregate(distance~Date+year+location+transect+condition,dat=LCyear,max)[,6]
LCRyr$treatment=as.factor(substring(LCRyr$location,3,9)) #this is controls and restored site, used as treatment effect
LCRyr$year=as.factor(LCRyr$year)

##Need to fix as treatment label also applied to 2013 sites 
#with no rocks, so that's why create the rocks variable below.  This should fix it

LCRyr$rocks=ifelse(LCRyr$treatment=='restore' & 
                     LCRyr$condition=="post","rock","norock")

table (LCRyr$rocks,LCRyr$year) #looks like rock is the better way to characterize treatment
#need to make sure that you need rock, condition, and, treatment.  condition
#and treatment may not be correct for year==2013 (year prior to rock being added)
#because.  may just need to work with rock and year

head(LCRyr)

str(LCRyr)
table(LCRyr$location,LCRyr$treatment)

pre=subset(LCRyr,condition=="pre") #not sure this is needed
post=subset(LCRyr,condition=="post") #not sure this is needed

boxplot(LCRyr$live~LCRyr$location, col=8)

box_plot=function(Location,Condition)
{
  temp=subset(LCRyr,location==Location&condition==Condition)
  boxplot(temp$live~temp$year,col=8,main=paste(Location,"-",Condition))
  abline(v=10.5)
}
par(mfcol=c(2,4))
par(mai=c(0.4,0.4,0.3,0.3))
box_plot("LCcontrol3","pre")
box_plot("LCcontrol3","post")
box_plot("LCcontrol4","pre")
box_plot("LCcontrol4","post")
box_plot("LCcontrol7","pre")
box_plot("LCcontrol7","post")
box_plot("LCcontrol8","pre")
box_plot("LCcontrol8","post")

#pre/post still isn't very informative because post is really just the time
#just pre or post putting the rocks out.  Need a treatment/control column
#Note that treatment label also applied to 2013 sites with no rocks

#below I just do everything by location
box_plot2=function(Location)
{
  temp=subset(LCRyr,location==Location)
  boxplot(temp$live~temp$year,col=8,ylim=c(0,1000),main=paste(Location))
  abline(v=10.5)
}
par(mfcol=c(4,2))
par(mai=c(0.4,0.4,0.3,0.3))
box_plot2("LCcontrol3")
box_plot2("LCcontrol4")
box_plot2("LCcontrol7")
box_plot2("LCcontrol8")
box_plot2("LCrestore1")
box_plot2("LCrestore2")
box_plot2("LCrestore5")
box_plot2("LCrestore6")
mtext("Year",1,outer=TRUE,line=-1)
mtext("Live Oyster Count by Transect",2,outer=TRUE,line=-1.2)

#help you think about replicates per site
# variation among sites and years

#########################################################
# GLM TO MODEL RESTORATION EFFECT OF LIVE OYSTERS.  
#+0 forces intercept through zero, so check this my doing a confint(mod) to see if
#it contains zero, of so then can force to zero
#so rocks/norocks captures everything about location given all the locations are LC


full.mod<-glm.nb(live~year+rocks+location+offset(log(effort))-1,data=LCRyr)
summary(full.mod)
coefplot(full.mod)
reduced.model<-step(full.mod,direction="backward")
summary(reduced.model)

mod<-glm.nb(live~year+offset(log(effort)),data=LCRyr)
summary(mod)
confint(mod)


mod1<-glm.nb(live~year*rocks+offset(log(effort))-1,data=LCRyr)
summary(mod1)
coefplot(mod1)


mod2=glm.nb(live~treatment:condition:year+offset(log(effort))+0,data=LCRyr)
summary(mod2)
#positive is effect on mean on log scale
# e^3.32=27.7 and e^2.54=12.7
#3.32-2.54= effect of adding rock

mod3=glm.nb(live~treatment+offset(log(effort))+0,data=LCRyr)
summary(mod3)
#Not that informative as treatment label also applied to 2013 sites 
#with no rocks

mod4=glm.nb(live~year*location+offset(log(effort))+0,data=LCRyr)
summary(mod4)

