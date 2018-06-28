####################################################################
#
#               Quadrat GLMS 
#               K. Zarada May 2018
#
####################################################################


#oyster GLM
setwd("/Users/fas/Desktop/Oysters/Analyses/Oyster_data/Quadrat/")
library(MASS)
library(coefplot)
library(RCurl)
require(fitdistrplus)
# Modeling transect density 
# These are the transect data from the 2009-2011 epoch
quadurl = getURL("https://raw.githubusercontent.com/LCRoysterproject/Data/master/Oyster_data/Quadrat/quadrat_combined.csv")
quad = read.csv(text = quadurl)


quad$Date = as.Date(quad$Date, format = "%m/%d/%y")
#quad$Year = format(quad$Date, "%Y")

# Aggregate data to count of oysters per transect
dat=aggregate(Count~Date+Month+ Year+Locality+Site+Bar+Substation,dat=quad,sum)
#the above sums the counts of oysters at each sampling site by date

dat$effort=aggregate(Quadrat_ID~Date+Month+ Year +Locality+Site+Bar+Substation,dat=quad,length)[,8]
#the above calculates the number of quadrats at a site

head(dat)
length(dat[,1])

# Count Histogram, for assessing GLM family fit to count data
hist(dat$Count,breaks=40,freq=FALSE,col=8)
theta=c(mu=1,k=0.2)
nb_LL=function(theta)
{
  -sum(dnbinom(dat$Count,mu=theta[1],size=theta[2],log=TRUE))
}
fit_nb=optim(theta,nb_LL)
lines(seq(0,4000,100),dnbinom(seq(0,4000,100),mu=fit_nb$par[1],size=fit_nb$par[2]),col=2)

## Using Fitdistr to look at fit
fit = fitdistr(dat$Count, "negative binomial")
hist(dat$Count, pch=20, breaks=25, prob=TRUE, main="")
lines(seq(0,900,10),dnbinom(seq(0,900,10), mu = fit$estimate[2], size = fit$estimate[1]), col="red", lwd=2)


fit2 = fitdistr(dat$Count, "poisson")
hist(dat$Count, pch=20, breaks=25, prob=TRUE, main="")
lines(seq(0,900,10),dpois(seq(0,900,10), lambda= fit2$estimate[1]), col="red", lwd=2)
plotdist(dat$Count,"pois", para=list(lambda = fit2$estimate[1]))

#nbiom fits better



#Negative Binomial GLMs of Counts with Effort Offset

#the effort offset is controling for variation in the amount of area
#in the quadrats. Could inlcude effort as a factor and see if it were significang
#as done by Zuur, but this is the "NOAA way".  Might not work well for low 
#densities however as the relationship between effort and counts might be flat

nb_glm=glm.nb(Count~Locality+Site+Month+Year +offset(log(effort))+0,data=dat)
summary(nb_glm)

#This is a model of oyster density as a function of individual site
#Working with oyster counts and effort as offset

mod1=glm.nb(Count~Site:Locality+offset(log(effort))+0,data=dat)  
#MODEl 1 DENSITY AS A FUNCTION OF SITE
summary(mod1)
coefplot(mod1)
plot(residuals(mod1))
abline(h=0)


## predict not working right now
#PREDICTED MEAN COUNT PER 50 quadrats
#Can change Locality (LC HB CR CK), Site (O N I), effort
pred_LC_NS=predict(mod1,list(Locality="LC",Site="N",effort=50),type="response")
pred_LC_NS 
pred_HB_IS=predict(mod1,list(Locality="HB",Site="I",effort=50),type="response")
pred_HB_IS 

#ok let's predict counts LC Offshore and then compare that to the restored
#as that's the site we are really interested in...that's where 2018 restoration will take place
pred_LC_OS=predict(mod1,list(Locality="LC",Site="O",effort=50),type="response")
pred_LC_OS  #predicted mean count/transect in current state


# RANDOM SAMPLE GENERATION, LC NEARSHORE BEFORE RESTORATION
#hist(rnbinom(50,mu=pred_LC_NS,size=mod1$theta),breaks=20,col=8)

# RANDOM SAMPLE GENERATION, LC OFFSHORE BEFORE RESTORATION
hist(rnbinom(50,mu=pred_LC_OS,size=mod1$theta),breaks=20,col=8)


##so this is the counts at those 50 sites, shows the range of possible counts

###################
###################
# NOW MODEL THE ACTUAL DATA FROM LC OFFSHORE BEFORE AND AFTER RESTORATION 
library(lubridate)

head(quad)
LC = subset(quad, quad$Locality == 'LC' & quad$Site == "O")
LCRyr=aggregate(Count~Date+Year+Substation + Treatment,dat=LC,sum)
LCRyr$effort=aggregate(Quadrat_ID~Date+Year+Substation+Treatment,dat=LC, length)[,5]
LCRyr$Year=as.factor(LCRyr$Year)
LCRyr$Substation = factor(LCRyr$Substation)



table (LCRyr$Treatment,LCRyr$Year) 

head(LCRyr)

str(LCRyr)
table(LCRyr$Substation,LCRyr$Treatment)

#pre=subset(LCRyr,Year == "2010"|Year == "2011"|Year == "2012"|Year == "2013") #not sure this is needed
#post=subset(LCRyr,Year == "2014"|Year == "2015"|Year == "2017"|Year == "2018") #not sure this is needed

boxplot(LCRyr$Count~LCRyr$Substation, col=8)

box_plot=function(substation)
{
  temp=subset(LCRyr,Substation==substation)
  boxplot(temp$Count~temp$Year,main=paste(substation))
  abline(v=10.5)
}
par(mfcol=c(2,2))
par(mai=c(0.4,0.4,0.3,0.3))
box_plot("LCO9A")
box_plot("LCO8B")
box_plot("LCO10B")
box_plot("LCO11A")


substation = unique(LCRyr$Substation)
for(i in substation) box_plot(i)

#pre/post still isn't very informative because post is really just the time
#just pre or post putting the rocks out.  Need a treatment/control column
#Note that treatment label also applied to 2013 sites with no rocks


#help you think about replicates per site
# variation among sites and years

#########################################################
# GLM TO MODEL RESTORATION EFFECT OF LIVE OYSTERS.  
#+0 forces intercept through zero, so check this my doing a confint(mod) to see if
#it contains zero, of so then can force to zero
#so rocks/norocks captures everything about location given all the locations are LC


full.mod<-glm.nb(Count~Year+Treatment+Substation+offset(log(effort))-1,data=LCRyr)
summary(full.mod)
coefplot(full.mod)
reduced.model<-step(full.mod,direction="backward")
summary(reduced.model)

mod<-glm.nb(Cnt_Live~Substation:Year+offset(log(effort)),data=LCRyr)
summary(mod)
confint(mod)


mod1<-glm.nb(Cnt_Live~Year*Treatment+offset(log(effort))-1,data=LCRyr)
summary(mod1)
coefplot(mod1)


mod2=glm.nb(Cnt_Live~Treatment:Year+offset(log(effort))+0,data=LCRyr)
summary(mod2)
#positive is effect on mean on log scale
# e^3.32=27.7 and e^2.54=12.7
#3.32-2.54= effect of adding rock

mod3=glm.nb(Cnt_Live~Treatment+offset(log(effort))+0,data=LCRyr)
summary(mod3)
#Not that informative as treatment label also applied to 2013 sites 
#with no rocks

mod4=glm.nb(Cnt_Live~Year*Substation+offset(log(effort))+0,data=LCRyr)
summary(mod4)

