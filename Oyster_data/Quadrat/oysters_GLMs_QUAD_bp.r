#oyster GLM
#setwd("C:/Users/Bill/Desktop/oyster glm")
setwd("F:/oyster glm")
library(MASS)
library(coefplot)

# Modeling grid density 
grid=read.csv("Grid_data_Nov_2012_bp.csv")
head(grid)
str(grid)
names(grid)
quadrat=subset(grid,Count!="NA")
#note working with counts of live and dead now, that's ok for now just add  & Live_Dead!="D" to only have live

# Aggregate data to count of oysters per quadrat, unique quadrat is identified by the 
#Dis_Alng & Dis_frm location if there are lengths associated then there is a 1 for the 
#count and then the length. After a certain number of lengths are measured then the number 
#becomes the remaining count, so sum of each unique quadrat is the
#count per quadrat


dat=aggregate(Count~Date+Month+Locality+Site+Bar+Station+Dist_Alng+Dist_frm,data=quadrat,FUN=sum)
#the above seems to be working for total number of oysters at each location

dat_1=aggregate(Count~Date+Month+Locality+Site+Bar+Station,data=dat,FUN=sum)
dat_1$quad_count=aggregate(Count~Date+Month+Locality+Site+Bar+Station,data=dat,FUN=length)[,7]


#check
#as an example Oct 16 2012 Inshore Bar 2 should have 13 quadrats
check=aggregate(Count~Date+Month+Locality+Site+Bar+Station,data=dat,FUN=length)
check2=aggregate(Count~Trip+Month+Locality+Site,data=quadrat,FUN=sum)

#Looks like it worked
check[check$Date=="10/16/2012"&check$Locality=="CK"&check$Site=="I"&check$Bar=="2",]  


#ok 1/2/2018 I think the above is working but need to double check. If working will go with
#this to run GLM by quadrat


head(dat_1)
length(dat_1[,1])
names(dat_1)


# Count Histogram, for assessing GLM family fit to count data
hist(dat_1$Count,breaks=40,freq=FALSE,col=8)
theta=c(mu=1,k=0.25)
nb_LL=function(theta)
	{
	-sum(dnbinom(dat_1$Count,mu=theta[1],size=theta[2],log=TRUE))
	}
fit_nb=optim(theta,nb_LL)
lines(seq(0,3000,100),dnbinom(seq(0,3000,100),mu=fit_nb$par[1],size=fit_nb$par[2]),col=2)

#Negative Binomial GLMs of Counts with Effort Offset

nb_glm=glm.nb(Count~Locality+Site+Month+offset(log(quad_count))+0,data=dat_1)
summary(nb_glm)

#This is a model of oyster density as a function of individual site
#Working with oyster counts and effort as offset

mod1=glm.nb(Count~Site:Locality+offset(log(quad_count))+0,data=dat_1)  #MODEl 1 DENSITY AS A FUNCTION OF SITE
summary(mod1)
coefplot(mod1)
plot(residuals(mod1))
abline(h=0)
#this tells you that the highest counts were LC Inshore, HB Inshore, and CR Inshore then CK Inshore


#PREDICTED MEAN COUNT PER 25 quads
#Can change Locality (LC HB CR CK), Site (O N I), effort
pred_LC_NS=predict(mod1,list(Locality="LC",Site="N",quad_count=1),type="response")
pred_LC_NS  #predicted mean count/grid in current state
pred_HB_IS=predict(mod1,list(Locality="HB",Site="I",quad_count=1),type="response")
pred_HB_IS  #predicted mean count of highest density natural reef

#ok let's predict LC Offshore and then compare that to the restored
pred_LC_OS=predict(mod1,list(Locality="LC",Site="O",quad_count=1),type="response")
pred_LC_OS  #predicted mean count/grid in current state


# RANDOM SAMPLE GENERATION, LC NEARSHORE BEFORE RESTORATION
#hist(rnbinom(50,mu=pred_LC_NS,size=mod1$theta),breaks=20,col=8)

# RANDOM SAMPLE GENERATION, LC OFFSHORE BEFORE RESTORATION
hist(rnbinom(50,mu=pred_LC_OS,size=mod1$theta),breaks=20,col=8)


#stop 1/3/2018
#stop 1/4/2018, data file below does not exist. needs to be built


##so this is the counts at those 50 sites

# NOW MODEL THE ACTUAL DATA FROM LC OFFSHORE BEFORE AND AFTER RESTORATION 
LCrestore=read.csv("grid_data_pilot_project_LCR_2013_2014.csv")
head(LCrestore)
LCR=aggregate(live~Date+location+grid+condition,dat=LCrestore,sum)
LCR$dead=aggregate(dead~Date+location+grid+condition,dat=LCrestore,sum)[,5]
LCR$effort=aggregate(distance~Date+location+grid+condition,dat=LCrestore,max)[,5]
LCR$treatment=as.factor(substring(LCR$location,3,9)) #this is controls and restored site, used as treatment effect

pre=subset(LCR,condition=="pre")
post=subset(LCR,condition=="post")
par(mfcol=c(2,1))
hist(pre$live,breaks=30,col=8,xlim=c(0,max(LCR$live)))
hist(post$live,breaks=30,col=8,xlim=c(0,max(LCR$live)))

# GLM TO MODEL RESTORATION EFFECT OF LIVE OYSTERS.  NEED TO DISCUSS THE CONTROL TREATMENT
mod2=glm.nb(live~treatment:condition+offset(log(quad_count)),data=LCR)
summary(mod2)
#positive is effect on mean on log scale
# e^3.32=27.7 and e^2.54=12.7
#3.32-2.54= effect of adding rock

mod3=glm.nb(live~treatment:offset(log(effort)),data=LCR)
summary(mod3)

#take out condition

# NOW PREDICT WHAT A SAMPLE FROM THE LC OFFSHORE SITE WILL LOOK LIKE AFTER RESTORATION EFFECT
hist(rnbinom(50,mu=predict(mod2,list(treatment="restore",condition="post",effort=25),type="response"),
	size=mod2$theta),breaks=30,col=8,xlab="Live count/25m",main="Predicted sample of Restored LC Reef")

#change this condition to pre or post OR restore or control
#post mean about 1500 pre about 75
 
# NOW PREDICT WHAT A SAMPLE FROM THE LC oFFSHRE SITE WILL LOOK LIKE AFTER RESTORATION EFFECT
hist(rnbinom(50,mu=0.5*predict(mod2,list(treatment="restore",condition="post",effort=25),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/25m",main="Predicted sample of Restored LC Reef")

#Here is reducing effect size by 50%

#unclear if this is the better way to "scale" the response or if it is better to manipulate "effort"



#####

#ok based on conversation 12/19/2017 w/ Matt, may be better to flip over to the quadrat data for doing the
#quadrat evaluations given the potential for more zeros in the quadrat data....as this is working now, the 
#effort offset is 
#####
#####
# NOW PREDICT WHAT 50 samples with an effort of 1/4 m2 area which is 25mX0.1524 width=3.81m2 is the original
# so to convert to 1/4 m2 quadrat that would be effort=1.64

##A SAMPLE FROM THE LC OFFSHORE SITE WILL LOOK LIKE AFTER RESTORATION EFFECT


#These are after the rocks have been added
#500 1/4m^2 quadrats
hist(rnbinom(500,mu=predict(mod2,list(treatment="restore",condition="post",effort=1.64),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/0.25m^2",main="Predicted # Live Oysters on Restored LC Reef")

#12 1.4m^2 quadrats
hist(rnbinom(30,mu=predict(mod2,list(treatment="restore",condition="post",effort=1.64),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/0.25m^2",main="Predicted # Live oysters of Restored LC Reef")

##control site

#500 1.4m^2 quadrats
hist(rnbinom(500,mu=predict(mod2,list(treatment="control",condition="post",effort=1.64),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/0.25m^2",main="Predicted # Live oysters of Control LC Reef")

#12 1.4m^2 quadrats
hist(rnbinom(12,mu=predict(mod2,list(treatment="control",condition="post",effort=1.64),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/0.25m^2",main="Predicted # Live oysters of Control LC Reef")
###
#These are before the rocks have been added, this is a site effect
###
#500 1/4m^2 quadrats
hist(rnbinom(500,mu=predict(mod2,list(treatment="restore",condition="pre",effort=1.64),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/0.25m^2",main="Predicted # Live Oysters on Unrestored LC Reef")

#12 1.4m^2 quadrats
hist(rnbinom(10,mu=predict(mod2,list(treatment="restore",condition="pre",effort=1.64),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/0.25m^2",main="Predicted # Live oysters of Unrestored LC Reef")

##control site

#500 1.4m^2 quadrats
hist(rnbinom(500,mu=predict(mod2,list(treatment="control",condition="pre",effort=1.64),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/0.25m^2",main="Predicted # Live oysters of Control LC Reef")

#12 1.4m^2 quadrats
hist(rnbinom(12,mu=predict(mod2,list(treatment="control",condition="pre",effort=1.64),type="response"),
             size=mod2$theta),breaks=30,col=8,xlab="Live count/0.25m^2",main="Predicted # Live oysters of Control LC Reef")
