#This is just an exercise to explore a variety of GLM models fit to
#snook catch data from CK FIM program and various water temperature
#and snook population size (from assessment) covariates

#This is designed as an example file to walk through a couple of things
#including examing a linear, poisson, and negative binomial GLM models, 
#as well as a framework for adding zero inflated models.  Also demonstrates
#testing for overdispersion, and thinking about how to fit covariates
#to these types of data.  A few different graphing approaches are used.

#Many of the ideas are from Zuur et al. "A beginner's guide to GLM and GLMM with R"

#Couple of points, the temperature covariates come from the water temperatre
#data measured with the fish sampling and are then converted to means
#for "winter" or "summer" months in each year
# Year explains most of the variation and the other covariates, or interactions
#among those covariates do explain a little more variance, but from
#looking at plots of the temperature data and snook catch they don't
#seem to show relationships between catch and the temperature data
#The relative abundance of snook from the assessment shows similar increasing
# trend over time and is worth exploring over time
# GLM models are easily overdispersed with these data and I thought the 
# overdispersion was from teh zero inflation, and that is one way it can 
# probably be addressed. But a negative-binomial GLM seems to fit pretty well
# and the interpretation might be easier with that.

#Overall improvements need to come from the development of a prior factors to 
#test and the data to inform those factors.

#code to read FWC SAS files

#setwd("C:/Users/billpine/Google Drive/GEBF Lone Cabbage Oyster/Fish/Snook")
setwd("C:/Users/Owner/Google Drive/GEBF Lone Cabbage Oyster/Fish/Snook")

#Note from Caleb
#Zone B is north and Zone C is south

library(sas7bdat)
library(MASS)
library(coefplot)
library(plyr)

install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB) #known of these models are in this file



dat=read.sas7bdat("ckm_cu_c.sas7bdat")
names(dat)

plot(dat$month, dat$number, pch=as.integer(dat$Zone))

snook=subset(dat,dat$month>3 & dat$month<11 & dat$Zone=="C") #only snook months 3-10 in Zone C
winter=dat[dat$month %in% c(1,2,11,12), ] #only temps from these months


## Observed frequency of occurrence of snook
snook$success=ifelse(snook$number>0,1,0)
mean_occur=aggregate(success~year,data=snook,mean) #note this is the mean of the successes
total_occur=aggregate(number~year,data=snook,sum) #note this is the sum catch
years=as.matrix(1997:2016)
colnames(years)=c("year")
freq_occur=merge(years,mean_occur,all.x=TRUE,all.Y=TRUE)
freq_occur=merge(mean_occur,total_occur,all.x=TRUE,all.Y=TRUE)

#summer water temps same as snook months
summer_temp=aggregate(temperature~year,data=snook,mean)

#winter water temps
winter_temp=aggregate(temperature~year,data=winter,mean)
#winter_temp=merge(freq_occur,winter_temp,all.x=TRUE,all.Y=TRUE)

#all water temps 
all_temp=aggregate(temperature~year,data=dat,mean)

par(mfrow = c(2,2))
plot(all_temp$temperature~all_temp$year)
plot(winter_temp$temperature~winter_temp$year)
plot(summer_temp$temperature~summer_temp$year)



#merge mean snook catch, winter teamp, annual temp
snook_temp=merge(freq_occur,all_temp,by="year",all.x=TRUE,all.Y=TRUE)
snook_temp=merge(snook_temp,winter_temp, by="year",all.x=TRUE,all.Y=TRUE)
snook2=merge(snook_temp,summer_temp, by="year",all.x=TRUE,all.Y=TRUE)


colnames(snook2)=c('year','mean_success','total_catch','all_t','winter_t', 'summer_t')


#read in relative snook abundance from snook assessment 1997-2014

GOM_N<-c(1195493,1197976,1455324,1735345,1486287,1414318,2118084,1950108,
                1587182,1330583,1127938,958978,927989,1031855,1228955,2011836,2772778,3581340)
max(GOM_N) 
year<-c(1997:2014)
rel_N<-GOM_N/max(GOM_N)
plot(year,rel_N)
aa<-cbind(year, rel_N)

snook3=merge(snook2,aa, by="year",all.x=TRUE,all.Y=TRUE)

#standardize things if want to use this later on per Zuur
snook3$rel_catch<-snook3$total_catch/(max(snook3$total_catch))
snook3$rel_w<-snook3$winter_t/(max(snook3$winter_t))
snook3$rel_s<-snook3$summer_t/(max(snook3$summer_t))

names(snook3)

colnames(snook3)=c('year','mean_success','total_catch','all_t','winter_t', 'summer_t', 'rel_N','rel_catch','rel_w','rel_s')

table(snook3$total_catch)

#############
#snook catch and GOM snook abundance

plot(snook3$rel_catch~snook3$year, xlab= "Year", ylab="Relative Snook catch Mar-Oct, Zone C")
lines(snook3$rel_N~snook3$year, col='red', type='p')


########
#Snook catch and temperatures
snook_fig <- data.frame(Y = rep(snook3$total_catch, 3),
                        X = c(snook3$summer_t, snook3$winter_t, snook3$all_t),
                        ID = rep(c("Mean summer Temp", 
                                   "Mean winter Temp", 
                                   "Mean all Temp"), each = nrow(snook3)))

p1 <- ggplot()
p1 <- p1 + xlab("Covariate") + ylab("Snook count")
p1 <- p1 + theme(text = element_text(size=15))
p1 <- p1 + geom_point(data = snook_fig, aes(x = X, y = Y))
p1 <- p1 + facet_grid(. ~ ID, scales = "free")
p1


###########
###########

fitdistr(snook3$total_catch,"Poisson")
fitdistr(snook3$total_catch,"negative binomial")
#interesting that just the catch isn't fit by NB



###########
#Zuur style analyses below

#working with the actual catch and temperature data and not the standardized



library(lattice)
library(MASS)

names(snook2)


# #make a simple graph snook catch Mar-Oct vs. Summer temp in Zone C
# xyplot(snook2$total_catch~snook2$summer_t, xlab= "Summer temperature", ylab="Snook catch Mar-Oct, Zone C")
# xyplot(snook3$total_catch~snook3$rel_N, xlab= "Relative GOM Snook N", ylab="Snook catch Mar-Oct, Zone C")
# 
# plot((snook3$total_catch/max(snook3$total_catch))~snook3$year, xlab= "Year", ylab="Relative Snook catch Mar-Oct, Zone C")
# lines(snook3$rel_N~snook3$year, col='red', type='p')

##
#ok will work through series of models starting with a simple linear
#model and then move into GLM models with different distributions
#knowing simple linear isn't likely to be a good fit, but a good example


#Fit simple linear model
M0<-lm(total_catch~year, data=snook2)
summary(M0)
#model isn't significant

#Get residuals and fitted values
E0 <- resid(M0)
F0 <- fitted(M0)

#residuals and fitted regression
par(mfrow = c(1,2), mar = c(5,5,3,2))
plot(x = F0, 
     y = E0,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x = snook2$year, 
     y = snook2$total_catch,
     xlab = "Year",
     ylab = "Snook catch",
     cex.lab = 1.5,
     pch = 16)
abline(M0, lwd = 5)


#Predicted and observed
#Linear model doesn't do a good job. negative fitted values

par(mfrow = c(1,1), mar = c(5,5,3,2))
plot(x = snook2$year, 
     y = snook2$total_catch,
     xlab = "Year",
     ylab = "Snook catch",
     cex.lab = 1.5,
     pch = 1,
     ylim = c(-50, 100))
abline(M0, lwd = 5)
abline(h = 0, lty = 2)

range(snook2$year)
md <- seq(1997, 2016, length = 20)

Beta <- coef(M0)
for (i in 1:10){
  mu <- Beta[1] + Beta[2] * md[i]
  yi <- rnorm(100, mean = mu, sd = summary(M0)$sigma)
  points(jitter(rep(md[i], 100)), jitter(yi), col = grey(0.5), pch = 16, cex = 1)
}

#now switch to Poisson GLM, again just year
M1 <- glm(total_catch~year, 
          data = snook2, 
          family = poisson(link = "log"))
summary(M1)

#######
#calculate explained deviance, similar to R2 for linear model

#100x((null dev-resid dev)/null dev)

100*((350.977-85.223)/350.977)

#so about 76% variation in snook catch explained by year

#This is observed snook catch vs year with the fitted Poisson GLM line
#improves over linear regression as it doesn't go to zero

par(mar = c(5,5,2,2))
MyData <- data.frame(year = seq(1997, 2017, length = 25))
P1 <- predict(M1, newdata = MyData, type = "response")
plot(x = snook2$year,
     y = snook2$total_catch,
     ylim = c(0,100),
     xlab = "Year",
     ylab = "Poisson Total snook catch", cex.lab = 1.5)

lines(MyData$year, P1, lwd = 3)

#this looks like a better fit 

#ok can simulate a bunch of values from a Poisson and 
#then plot those and see how it fits

HL <- seq(1997, 2017, length = 25)
Beta <- coef(M1)
for (i in 1:25){
  mu <- exp(Beta[1] + Beta[2] * HL[i])
  yi <- rpois(50, lambda= mu)
  points(jitter(rep(HL[i], 50)), 
         jitter(yi), col = grey(0.5), 
         pch = 16, cex = 1)
}

lines(MyData$year, P1, lwd = 3)

#simulated data not great


#
#check for overdispersion
E1<-resid(M1, type="pearson")
N<-nrow(snook2)
p<-length(coef(M1))
Dispersion <- sum(E1^2) / (N - p)
Dispersion
#high overdispersion

#overdispersion occurs when variation greater than what is expected for
#given distribution, should be around 1


##negative binomial

M2 <- glm.nb(total_catch~year, 
          data = snook2)
summary(M2)

#check for overdispersion
E2<-resid(M2, type="pearson")
N<-nrow(snook2)
p2<-length(coef(M2))+1
Dispersion.nb <- sum(E2^2) / (N - p2)
Dispersion.nb
#much lower dispersion, not bad

par(mar = c(5,5,2,2))
MyData <- data.frame(year = seq(1997, 2017, length = 25))
P2 <- predict(M2, newdata = MyData, type = "response")
plot(x = snook2$year,
     y = snook2$total_catch,
     ylim = c(0,100),
     xlab = "Year",
     ylab = "NB Total snook catch", cex.lab = 1.5)

lines(MyData$year, P2, lwd = 3)

#this looks like a better fit 

#ok can simulate a bunch of values from a neg binomial and 
#then plot those and see how it fits

HL <- seq(1997, 2017, length = 25)
Beta <- coef(M2)
for (i in 1:25){
  mu <- exp(Beta[1] + Beta[2] * HL[i])
  yi <- rpois(50, lambda= mu)
  points(jitter(rep(HL[i], 50)), 
         jitter(yi), col = grey(0.5), 
         pch = 16, cex = 1)
}

lines(MyData$year, P2, lwd = 3)


AIC(M0, M1, M2)

#so just working with year, plots and AIC suggest 
#negative binomial best

#############
#############
##ok start fitting models with summer, winter, and rel N, interactions w/ year
##recognizing that these temps are likely not independent
#pscl package is useful

#Poisson GLM

M3 <- glm(total_catch~year+summer_t+winter_t+rel_N-1, 
             data = snook3,family=poisson, maxit=30000)
summary(M3)

#remember Poisson has to be integer

#check for overdispersion
E3<-resid(M3, type="pearson")
N<-nrow(snook3)
p3<-length(coef(M3))
Dispersion.pois <- sum(E3^2) / (N - p3)
Dispersion.pois

#overdispersed, so parameter estimates potentially biased 


M4 <- glm.nb(total_catch~year+summer_t+winter_t+rel_N-1, 
             data = snook3, maxit=3000)
summary(M4)

#check for overdispersion
E4<-resid(M4, type="pearson")
N<-nrow(snook3)
p4<-length(coef(M4))+1
Dispersion.nb <- sum(E4^2) / (N - p4)
Dispersion.nb

#not overdispersed, might be underdispersed, but not too bad

AIC(M3, M4)
#AIC suggests NB is better fit

#could go zero inflated, but would need to take out time as 
#fixed factor

##zero inflated
library(pscl)
M5 <- zeroinfl(total_catch~summer_t+winter_t+rel_N-1, 
             data = snook3)
summary(M5)

E5<-resid(M5, type="pearson")
N<-nrow(snook3)
p5<-length(coef(M5))
Dispersion.zip <- sum(E5^2) / (N - p5)
Dispersion.zip

#still overdispersed







