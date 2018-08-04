library("tidyverse")

dat<- read_csv("example_survey.csv")

#create Element_id_2 as a factor to specify the order of plots in the facet wrap, not within each individual facet
dat$Element_id_2<-factor(dat$Element_id, levels =c ("5", "7", "8a", "9b", "10b", "11b"))

windows(record=T)

p1<-ggplot(data=dat) +
  aes(dat$Elev) + 
  labs(x="Elev", y="Frequency", title="Elevation of rock top surface") +
  geom_histogram(breaks=seq(-2.5, 0.5, by=0.01)) +
  xlim(c(-2.5,0.5)) +
  ylim(c(0,10))

p1


p2<-p1+
  facet_wrap(~Element_id) +
  labs(title = "Element_id")+
  geom_vline(xintercept = -2, color = "black", size=1, linetype = 2)

p2

#now try with Element_id_2 which is a factor
p2.1<-p1+
  facet_wrap(~Element_id_2) +
  labs(title = "Element_id_2")+
  geom_vline(xintercept = -2, color = "black", size=1, linetype = 2)
p2.1

##take a look at reef element 5 in plot 2 there are no values shown below -2
##however in plot 2.1 there are values shown below -2
##there are no data values for reef element 5 below -2 in the dataset dat