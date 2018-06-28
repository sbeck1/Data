library(reshape)
d1<-read.csv("ex_old.csv")
names(d1)

#use melt from reshape
d2 <- melt(d1, id=c("State","X","Y"))

#read the first 4 characters of the new "variable" to capture Live or Dead
d2$Fate <- substr(d2$variable, start=1, stop=4)

#next use cast to sum the counts at each unique X Y combination by reef
#figure out what to do with the "over" in the actual data file

## cast the melted data
# cast(data, formula, function) 
#subjmeans <- cast(mdata, id~variable, mean)
#timemeans <- cast(mdata, time~variable, mean)
