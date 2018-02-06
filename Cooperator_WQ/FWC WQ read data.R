#code to read FWC SAS files with water quality data

setwd("C:/Users/billpine/Google Drive/GEBF Lone Cabbage Oyster/WQ/FWC_FIM")

#Note from Caleb
#I have attached a file that shows the structure of our water quality data.  
#I also attached our physical data, since you will need to merge the two together by Reference.  
#The physical data will provide location (GPS and Zone B), date, and time.

library(sas7bdat)


hydro=read.sas7bdat("ckm_2017_hydrolab.sas7bdat")
physical=read.sas7bdat("ckm_2017_physical.sas7bdat")

