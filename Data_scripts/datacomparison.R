######## Comparing Data Scripts #############
##### K. Zarada ######
##### Feb 23 ########

library(devtools)
#read in data files
dat1 = read.csv("/Users/fas/Desktop/Oysters/20180302_oyster_quadrat_MM.CSV", header =TRUE, stringsAsFactors = FALSE)
dat2 = read.csv("/Users/fas/Desktop/Oysters/20180302_oyster_quadrat_SB.CSV", header =TRUE, stringsAsFactors = FALSE)

#first, are the columns and rows the same? 
dim(dat1)
dim(dat2)


#row by row comparison 

mistakes = list()       
for(i in 1:dim(dat2)[1]) {
 
    mistakes[[i]] = dat1[i,] %in% dat2[i,]  #row by row comparison of which dat2 is not in dat1
  
}

mistakes.df = do.call(rbind,mistakes) #change list to data.frame 

mistakes.cols = list()  #loop to pull out FALSE reads from above into a list

for(i in 1:dim(mistakes.df)[2]){
  
  mistakes.cols[[i]] = which(mistakes.df[,i] == "FALSE")
  
}

names(mistakes.cols) = colnames(dat2)   #renaming lists by the columns they represent

mistakes.cols         #list of columns and the row numbers that do not match up 


#returning the chunks that do not match up 

mistakes.cells = list()
for(i in 1:length(mistakes.cols)) {
  
    mistakes.cells[[i]] = cbind(dat1[mistakes.cols[[i]],i],dat2[mistakes.cols[[i]],i])
}







