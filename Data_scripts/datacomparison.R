######## Comparing Data Scripts #############
##### K. Zarada ######
##### Feb 23 ########

library(devtools)
#read in data files
dat1 = read.csv("/Users/katiezarada/Desktop/Oysters/OysterData_16Feb2018_Quadrat_SB.csv", header =TRUE, stringsAsFactors = FALSE)
dat2 = read.csv("/Users/katiezarada/Desktop/Oysters/OysterData_16Feb2018_QuadratSL .csv", header =TRUE, stringsAsFactors = FALSE)


#get the list for each column where there are no matches
nomatch = list()
for(i in 1:dim(dat2)[2]){
  
  nomatch[[i]] = which(dat2[,i] %in% dat1[,i] == "FALSE")
  
}

nomatch.SL = nomatch[c(9,10,11,13)]
names(nomatch.SL) = c("Quad_Lat", "Quad_Long", "Center_Lat", "Rand_Coord" )

nomatch2 = list()
for(i in 1:dim(dat1)[2]){
  
  nomatch2[[i]] = which(dat1[,i] %in% dat2[,i] == "FALSE")
  
}


nomatch.SB = nomatch2[c(9,10,13)]
names(nomatch.SB) = c("Quad_Lat", "Quad_Long", "Rand_Coord")



#row by row comparison 

mistakes = list()
for(i in 1:dim(dat2)[1]) {
 
    mistakes[[i]] = dat1[i,] %in% dat2[i,]
  
}

mistakes.df = data.frame()

for(i in 1:length(mistakes)){
  for(j in 1: 20){
    
    mistakes.df[i,j] = mistakes[[i]][j]
  }
}


which(mistakes.df$Count == "FALSE")

mistakes.cols = list()

for(i in 1:dim(mistakes.df)[2]){
  
  mistakes.cols[[i]] = which(mistakes.df[,i] == "FALSE")
  
}

names(mistakes.cols) = colnames(dat2)




for(i in 1:dim(dat2)[2]){
  for(j in 1:dim(dat1)[1]){
    
    mistakes[[i]] = dat1[i,j] == dat2[i,j]
  }
}

mistakes.df = data.frame()

for(i in 1:length(mistakes)){
  for(j in 1: 20){
    
    mistakes.df[i,j] = mistakes[[i]][j]
  }
}

mistakes.cols = list()

for(i in 1:dim(mistakes.df)[2]){
  
  mistakes.cols[[i]] = which(mistakes.df[,i] == "FALSE")
  
}

names(mistakes.cols) = colnames(dat2)


