# This script is to convert decimal degrees to UTM
#
# Oyster Quad data is in DD, we want UTMs
# Cedar Key area: UTM zone 17
# KZ 6.25.2018

library(rgdal)

setwd("/Users/fas/Desktop/Oysters/Analyses/")

convert_lonlat_utm = function(dat,xcolumn,ycolumn) {
  df = dat[,c(xcolumn,ycolumn)]
  # get rows of non-NA values
  value.rows = which(complete.cases(df))
  # remove NAs from data to convert
  values = df[complete.cases(df),]
  # prepare Latlong coordinates matrix
  longlatcoords <- SpatialPoints(values, proj4string= CRS("+proj=longlat"))
  # convert to UTM
  utm_coords <- spTransform(longlatcoords, CRS("+proj=utm +zone=17"))
  # convert back to data frame
  utm <- as.data.frame(utm_coords)
  # replace columns in original data frame with new coordinates
  dat[value.rows,xcolumn] <- utm[,xcolumn]
  dat[value.rows,ycolumn] <- utm[,ycolumn]
  
  return(dat)
}


# =================================================================
# Use:

# read data file in need of conversion

dat = read.csv('./Oyster_data/Quadrat/quadrat.locations.csv')

dat$Quad_Long = dat$Quad_Long * -1
dat$Center_Long = dat$Center_Long * -1
dat = convert_lonlat_utm(dat,'Quad_Long','Quad_Lat')
dat = convert_lonlat_utm(dat,'Center_Long','Center_Lat')


colnames(dat)[colnames(dat)=='Quad_Long'] <- 'Quad_E'
colnames(dat)[colnames(dat)=='Quad_Lat'] <- 'Quad_N'
colnames(dat)[colnames(dat)=='Center_Long'] <- 'Center_E'
colnames(dat)[colnames(dat)=='Center_Lat'] <- 'Center_N'

write.csv(dat,'./Oyster_data/Quadrat/quadrat.locations.csv',row.names=F)

