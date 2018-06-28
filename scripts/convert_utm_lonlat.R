# This script is to convert UTM coordinates to decimal degrees
#
# Some Oyster Transect data files recorded locations in UTM, our convention is degrees
# Cedar Key area: UTM zone 17
# started by EMC 3/2/18


library(rgdal)

#' @title convert UTM to Lon/Lat@template 
#' 
#' @description this function converts UTM coordinates to decimal degrees.
#' WARNING: only works for UTM zone 17 (Cedar Key area)
#' 
#' @param dat data frame to be changed
#' @param xcolumn name of column containing Easting coordinate
#' @param ycolumn name of column containing Northing coordinate
#' 
#' @return data frame with xcolumn and ycolumn converted to decimal degrees
#' 
convert_utm_lonlat = function(dat,xcolumn,ycolumn) {
  df = dat[,c(xcolumn,ycolumn)]
  # get rows of non-NA values
  value.rows = which(complete.cases(df))
  # remove NAs from data to convert
  values = df[complete.cases(df),]
  # prepare UTM coordinates matrix
  utm_coords <- SpatialPoints(values, proj4string=CRS("+proj=utm +zone=17"))
  # convert to Lon/Lat
  longlatcoords <- spTransform(utm_coords, CRS("+proj=longlat"))
  # convert back to data frame
  lonlat <- as.data.frame(longlatcoords)
  # replace columns in original data frame with new coordinates
  dat[value.rows,xcolumn] <- lonlat[,xcolumn]
  dat[value.rows,ycolumn] <- lonlat[,ycolumn]
  
  return(dat)
}


# =================================================================
# Use:

# read data file in need of conversion
dat = read.csv('./Oyster_data/Transect/transect.locations.csv')

# the 2018 data is already in lon/lat: take this off for now
dat2018 = dat[dat$Date=='01/30/2018',]
dat = head(dat,-1)

dat = convert_utm_lonlat(dat,'StartGPS_E','StartGPS_N')
dat = convert_utm_lonlat(dat,'MidGPS_E','MidGPS_N')
dat = convert_utm_lonlat(dat,'EndGPS_E','EndGPS_N')

dat = rbind(dat,dat2018)


# change column names
colnames(dat)[colnames(dat)=='StartGPS_E'] <- 'Start_Long'
colnames(dat)[colnames(dat)=='StartGPS_N'] <- 'Start_Lat'
colnames(dat)[colnames(dat)=='MidGPS_E'] <- 'Mid_Long'
colnames(dat)[colnames(dat)=='MidGPS_N'] <- 'Mid_Lat'
colnames(dat)[colnames(dat)=='EndGPS_E'] <- 'End_Long'
colnames(dat)[colnames(dat)=='EndGPS_N'] <- 'End_Lat'


write.csv(dat,'./Oyster_data/Transect/transect.locations.csv',row.names=F)
