
#############################################################
#############################################################

# This program processes surface air temperature files 
# downloaded from NOAA here:
#
#ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface/
#
# It lets you select the daily average temperatures 
# for a certain range of years and a certain rectangle in 
# the NOAA grid and convert this data into a format 
# suitable for computation, especially R.

# You can use this program by editing this section only.
# The defaults are those used to replicate this paper:
#
# Josef Ludescher, Avi Gozolchiani, Mikhail I. Bogachev, 
# Armin Bunde, Shlomo Havlin, and Hans Joachim Schellnhuber, 
# Very early warning of next El Niño, Proceedings of the 
# National Academy of Sciences, February 2014.  

# Choose your working directory for R here:

setwd("C:/Users/JOHN/Documents/My Backups/azimuth/el nino")

# Choose your latitude and longitude range:

lat.range <- 24:50
lon.range <- 48:116 

# Supply the first and last years:

firstyear <- 1948
lastyear <- 1980

# Supply the output name as a text string.  The default here
# is "Pacific-".  
# paste0() concatenates strings, which you may find handy:

outputfilename <- paste0("Pacific-", firstyear, "-", lastyear, ".txt")

#############################################################
#############################################################

#                  Explanation


# 1. Use setwd() to set the working directory to the one 
# containing the .nc files such as air.sig995.1951.nc.
# Example:
# setwd("C:/Users/JOHN/Documents/My Backups/azimuth/el nino")

# 2. Supply the latitude and longitude range.  The NOAA data is 
# on a 2.5 degree x 2.5 degree grid. The ranges are supplied 
# as the number of steps of 2.5 degrees, counting from 1. 
# For latitude, 1 means North Pole, 73 means South Pole. 
# For longitude, 1 means the prime meridian, 0 degrees East.
# 37 means 90 degrees E, 73 means 180 degrees E, 109 means 270 
# degrees E or 90 degrees W, and 144 means 2.5 degrees W. 

# These give the area used by Ludescher et al, 2013.  It is 27x69 
# grid points:
# lat.range <- 24:50
# lon.range <- 48:116 

# 3. Supply the years.  These are the years for Ludescher et al:
# firstyear <- 1950
# lastyear <- 1979

# 4. Supply the output name as a text string. 
# paste0() concatenates strings, which you may find handy:
# outputfilename <- paste0("Pacific-", firstyear, "-", lastyear, ".txt")

#############################################################
#############################################################

#                      Example of output

# S024E048 S024E049 S024E050 S024E051 S024E052 S024E053 [etc.]
# Y1950P001 277.85 279.8 281.95 282.77 283.7 285.57     [etc.]

# There is one row for each day, and 365 days in each year 
# (leap days are omitted). In each row, you have temperatures 
# in Kelvin for each grid point in a rectangle.

# S024E142 means 24 steps South from the North Pole and 
# 48 steps East from Greenwich, where no steps at all is 
# counted as step 1.  The points are in reading order, 
# starting at the top-left (Northmost, Westmost) and going 
# along the top row first.

# Y1950P001 means year 1950, day 1. (P because longer 
# periods might be used later.)

#############################################################
#############################################################

library(RNetCDF)


n.lat <- length(lat.range)
n.lon <- length(lon.range)
n.points <- n.lat * n.lon
smallest.lat <- lat.range[1]
smallest.lon <- lon.range[1]
biggest.lat <- lat.range[n.lat]
biggest.lon <- lon.range[n.lon]

# extract the data as a vector (not a matrix) at each time
# These functions do the translation

index.from.latlon <- function(i, j) { (i-lat.range[1]) * n.lon + (j-lon.range[1]) + 1 }
lon.from.index <- function(idx) { (idx-1) %% n.lon  +  lon.range[1] }
lat.from.index <- function(idx) { floor((idx-1) / n.lon)  +  lat.range[1] }

index.table <- matrix(0, nrow=biggest.lat, ncol=biggest.lon)
for (lat in lat.range) {
  for (lon in lon.range) {
    index.table[lat, lon] <- index.from.latlon(lat, lon)
  }
}

point.names <- function() {
  pointnames <- rep("", n.points)
  for (idx in 1:n.points) {
    lon <- lon.from.index(idx)
    lat <- lat.from.index(idx)
    pointnames[idx] <- paste0("S", sprintf("%03d", lat), "E", sprintf("%03d", lon))
    stopifnot(idx == index.from.latlon(lat, lon))
  }
  pointnames
}


make.Kvals.for.year <- function(year) { 
  onc <- open.nc(paste0("air.sig995.", year, ".nc"))
  rnc <- read.nc(onc)
  close.nc(onc)
  yearof.Kvals <- matrix(0, nrow=365, ncol=n.points)
  for (lat in lat.range) {
    for (lon in lon.range) { 
      yearof.Kvals[, index.table[lat, lon] ] <- rnc$air[ lon, lat, 1:365 ] # ignore leap days
    }
  }
  
  colnames(yearof.Kvals) <- point.names()
  rownames(yearof.Kvals) <- paste0("Y", year, "P", sprintf("%03d", 1:365))
  yearof.Kvals
}


setof.Kvals <- NULL
for (i in firstyear:lastyear) {
  setof.Kvals <- rbind(setof.Kvals, make.Kvals.for.year(i))
}

write.table(x=round(setof.Kvals, digits=2), file=outputfilename, quote=FALSE)

#############################################################
#############################################################
