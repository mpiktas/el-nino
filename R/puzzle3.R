##You need to run this line only once! 
##source("download_data.R")

library(RNetCDF)
source("code/convertor.R")

outputprefix <- "data/Pacific-"

#We save output to the file, but it is not strictly necessary.
Kvals <- read.Kvals(lat.range=24:50,lon.range=48:116,firstyear=1980,lastyear=1983,outputprefix)

ludescher.basin <- function() {
  lats <- c( 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6)
  lons <- c(11,12,13,14,15,16,17,18,19,20,21,22,16,22)
  stopifnot(length(lats) == length(lons))
  list(lats=lats,lons=lons)
}
source("code/replication.R")

nini <- read.table("nino3.4-anoms.txt", skip=1, header=TRUE)

S <- ludescher_replication(Kvals,ludescher.basin(),n=365,m=200,step=10)

