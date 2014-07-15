
##You need to run this line only once! 
##source("download_data.R")

lat.range <- 24:50
lon.range <- 48:116 

# Supply the first and last years:
firstyear <- 1980
lastyear <- 1985

# Supply the output name as a text string.  The default here
# is "Pacific-".  
# paste0() concatenates strings, which you may find handy:

outputfilename <- paste0("data/Pacific-", firstyear, "-", lastyear, ".txt")

source("netcdf-convertor-ludescher.R")

Kvals <- as.matrix(read.table(file=outputfilename, header=TRUE))

source("ludescher-replication.R")

