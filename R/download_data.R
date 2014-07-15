##Download all the data for the years 1948 through 2014 and put them in a folder data

for (year in 1948:2014) {
download.file(url=paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface/air.sig995.", year, ".nc"), destfile=paste0("data/air.sig995.", year, ".nc"), mode="wb")
}
