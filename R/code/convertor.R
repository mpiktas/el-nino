index.from.latlon <- function(i, j) { (i-lat.range[1]) * n.lon + (j-lon.range[1]) + 1 }
lon.from.index <- function(idx) { (idx-1) %% n.lon  +  lon.range[1] }
lat.from.index <- function(idx) { floor((idx-1) / n.lon)  +  lat.range[1] }

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


make.Kvals.for.year <- function(year, tb, datadir="") { 
  onc <- open.nc(paste0(datadir,"air.sig995.", year, ".nc"))
  rnc <- read.nc(onc)
  close.nc(onc)
  yearof.Kvals <- matrix(0, nrow=365, ncol=n.points)
  for (lat in lat.range) {
    for (lon in lon.range) { 
      yearof.Kvals[, tb[lat, lon] ] <- rnc$air[ lon, lat, 1:365 ] # ignore leap days
    }
  }
  
  colnames(yearof.Kvals) <- point.names()
  rownames(yearof.Kvals) <- paste0("Y", year, "P", sprintf("%03d", 1:365))
  yearof.Kvals
}

read.Kvals <- function(lat.range,lon.range,firstyear,lastyear,prefix=NULL) {
    n.lat <- length(lat.range)
    n.lon <- length(lon.range)
    n.points <- n.lat * n.lon
    smallest.lat <- lat.range[1]
    smallest.lon <- lon.range[1]
    biggest.lat <- lat.range[n.lat]
    biggest.lon <- lon.range[n.lon]
    
    ## extract the data as a vector (not a matrix) at each time
    ## These functions do the translation


    index.table <- matrix(0, nrow=biggest.lat, ncol=biggest.lon)
    for (lat in lat.range) {
        for (lon in lon.range) {
            index.table[lat, lon] <- index.from.latlon(lat, lon)
        }
    }
    setof.Kvals <- NULL
    for (i in firstyear:lastyear) {
        setof.Kvals <- rbind(setof.Kvals, make.Kvals.for.year(i,index.table,datadir="data/"))
    }
    
    if(!is.null(prefix))write.table(x=round(setof.Kvals, digits=2), file=paste0(prefix,firstyear,"-",lastyear,".txt"), quote=FALSE)
    round(setof.Kvals,digits=2)
}
