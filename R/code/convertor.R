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
