##This file is generated in puzzle3.R
Kvals <- as.matrix(read.table(file="data/Pacific-1980-1983.txt", header=TRUE))

ludescher.basin.baez <- function() {
  lats <- c( 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
  lons <- c(11,12,13,14,15,16,17,18,19,20,21,22)
  stopifnot(length(lats) == length(lons))
  list(lats=lats,lons=lons)
}

ludescher.basin.original <- function() {
  lats <- c( 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6)
  lons <- c(11,12,13,14,15,16,17,18,19,20,21,22,16,22)
  stopifnot(length(lats) == length(lons))
  list(lats=lats,lons=lons)
}

source("code/replication.R")

S1 <- ludescher_replication(Kvals,ludescher.basin.baez(),n=365,m=200,step=10)
S2 <- ludescher_replication(Kvals,ludescher.basin.original(),n=365,m=200,step=10)

nini <- read.table("nino3.4-anoms.txt", skip=1, header=TRUE)
plot.S(S1,nini)
plot.S(S2,nini)

plot.cmp.S(S1,S2,nini)
