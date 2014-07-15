Kvals <- as.matrix(read.table(file="data/Pacific-1980-1983.txt", header=TRUE))
ludescher.basin <- function() {
  lats <- c( 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6)
  lons <- c(11,12,13,14,15,16,17,18,19,20,21,22,16,22)
  stopifnot(length(lats) == length(lons))
  list(lats=lats,lons=lons)
}
source("code/replication.R")

nini <- read.table("nino3.4-anoms.txt", skip=1, header=TRUE)

S3 <- ludescher_replication(Kvals,ludescher.basin(),n=365,m=200,step=10,coarse=FALSE)

plot.S(S3,nini)
