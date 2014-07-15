

#########################################################
#########################################################

# This program needs files
# Pacific-1948-1980.txt 
# and
# nino34-anoms.txt
#
# It computes the "average link strength" as a function of time,
# and plots it along with the Niño 3.4 index, allowing you to 
# replicate this paper:
#
# Josef Ludescher, Avi Gozolchiani, Mikhail I. Bogachev, 
# Armin Bunde, Shlomo Havlin, and Hans Joachim Schellnhuber, 
# Very early warning of next El Niño, Proceedings of the 
# National Academy of Sciences, February 2014.  

options(warn=2)

source("code/replication.R")
#########################################################
############ Making the link strengths - basin vs rest

ludescher.basin <- function() {
  lats <- c( 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6)
  lons <- c(11,12,13,14,15,16,17,18,19,20,21,22,16,22)
  stopifnot(length(lats) == length(lons))
  list(lats=lats,lons=lons)
}

#########################################################
#################### Main analysis

Kvals <- as.matrix(read.table(file="Pacific-1948-1979.txt", header=TRUE))
Kvals.cnames <- colnames(Kvals)

Kvals.3D <- data.to.3D(Kvals)

SAvals.3D <- seasonally.adjust(Kvals.3D)

SAvals.3D.3x3 <- subsample.3x3(SAvals.3D)

firstyear <- 1950
lastyear <- 1979
n <- 365
m <- 200
step <- 10
w <- seq(from = (firstyear-1948)*365, to=dim(SAvals.3D.3x3)[1], by=step)

rmeans <- make.runningmeans(SAvals.3D.3x3)

S <- rep(0, length(w))
for (i in 1:length(w)) {
  d <- w[i]
  sds <- make.standarddevs(SAvals.3D.3x3, n, m, d)  
  S[i] <- signalstrength("abs.correlations", ludescher.basin(), SAvals.3D.3x3, rmeans, sds, n, m, d)
  cat("done day", d, "S(d)=", S[i], "\n")  
}


#########################################################
############## Plotting results

time.axis <- firstyear+(0:(length(S)-1)) * step / 365
par(mar=c(5, 4, 4, 5))
plot(time.axis, S, type='n', xlab="Years", ylab="Signal strength S", 
     main=expression(paste("S and ", theta, " in red. Niño 3.4 in blue, below 0.5°C shaded")))
ninoplotinfo <- find.nino.plotting.info(firstyear, lastyear, min(S), max(S))
plot.nino.zp5.rect(ninoplotinfo, "#eeeeffff")
for (yr in firstyear:(lastyear+1)) {
  lines(c(yr,yr), c(min(S),max(S)), col="grey80")
}
lines(time.axis, S, col="red")
plot.nino.3.4(ninoplotinfo, "blue")
lines(c(firstyear,(lastyear+1)), rep(2.82,2), col="red")
axis(side=4, at=ninoplotinfo$ticks, labels=ninoplotinfo$labels)
mtext(text="NINO 3.4 index", side = 4, line = 3)

#########################################################
#########################################################


