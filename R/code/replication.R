#########################################################
############ For reading in data

lat.from.label <- function(label) {
  as.integer(substr(label,2,4))
}

lon.from.label <- function(label) {
  as.integer(substr(label,6,8))
}

lat.range.from.colnames <- function(cnames) {
  lat.from.label(cnames[1]) : lat.from.label(cnames[length(cnames)])
}

lon.range.from.colnames <- function(cnames) {
  lon.from.label(cnames[1]) : lon.from.label(cnames[length(cnames)])
}

year.from.label <- function(label) {
  as.integer(substr(label,2,5))
}


year.range.from.rownames <- function(rnames) {
  year.from.label(rnames[1]) : year.from.label(rnames[length(rnames)])
}


#########################################################
########## Conversion, seasonal adjustment, subsampling

# Converts from a vector per day to a 2D array per day

data.to.3D <- function(vals) {
  lat.range <- lat.range.from.colnames(colnames(vals))
  lon.range <- lon.range.from.colnames(colnames(vals))
  n.times <- dim(vals)[1]
  n.lat <- length(lat.range)
  n.lon <- length(lon.range)
  vals3D <- array(0, dim=c(n.times, n.lat, n.lon))
  for (tim in 1:n.times) {
    for (lat in 1:n.lat) {
      vals3D[tim, lat, ] <- vals[tim, ((lat-1) * n.lon) +  (1:n.lon)]
    }
  }
  vals3D
}

# subtracts the climatological seasonal cycle (mean over years 
# for each grid point, each day-in-year)

seasonally.adjust <- function(vals) {
  stopifnot(dim(vals)[1] %% 365 == 0)
  n.years <- as.integer(dim(vals)[1]/365)
  offsets <- (0:(n.years-1))*365
  y.means <- array(0, dim=c(365, dim(vals)[2], ncols=dim(vals)[3]))
  for (d in 1:365) {
    for (lat in 1:dim(vals)[2]) {
      for (lon in 1:dim(vals)[3]) {
        y.means[d, lat, lon] <- mean(vals[d + offsets, lat, lon])
      }                   
    }
  }  
  for (y in 1:n.years ) {
    for (d in 1:365) {
      tim <- (y-1)*365 + d
      vals[tim, , ] <- vals[tim, , ] - y.means[d, , ]
    }
  }
  vals
}

# the data per day is reduced from e.g. 27x69 to 9x23. 

subsample.3x3 <- function(vals) {
  stopifnot(dim(vals)[2] %% 3 == 0)
  stopifnot(dim(vals)[3] %% 3 == 0)
  n.sslats <- dim(vals)[2]/3
  n.sslons <- dim(vals)[3]/3
  ssvals <- array(0, dim=c(dim(vals)[1], n.sslats, n.sslons))  
  for (d in 1:dim(vals)[1]) {
    for (slat in 1:n.sslats) {
      for (slon in 1:n.sslons) {
        ssvals[d, slat, slon] <- mean(vals[d, (3*slat-2):(3*slat), (3*slon-2):(3*slon)])
      }
    }
  }
  ssvals
}


#########################################################
############ Making the time-delayed cross correlations - ############ arbitrary points and days

# input: 3D array vals of temperatures vals for s days, 
# a running mean length n (=365)

# output: 3D array of the same dimensions as vals 
#containing running means

make.runningmeans <- function(vals,n) {
  rmeans <- array(0, dim=dim(vals))
  for (lat in 1:dim(vals)[2]) {
    for (lon in 1:dim(vals)[3]) { 
      rmeans[, lat, lon] <- as.vector(filter(vals[, lat, lon], rep(1/n,n), sides=1))
    }
  }
  rmeans
}




convolve.filter.nextpow2and3 <- function(delayed, current) {
  nm <- length(delayed)
  n <- length(current)
  m <- nm - n
  v <- nm+n-1
  N <- nextn(v, c(2,3))
  d <- N-v
  convolve(c(delayed,rep(0,d)), current, type="filter")[1:(m+1)]
}




# input: two vectors x and y of length s, corresponding running means
# of period n, two numbers n and m with
# n + m < s, and a day d in (n+m):s. 

# output: an array convs of length (2*m+1) columns containing 
# covariances between certain subsets of x and y of length n.
# convs[m+1-j] = cov( x[(d-j-n+1):(d-j)],  y[(d-n+1):d] )
# convs[m+1+j] = cov( x[(d-n+1):d],  y[(d-j-n+1):(d-j)] )
# where j is in 0:m.
convolve.covs <- function(x, y, xrm, yrm, n, m, d) {
  s <- length(x)
  stopifnot(s == length(y))
  stopifnot(n + m <= s)
  
  convs <- rep(0, 2*m+1)
  
  yn <- y[(d-n+1):d] - yrm[d]
  xnm <-  x[(d-n-m+1):d]    
  convs[1:(m+1)] <- convolve.filter.nextpow2and3(xnm, yn)   
  
  xn <- x[(d-n+1):d] - xrm[d]
  ynm <-  y[(d-n-m+1):d]
  convs[(2*m+1):(m+1)] <- convolve.filter.nextpow2and3(ynm, xn)   

  convs <- convs/n
}


# input: 3D array of temperatures vals for s days, 
# a convolution-length n (eg 365)
# a maximum time-delay m (eg 200), and a day d in (n+m):s. 
# output: 3D array of time-delayed standard deviations for day d. 
# The first dimension is the time delay -m:m, second is lat, 
# third lon

make.standarddevs <- function(vals, n, m, d) {
  sds <- array(0, dim=c(2*m+1, dim(vals)[2], dim(vals)[3]))
  for (lat in 1:dim(vals)[2]) {
    for (lon in 1:dim(vals)[3]) {
      recentx <- vals[(d-n-m+1):d , lat, lon]
      xrm <- as.vector(filter(recentx, rep(1/n,n), sides=1))
      x2rm <- as.vector(filter(recentx^2, rep(1/n,n), sides=1))      
      fsds <- sqrt(x2rm - xrm^2)*sqrt(n/(n-1))
      fsds <- fsds[n:(n+m)]
      sds[1:(m+1) ,lat, lon] <- fsds
      sds[(m+1):(2*m+1) ,lat, lon] <- rev(fsds)
    }
  }
  sds
}



# input: 3D array of temperatures vals for s days, 
# corresponding array
# rmeans of running means of length n, an array 
# sds from make.standarddevs()
# for the same day d, a grid point (xlat, xlon),
# a convolution-length n (eg 365)
# a maximum time-delay m (eg 200), and a day d in (n+m):s. 
# output: 3D array of time-delayed correlations for point 
#(xlat, xlon) for day d. 
# The first dimension is the time delay -m:m, 
# second is lat, third lon

make.cors <- function(vals, rmeans, sds, xlat, xlon, n, m, d) {
  cors <- array(0, dim=c(2*m+1, dim(vals)[2], dim(vals)[3]))
  x <- vals[ , xlat, xlon]
  xrm <- rmeans[, xlat, xlon]           
  for (ylat in 1:dim(vals)[2]) {#
    for (ylon in 1:dim(vals)[3]) {
      y <- vals[ , ylat, ylon]
      yrm <- rmeans[ , ylat, ylon]
      covs <- convolve.covs(x, y, xrm, yrm, n, m, d)
      cors[ , ylat, ylon] <- covs / (sds[ , ylat, ylon] * sds[ , xlat, xlon])      
    }
  }
  cors
}

# returns TRUE if (lat,lon) is in 'focus'
in.focus <- function(fa, lat, lon) {
  w <- which(fa$lats==lat)  
  (length(which(fa$lons[w]==lon)) > 0)
}

# for each point (lat,lon) in grid, calculates an average link strength between
#  (lat,lon) and the points in Ludescher et al's "El Nino basin" 
signalstrength <- function(linktype, focus, vals, rmeans, sds, n, m, d) {
  S <- 0
  nba <- length(focus$lats)
  nlats <- dim(vals)[2]
  nlons <- dim(vals)[3]
  for (b in 1:nba) {
    latb <- focus$lats[b]
    lonb <- focus$lons[b]
    if (linktype == "abs.correlations") {
      links <- abs(make.cors(vals, rmeans, sds, latb, lonb, n, m, d))
    } else if (linktype == "signed.correlations") {
      links <- make.cors(vals, rmeans, latb, lonb, n, m, d)
    } else if (linktype == "abs.covariances") {
      links <- abs(make.covs(vals, rmeans, latb, lonb, n, m, d))
    } else if (linktype == "signed.covariances") {
      links <- make.covs(vals, rmeans, latb, lonb, n, m, d)
    } else {
      stop()
    }
    
    for (lat in 1:nlats) {
      for (lon in 1:nlons) {
        if (!in.focus(focus, lat, lon)) {
          tdccs <- links[ , lat, lon]
          sig <- (max(tdccs) - mean(tdccs)) / sd(tdccs)
          S <- S + sig
        }
      }
    } 
  }
  S / nba / (nlats*nlons - nba)
}



#########################################################
############# For plotting Nino index

plot.nino.3.4.background.rectangle <- function(mint, maxt, col) {
  polygon(x=c(1,13,13,1,1), y=c(maxt,maxt,mint,mint,maxt), border = NA, col=col)
}


find.nino.plotting.info <- function(firstyear, lastyear, miny, maxy, nini) {  
  nini <- as.matrix(nini)
  w <- which((nini[,"YR"] >= firstyear) & (nini[,"YR"] <= lastyear))
  stopifnot((length(w) %% 12) == 0)
  yrnini <- nini[w,"ANOM"]
  offset <- min(yrnini) 
  scaling <- (maxy - miny) / (max(yrnini) - min(yrnini))
  yrnini <- miny + scaling * (yrnini - offset)
  zp5 <- miny + scaling * (0.5 - offset)
  labels <- c("-2", "-1", "0", "+1", "+2")
  ticks <- miny + scaling * (c(-2,-1,0,1,2) - offset)
  time.axis <- firstyear + (0:(length(w)-1))/12
  list(time.axis=time.axis, yrnini=yrnini, zp5=zp5, ticks=ticks, labels=labels,
       firstyear=firstyear, lastyear=lastyear, miny=miny, maxy=maxy)
}

plot.nino.zp5.rect <- function(plotinfo, col) {
  time.axis <- plotinfo$time.axis
  minx <- time.axis[1]
  maxx <- time.axis[length(time.axis)]
  miny <- plotinfo$miny
  zp5 <- plotinfo$zp5
  polygon(x=c(minx,maxx,maxx,minx,minx), y=c(zp5,zp5,miny,miny,zp5), border = NA, col=col)
  
}

plot.nino.3.4 <- function(plotinfo, col) {
  lines(plotinfo$time.axis, plotinfo$yrnini, col=col)
}

ludescher_replication <- function(Kvals,basin,n,m,step,coarse=TRUE) {
    years <- as.numeric(unique(substr(rownames(Kvals),2,5)))
    Kvals.cnames <- colnames(Kvals)

    Kvals.3D <- data.to.3D(Kvals)
    
    SAvals.3D <- seasonally.adjust(Kvals.3D)

    if(coarse)SAvals.3D.3x3 <- subsample.3x3(SAvals.3D)
    else SAvals.3D.3x3 <- SAvals.3D
        
    w <- seq(from = 2*365, to = dim(SAvals.3D.3x3)[1], by=step)

    rmeans <- make.runningmeans(SAvals.3D.3x3,n)

    S <- rep(0, length(w))
    for (i in 1:length(w)) {
        d <- w[i]
        sds <- make.standarddevs(SAvals.3D.3x3, n, m, d)  
        S[i] <- signalstrength("abs.correlations", basin, SAvals.3D.3x3, rmeans, sds, n, m, d)
        cat("done day", d, "S(d)=", S[i], "\n")  
    }
    list(S=S,n=n,m=m,step=10,firstyear=min(years),lastyear=max(years))
}

plot.S <- function(S,nini) {
    time.axis <- S$firstyear+2+(0:(length(S$S)-1)) * S$step / 365
    par(mar=c(5, 4, 4, 5))
    plot(time.axis, S$S, type='n', xlab="Years", ylab="Signal strength S", 
         main=expression(paste("S and ", theta, " in red. Niño 3.4 in blue, below 0.5°C shaded")))

    ninoplotinfo <- find.nino.plotting.info(S$firstyear, S$lastyear, min(S$S), max(S$S),nini)
    plot.nino.zp5.rect(ninoplotinfo, "#eeeeffff")
    for (yr in (S$firstyear+2):(S$lastyear+1)) {
        lines(c(yr,yr), c(min(S$S),max(S$S)), col="grey80")
    }
    lines(time.axis, S$S, col="red")
    plot.nino.3.4(ninoplotinfo, "blue")
    lines(c(S$firstyear,(S$lastyear+1)), rep(2.82,2), col="red")
    axis(side=4, at=ninoplotinfo$ticks, labels=ninoplotinfo$labels)
    mtext(text="NINO 3.4 index", side = 4, line = 3)    
}

plot.cmp.S <- function(S1,S2,nini) {
    time.axis <- S1$firstyear+2+(0:(length(S1$S)-1)) * S1$step / 365
    par(mar=c(5, 4, 4, 5))
    plot(time.axis, S1$S, type='n', xlab="Years", ylab="Signal strength S", 
         main=expression(paste("S1 and ", theta, " in red. Niño 3.4 in blue, below 0.5°C shaded")))

    ninoplotinfo <- find.nino.plotting.info(S1$firstyear, S1$lastyear, min(S1$S), max(S1$S),nini)
    plot.nino.zp5.rect(ninoplotinfo, "#eeeeffff")
    for (yr in (S1$firstyear+2):(S1$lastyear+1)) {
        lines(c(yr,yr), c(min(S1$S),max(S1$S)), col="grey80")
    }
    lines(time.axis, S1$S, col="red")
    lines(time.axis, S2$S, col="green")
    plot.nino.3.4(ninoplotinfo, "blue")
    lines(c(S1$firstyear,(S1$lastyear+1)), rep(2.82,2), col="red")
    axis(side=4, at=ninoplotinfo$ticks, labels=ninoplotinfo$labels)
    mtext(text="NINO 3.4 index", side = 4, line = 3)    
}
