dt <- read.table("data/Riverside-1948-1979.txt",skip=1,header=FALSE)
colnames(dt) <- c("date","temp")

library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)

get_date <- function(xy) {
    year <- substr(xy[,1],2,5)
    day <- as.numeric(xy[,2])
    start <- as.Date(paste0(year,"-01-01"))    
    start+days(day-1)
}

dt$nice_date <- strsplit(dt$date %>% as.character,split="P") %>% do.call("rbind",.) %>% get_date

##Plot all years
qplot(y=temp,x=nice_date,data=dt,geom="line")

##Plot year 1963
 qplot(y=temp,x=nice_date,data=dt %>% filter(year(nice_date)==1963),geom="line")

##Output the numbers
write(dt$temp,file="numbers.txt",ncolumns=1)

##Output to the csv file for easier import in Excel
write.csv(dt %>% select(nice_date,temp),file="data.csv",row.names=FALSE)

