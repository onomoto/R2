# read data as csv format and convert to xts to plot
#
Sys.setenv(TZ=Sys.timezone())
#
# Update below to check file.exists.
#
if(file.exists("~/Downloads/bp2018.csv")){
  bp2018 <- read.csv("~/Downloads/bp2018.csv")
  system("rm \"$HOME/Downloads/bp2018.csv\"")
}else{
  print("!!!FILE DOESN'T EXIST!!!!!")
}
#
# Update ends.
#
# bp2018.xts <- xts(bp2018[,c(-1,-2,-6)],as.POSIXct(paste(bp2018$Date,bp2018$Time,sep=" "),tz=Sys.timezone()),tz=Sys.timezone())
if(file.exists("~/Downloads/bp - シート1.csv")){
  bp2018.xts <- xts(bp2018[,c(3,4,5)],as.POSIXct(paste(bp2018$Date,bp2018$Time,sep=" "),tz=Sys.timezone()),tz=Sys.timezone())
  bp2019 <- read.csv("~/Downloads/bp - シート1.csv")
  system("rm \"$HOME/Downloads/bp - シート1.csv\"")
  bp2019.xts <- xts(bp2019[,c(3,4,5)],as.POSIXct(paste(bp2019$Date,bp2019$Time,sep=" "),tz=Sys.timezone()),tz=Sys.timezone())
  bp.xts <- append(bp2018.xts,bp2019.xts)
}else{
  print("!!! bp - sheet1.csv doesn't EXIST")
}
# weekly average
apply.weekly(bp.xts[bp.xts$High > 95],mean)
#
#
# prepare data according to system timezone. "Asia/Tokyo" in most cases.
#
bp.day <- merge(as.xts(as.vector(bp.xts[,1]),as.Date(index(bp.xts),tz=tzone(bp.xts))),as.vector(bp.xts[,2]))
colnames(bp.day)[1] <- "high"
colnames(bp.day)[2] <- "low"
#
# prepare timezone 2 hours behind "Asia/Tokyo".
#
bp.bangkok <- merge(as.xts(as.vector(bp.xts[,1]),as.Date(index(bp.xts),tz="Asia/Bangkok")),as.vector(bp.xts[,2]))
colnames(bp.bangkok)[1] <- "high"
colnames(bp.bangkok)[2] <- "low"
apply.weekly(bp.bangkok,mean)
#
# use anonymous function in line
#
mapply(function(x,y){return(mean(na.omit(bp.xts[strptime(format(index(bp.xts),"%H:%M:%S"),"%H:%M:%S") > strptime(x,"%H:%M:%S") & strptime(format(index(bp.xts),"%H:%M:%S"),"%H:%M:%S") < strptime(y,"%H:%M:%S")])
[,1]))},c("05:00:00","06:00:00","07:00:00","08:00:00","09:00:00","10:00:00","11:00:00","23:00:00","00:00:00"),c("05:00:00","06:59:00","07:59:00","08:59:00","09:59:00","10:59:00","11:59:00","23:59:00","00:59:00"))

bp.day <- merge(as.xts(as.vector(bp.xts[,1]),as.Date(index(bp.xts),tz=tzone(bp.xts))),as.vector(bp.xts[,2]))
colnames(bp.day)[1] <- "high"
colnames(bp.day)[2] <- "low"
apply.weekly(bp.day,mean)
apply.monthly(bp.bangkok,mean)
na.omit(diff(apply.monthly(bp.bangkok,mean),lag=12))

