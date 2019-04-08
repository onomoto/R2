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
# the graph w/ 7 day moving average
#
# len <- length(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7)))
# plot(merge(as.xts(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7)),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),
# as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,2],rep(1,7))/7)),last(apply.daily(bp.bangkok,mean),n=len)),
# suffixes=c("mh","ml","high","low"),main="daily w/ 7 day moving average",grid.ticks.on='weeks')
# addSeries(as.xts(rep(mean(bp.xts[,1][bp.xts$High > 95]),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),on=1,col=6,lwd=1)
# addSeries(as.xts(rep(mean(bp.xts[,2][bp.xts$High > 95]),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),on=1,col=6,lwd=1)
# addSeries(as.xts(rep(last(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,2],rep(1,7))/7))),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),on=1,col=6,lwd=1)
# addSeries(as.xts(rep(last(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7))),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),on=1,col=6,lwd=1)
# axis(2,at=c(135,130,125,120,115,85,80,75,70))
# axis(4,at=c(135,130,125,120,115,85,80,75,70))
# events <- xts(c("natrix","weight","abort natrix","70k","75k"),as.Date(c("2018-06-20", "2018-07-14","2018-08-09","2019-01-23","2019-02-14")))
# addEventLines(events, srt=90, pos=2,col=10)

# bp.day <- apply.daily(bp.bangkok,mean)
# d <- intersect(intersect(index(bp.day["2019"]),seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by='days'))-17896,intersect(index(bp.day["2018"]),seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days'))-(17896-365))
# #
# # bp.day[seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by='days')[d]]
# # bp.day[seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days')[d]]
# #
# merge(bp.day[seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by='days')[d]],as.vector(bp.day[,1][seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days')[d]]),as.vector(bp.day[,2][seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days')[d]]),suffixes = c("","h18","l18"))
#
# plot(merge(bp.day[seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by='days')[d]],as.vector(bp.day[,1][seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days')[d]]),as.vector(bp.day[,2][seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days')[d]]),suffixes = c("","h18","l18")))
# axis(2,at=c(135,130,125,120,115,85,80,75,70))
# axis(4,at=c(135,130,125,120,115,85,80,75,70))
#
# use anonymous function in line
#
mapply(function(x,y){return(mean(na.omit(bp.xts[strptime(format(index(bp.xts),"%H:%M:%S"),"%H:%M:%S") > strptime(x,"%H:%M:%S") & strptime(format(index(bp.xts),"%H:%M:%S"),"%H:%M:%S") < strptime(y,"%H:%M:%S")])
[,1]))},c("05:00:00","06:00:00","07:00:00","08:00:00","09:00:00","10:00:00","11:00:00","23:00:00","00:00:00"),c("05:00:00","06:59:00","07:59:00","08:59:00","09:59:00","10:59:00","11:59:00","23:59:00","00:59:00"))

bp.day <- merge(as.xts(as.vector(bp.xts[,1]),as.Date(index(bp.xts),tz=tzone(bp.xts))),as.vector(bp.xts[,2]))
colnames(bp.day)[1] <- "high"
colnames(bp.day)[2] <- "low"
apply.weekly(bp.day,mean)
# plot(apply.weekly(bp.day,mean),type='p')
#
# adjust mix-max of ylim by min() and max()
#
plot(apply.weekly(bp.day,mean),type='p',ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10))
#
# draw the horizontal line at 125
#
# addSeries(as.xts(rep(125,length(apply.weekly(bp.day,mean)[,1])),index(apply.weekly(bp.day,mean))),on=1,ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10))
# addSeries(as.xts(rep(85,length(apply.weekly(bp.day,mean)[,1])),index(apply.weekly(bp.day,mean))),on=1,ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10))
addSeries(as.xts(rep(mean(bp.xts[,2]),length(apply.weekly(bp.day,mean)[,1])),index(apply.weekly(bp.day,mean))),on=1,ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10),col=2)
addSeries(as.xts(rep(mean(bp.xts[,1]),length(apply.weekly(bp.day,mean)[,1])),index(apply.weekly(bp.day,mean))),on=1,ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10),col=2)
axis(2,at=c(135,130,125,120,115,85,80,75,70))
axis(4,at=c(135,130,125,120,115,85,80,75,70))

#
# graph daily
#
# plot(bp.xts[,c(1,2)][bp.xts$High > 95],col = c("red", "blue"),lwd=c(3,3,2,2),major.ticks='days',grid.ticks.on='days',type='p',ylim=c(60,160))
# # draw a horizontal line at 130 as the benchmark
# addSeries(xts(rep(130,length(index(bp.xts[bp.xts$High > 95]))),index(bp.xts[bp.xts$High > 95])),ylim=c(60,160),on=1,col=5,lwd=1)
# # draw another line at 85.
# addSeries(xts(rep(85,length(index(bp.xts[bp.xts$High > 95]))),index(bp.xts[bp.xts$High > 95])),ylim=c(60,160),on=1,col=5,lwd=1)
# # draw the horizontal line at the average
# addSeries(xts(rep(mean(bp.xts[,1][bp.xts$High > 95]),length(index(bp.xts[bp.xts$High > 95]))),index(bp.xts[bp.xts$High > 95])),ylim=c(60,160),on=1,col=6,lwd=1)
# addSeries(xts(rep(mean(bp.xts[,2][bp.xts$High > 95]),length(index(bp.xts[bp.xts$High > 95]))),index(bp.xts[bp.xts$High > 95])),ylim=c(60,160),on=1,col=6,lwd=1)
# events <- xts(c("natrix","weight","abort natrix","70k","75k","80k"),as.Date(c("2018-06-20", "2018-07-14","2018-08-09","2019-01-23","2019-02-14","2019-03-15")))
# addEventLines(events, srt=90, pos=2,col=10)
# axis(2,at=c(135,130,125,120,115,85,80,75,70))
# axis(4,at=c(135,130,125,120,115,85,80,75,70))
