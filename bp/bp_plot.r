#
# daily
#

start_date <- "2018-02-01::"
len <- length(index(bp.xts[start_date][bp.xts[start_date]$High > 95]))
plot(bp.xts[start_date][,c(1,2)][bp.xts[start_date]$High > 95],col = c("red", "blue"),lwd=c(3,3,2,2),
major.ticks='days',grid.ticks.on='days',type='p',ylim=c(60,160))
addSeries(merge(as.xts(rep(125,len),index(bp.xts[start_date][bp.xts[start_date]$High > 95])),rep(75,len),
     rep(mean(bp.xts[start_date][,1][bp.xts[start_date]$High > 95]),len),
     rep(mean(bp.xts[start_date][,2][bp.xts[start_date]$High > 95]),len)), ylim=c(60,160),on=1,col=4,lwd=1)
# events <- xts(c("weight","70k","75k","80k","85k"),as.Date(c("2018-07-14","2019-01-23","2019-02-14","2019-03-15","2019-4-16")))
events <- xts(c("weight","70k","75k","80k","85k","90k"),as.Date(c("2018-07-14","2019-01-23","2019-02-14","2019-03-15","2019-4-16","2019-06-07")))
addEventLines(events, srt=90, pos=2,col=10)
axis(4,at=c(seq(70,140,5)))

#
# moving average plus daily
#
len <- length(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7)))
plot(merge(as.xts(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7)),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),
as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,2],rep(1,7))/7)),
last(apply.daily(bp.bangkok,mean),n=len)),
suffixes=c("mh","ml","high","low"),main="daily w/ 7 day moving average",grid.ticks.on='weeks')
addSeries(merge(as.xts(rep(mean(bp.xts[,1][bp.xts$High > 95]),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),
  rep(mean(bp.xts[,2][bp.xts$High > 95]),len),
  rep(last(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7))),len),
  rep(last(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,2],rep(1,7))/7))),len)),on=1,col=10,lwd=1)
# events <- xts(c("natrix","weight","abort natrix","70k","75k","80k","85k"),as.Date(c("2018-06-20", "2018-07-14","2018-08-09","2019-01-23","2019-02-14","2019-03-15","2019-4-16")))
events <- xts(c("weight","70k","75k","80k","85k","90k"),as.Date(c("2018-07-14","2019-01-23","2019-02-14","2019-03-15","2019-4-16","2019-06-07")))
addEventLines(events, srt=90, pos=2,col=10)
axis(2,at=c(135,130,125,120,115,85,80,75,70))
axis(4,at=c(135,130,125,120,115,85,80,75,70))

# events <- xts(c("natrix","weight","abort natrix","70k","75k"),as.Date(c("2018-06-20", "2018-07-14","2018-08-09","2019-01-23","2019-02-14")))
# addEventLines(events, srt=90, pos=2,col=10)

#
# 2018 vs. 2019 comparison
#

bp.day <- apply.daily(bp.bangkok,mean)
d <- intersect(intersect(index(bp.day["2019"]),seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by='days'))-17896,intersect(index(bp.day["2018"]),seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days'))-(17896-365))

plot(merge(bp.day[seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by='days')[d]],as.vector(bp.day[,1][seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days')[d]]),as.vector(bp.day[,2][seq(as.Date("2018-01-01"),as.Date("2018-12-31"),by='days')[d]]),suffixes = c("","h18","h18")))
# addEventLines(events, srt=90, pos=2,col=10)
# adjust mix-max of ylim by min() and max()
#
# week average graph.
#
plot(apply.weekly(bp.day,mean),type='p',ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10))
#
# draw the horizontal line at last weeks's average.
#
# addSeries(as.xts(rep(125,length(apply.weekly(bp.day,mean)[,1])),index(apply.weekly(bp.day,mean))),on=1,ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10))
# addSeries(as.xts(rep(85,length(apply.weekly(bp.day,mean)[,1])),index(apply.weekly(bp.day,mean))),on=1,ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10))
addSeries(as.xts(rep(last(apply.weekly(bp.day,mean)[,1]),length(apply.weekly(bp.day,mean)[,1])),index(apply.weekly(bp.day,mean))),on=1,ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10),col=2)
addSeries(as.xts(rep(last(apply.weekly(bp.day,mean)[,2]),length(apply.weekly(bp.day,mean)[,1])),index(apply.weekly(bp.day,mean))),on=1,ylim=c( min(apply.weekly(bp.day,mean)[,2])-10,max(apply.weekly(bp.day,mean)[,1])+10),col=2)

events <- xts(c("weight","70k","75k","80k","85k","90k"),as.Date(c("2018-07-14","2019-01-23","2019-02-14","2019-03-15","2019-4-16","2019-06-07")))
addEventLines(events, srt=90, pos=2,col=10)
axis(2,at=c(135,130,125,120,115,85,80,75,70))
axis(4,at=c(135,130,125,120,115,85,80,75,70))
