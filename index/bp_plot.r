# plot(bp.xts[,c(1,2)][bp.xts$High > 95],col = c("red", "blue"),lwd=c(3,3,2,2),major.ticks='days',grid.ticks.on='days',type='p',ylim=c(60,160))
# # draw a horizontal line at 130 as the benchmark
# addSeries(as.xts(rep(130,length(index(bp.xts[bp.xts$High > 95]))),index(bp.xts[bp.xts$High > 95])),ylim=c(60,160),on=1,col=5,lwd=1)
# # draw another line at 85.
# # addSeries(as.xts(rep(85,length(index(bp.xts[bp.xts$High > 95]))),index(bp.xts[bp.xts$High > 95])),ylim=c(60,160),on=1,col=5,lwd=1)
# l <- length(index(bp.xts[bp.xts$High > 95]))
# addSeries(merge(as.xts(rep(125,l),index(bp.xts[bp.xts$High > 95]),rep(75,l)),
# rep(mean(bp.xts[,1][bp.xts$High > 95]),l)),ylim=c(60,160),on=1,col=5,lwd=1)


len <- length(index(bp.xts[bp.xts$High > 95]))
plot(bp.xts[,c(1,2)][bp.xts$High > 95],col = c("red", "blue"),lwd=c(3,3,2,2),
major.ticks='days',grid.ticks.on='days',type='p',ylim=c(60,160))
addSeries(merge(as.xts(rep(125,l),index(bp.xts[bp.xts$High > 95])),rep(75,l),
     rep(mean(bp.xts[,1][bp.xts$High > 95]),len),
     rep(mean(bp.xts[,2][bp.xts$High > 95]),len)), ylim=c(60,160),on=1,col=4,lwd=1)
events <- xts(c("natrix","weight","abort natrix","70k","75k","80k"),as.Date(c("2018-06-20", "2018-07-14","2018-08-09","2019-01-23","2019-02-14","2019-03-15")))
addEventLines(events, srt=90, pos=2,col=10)


len <- length(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7)))
plot(merge(as.xts(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7)),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),
as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,2],rep(1,7))/7)),
last(apply.daily(bp.bangkok,mean),n=len)),
suffixes=c("mh","ml","high","low"),main="daily w/ 7 day moving average",grid.ticks.on='weeks')
addSeries(merge(as.xts(rep(mean(bp.xts[,1][bp.xts$High > 95]),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),
  rep(mean(bp.xts[,2][bp.xts$High > 95]),len),
  rep(last(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7))),len),
  rep(last(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,2],rep(1,7))/7))),len)),on=1,col=10,lwd=1)
events <- xts(c("natrix","weight","abort natrix","70k","75k","80k"),as.Date(c("2018-06-20", "2018-07-14","2018-08-09","2019-01-23","2019-02-14","2019-03-15")))
addEventLines(events, srt=90, pos=2,col=10)




# addSeries(as.xts(rep(mean(bp.xts[,1][bp.xts$High > 95]),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),on=1,col=6,lwd=1)
#
# addSeries(as.xts(rep(mean(bp.xts[,2][bp.xts$High > 95]),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),on=1,col=6,lwd=1)
#
# addSeries(as.xts(rep(last(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,2],rep(1,7))/7))),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),on=1,col=6,lwd=1)
#
# addSeries(as.xts(rep(last(as.vector(na.omit(filter(apply.daily(bp.bangkok,mean)[,1],rep(1,7))/7))),len),last(index(apply.daily(bp.bangkok,mean)[,2]),len)),on=1,col=6,lwd=1)
#
# events <- xts(c("natrix","weight","abort natrix","70k","75k"),as.Date(c("2018-06-20", "2018-07-14","2018-08-09","2019-01-23","2019-02-14")))
# addEventLines(events, srt=90, pos=2,col=10)
