#
# 1) draw graph PAEMS delta and its 6month moving average.
# 2) length is fixed to 90 months from the current most updated data available month.
#
len_mon <- length(seq(as.Date("2012-01-01"),index(last(PA)),by='months'))
mov_mon <- 6
plot(last(seq(as.Date("2001-01-01"),index(last(PA)),by='months'),n=len_mon),last(na.omit(filter(diff(PA),rep(1,1))/1),n=len_mon),type='h',axes=T,ylim=c(0,400),lwd=4)
par(new=T)
plot(last(seq(as.Date("2001-01-01"),Sys.Date(),by='months'),n=len_mon),last(na.omit(filter(diff(PA),rep(1,mov_mon))/mov_mon),n=len_mon),type='l',col=2,axes=F,ylim=c(0,400),lwd=2)
abline(h=seq(80,340,20),lty=2,col=4)
abline(v=seq(as.Date("2013-02-01"),as.Date("2019-02-01"),by='years'),lty=2,col=4)
