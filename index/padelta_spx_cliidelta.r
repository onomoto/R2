 # draw the graph to overlay PA 1 month delta, SPX, oecd cli and its 6 month delta.
 #  https://00819.blogspot.com/2019/02/plot-abline-eps-gspc.html for the reference
 #

plot.default(diff(PA["1992::2018"])[-1],type='h',axes=F,col=3)
par(new=T)
mnt <- seq(as.Date("1992-02-01"),as.Date("2018-12-31"),by='months')
plot.default(mnt,to.monthly(SP5["1992-02::2018"])[,4],type='l')
abline(v=as.Date("2018-01-01"),col = "gray60",lty=3)
abline(v=as.Date("2015-01-01"),col = "gray60",lty=3)
abline(v=as.Date("2017-01-01"),col = "gray60",lty=3)
abline(v=as.Date("2016-01-01"),col = "gray60",lty=3)
abline(h=2000,col="gray60",lty=3)
abline(v=as.Date("2014-01-01"),col = "gray60",lty=3)
par(new=T)
plot.default(mnt,as.vector(cli_xts$oecd["1992-02::2018"]),axes=F,col=4,type='l')
par(new=T)
plot.default(mnt,na.trim(diff(cli_xts$oecd["1991-08::2018"],lag=6)),axes=F,col=6,type='l')
abline(v=as.Date("2000-03-01"),col=2,lty=2)
abline(v=as.Date("2007-06-01"),col=2,lty=2)
