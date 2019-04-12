#
# plese refer the latter half of
# https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html
# lag is set to  5 months.
#
plot.default(na.trim(diff(cli_xts$oecd["2010-08-01::"],lag=5)),cli_xts$oecd["2011::"],type='b')
tmp <- par('usr')
plot.default(na.trim(diff(cli_xts$oecd["2010-08-01::"],lag=5)),cli_xts$oecd["2011::"],type='b',xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]))
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2018-08-01::2019"],lag=5)),cli_xts$oecd["2019"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=9,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2017-08-01::2018"],lag=5)),cli_xts$oecd["2018"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=2,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2016-08-01::2017"],lag=5)),cli_xts$oecd["2017"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=3,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2015-08-01::2016"],lag=5)),cli_xts$oecd["2016"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=4,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2014-08-01::2015"],lag=5)),cli_xts$oecd["2015"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=5,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2013-08-01::2014"],lag=5)),cli_xts$oecd["2014"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=6,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2012-08-01::2013"],lag=5)),cli_xts$oecd["2013"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=7,lwd=2,type='p',pch='x')
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2011-08-01::2012"],lag=5)),cli_xts$oecd["2012"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=8,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2010-08-01::2011"],lag=5)),cli_xts$oecd["2011"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=9,lwd=2,bg='grey')
abline(v=0)
abline(h=100)
abline(v=seq(0.5,-1,-0.1),col=6,lty=3)
