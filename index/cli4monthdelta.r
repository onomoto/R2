#
# draw CLI 4 month delta vs. its reading graph.
#

plot.default(na.trim(diff(cli_xts$oecd["2014-09-01::"],lag=4)),cli_xts$oecd["2015::"],type='b')
tmp <- par('usr')


plot.default(na.trim(diff(cli_xts$oecd["2014-09-01::"],lag=4)),cli_xts$oecd["2015::"],type='b',xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]))
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2018-09-01::2019"],lag=4)),cli_xts$oecd["2019"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=9,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2017-09-01::2018"],lag=4)),cli_xts$oecd["2018"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=2,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2016-09-01::2017"],lag=4)),cli_xts$oecd["2017"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=3,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2015-09-01::2016"],lag=4)),cli_xts$oecd["2016"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=4,lwd=2)
par(new=T)
plot.default(na.trim(diff(cli_xts$oecd["2014-09-01::2015"],lag=4)),cli_xts$oecd["2015"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=5,lwd=2)
par(new=T)

abline(v=0)
abline(h=100)
legend("topleft", legend = "Pink: 2015\nLight Blue: 2016\nLime: 2017\nRed: 2018\nBlack: 2019",bty='n')
