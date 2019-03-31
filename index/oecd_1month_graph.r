

plot.default(diff(cli_xts$oecd["2014-12-01::"])[-1],cli_xts$oecd["2015::"],type='b')
tmp <- par('usr')
plot.default(diff(cli_xts$oecd["2014-12-01::"])[-1],cli_xts$oecd["2015::"],type='b',xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]))
par(new=T)
plot.default(diff(cli_xts$oecd["2016-12-01::2017"])[-1],cli_xts$oecd["2017"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=3,lwd=2)
par(new=T)
plot.default(diff(cli_xts$oecd["2015-12-01::2016"])[-1],cli_xts$oecd["2016"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=4,lwd=2)
par(new=T)
plot.default(diff(cli_xts$oecd["2015-12-01::2016"])[-1],cli_xts$oecd["2016"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=5,lwd=2)
par(new=T)
plot.default(diff(cli_xts$oecd["2014-12-01::2015"])[-1],cli_xts$oecd["2015"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=6,lwd=2)
par(new=T)
plot.default(diff(cli_xts$oecd["2017-12-01::"])[-1],cli_xts$oecd["2018"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=2,lwd=2)
#
#    belows are for the case the data after 2019 are released. comment out 2 ines above, and remove comment below 4 lines.
#
# par(new=T)
# plot.default(diff(cli_xts$oecd["2017-12-01::2018"])[-1],cli_xts$oecd["2018"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=7,lwd=2)
# par(new=T)
# plot.default(diff(cli_xts$oecd["2018-12-01::"])[-1],cli_xts$oecd["2019"],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=2,lwd=2)
#
abline(v=0)
abline(h=100)
# tmp.legend <- "Pink: 2015\nLight Blue: 2016\nLime: 2017\nRed: 2018"
# addLegend(legend.loc = "topleft", legend.names = tmp.legend,col=3)
legend("topleft", legend = "Pink: 2015\nLight Blue: 2016\nLime: 2017\nRed: 2018",bty='n')
# legend("topleft", legend = "Pink: 2015\nLight Blue: 2016\nLime: 2017\nRed: 2018",pch = pchs, lty = ltys)
