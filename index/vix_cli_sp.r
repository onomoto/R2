#
# see https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html for the detail.
#
# draw overlay graph of vix and spx, while vix will be drawned in the different color when cli 6 month delta is positive and negative.
#
#
#
mnt <- index(cli_xts$oecd["2000::2019"][cli_xts$oecd["2000::2019"]/as.vector(cli_xts$oecd["1999-07-01::2018-07-01"]) < 1])
plot.zoo(merge(VIX["2000::2019-01"][,4],VIX[mnt][,4]),type='h',col = c("red", "blue"), plot.type = "single")
abline(v=seq(as.Date("2001-01-01"),as.Date("2019-01-01"),by='years'), col=rgb(0,1,0,alpha=0.9),lty=2)
par(new=T)
plot.default(SP5[,4]["2000::2019-01"],axes=F,type='l')
