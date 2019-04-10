# mnt <- index(cli_xts$oecd["2000::2018"][cli_xts$oecd["2000::2018"] < 100 & cli_xts$oecd["2000::2018"]/as.vector(cli_xts$oecd["1999-07-01::2018-06-01"]) < 1])
# plot.zoo(merge(SP5["2000::"][,4]/SP5["2000::"][,1]-1,SP5[mnt][,4]/SP5[mnt][,1]-1),type='h',col = c("red", "blue"), plot.type = "single")

# mnt <- index(cli_xts$oecd["2000::2019"][cli_xts$oecd["2000::2019"]/as.vector(cli_xts$oecd["1999-07-01::2018-08-01"]) < 1])

mnt <- index(cli_xts$oecd["2000::"][na.omit(diff(cli_xts$oecd["1999-07-01::"],lag=6)) < 0])
# plot.zoo(merge(VIX["2000::2019-02"][,4],VIX[mnt][,4]),type='h',col = c("red", "blue"), plot.type = "single")
plot.zoo(merge(VIX[paste("2000::",substr(as.character(index(last(cli_xts))),1,7),sep="")][,4],VIX[mnt][,4]),type='h',col = c("red", "blue"), plot.type = "single")
abline(v=seq(as.Date("2001-01-01"),as.Date("2019-01-01"),by='years'), col=rgb(0,1,0,alpha=0.9),lty=2)
par(new=T)
# plot.default(SP5[,4]["2000::2019-02"],axes=F,type='l')
plot.default(SP5[,4][paste("2000::",substr(as.character(index(last(cli_xts))),1,7),sep="")],axes=F,type='l')