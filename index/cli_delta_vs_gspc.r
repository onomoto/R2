#
# plot default spx 6month delta vs. composite leading indicator delta.
#
plot.default(diff(cli_xts$oecd,lag=6)["2007::2019-01"],diff(SP5[,4],lag=6)["2007::2019-01"])
# abline(v=-0.81766,lty=2)
abline(h=as.vector(last(diff(SP5[,4],lag=6)["2007::2019-01"])) ,lty=2)
# abline(h=-112.1899,lty=2)
abline(v=as.vector(last(diff(cli_xts$oecd,lag=6)["2007::2019-01"])),lty=2)
abline(lm(diff(SP5[,4],lag=6)["2007::2019-01"] ~ diff(cli_xts$oecd,lag=6)["2007::2019-01"]))
abline(v=0)
abline(h=0)
