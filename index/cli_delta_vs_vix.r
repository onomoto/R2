# 
# plot default vix 6month delta vs. composite leading indicator delta.
#
plot.default(diff(cli_xts$oecd,lag=6)["2007::2019-01"],diff(VIX[,4],lag=6)["2007::2019-01"])
# abline(v=-0.81766,lty=2)
abline(h=as.vector(last(diff(VIX[,4],lag=6)["2007::2019-01"])),lty=2)
# abline(h=-112.1899,lty=2)
abline(v=as.vector(last(diff(cli_xts$oecd,lag=6)["2007::2019-01"])),lty=2)
abline(lm(diff(VIX[,4],lag=6)["2007::2019-01"] ~ diff(cli_xts$oecd,lag=6)["2007::2019-01"]))
abline(v=0)
abline(h=0)
