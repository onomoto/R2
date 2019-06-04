#
# plot default spx 6month delta vs. composite leading indicator delta.
#
func <- function(p="2007::2019-03",l_c=6,l_s=6){
  period <- p
  lag_month <- l_c
  lag_spx <- l_s
  SP5 <- to.monthly(SP5)
  plot.default(diff(cli_xts$oecd,lag=lag_month)[period],diff(SP5[,4],lag=lag_spx)[period])
  # abline(v=-0.81766,lty=2)
  abline(h=as.vector(last(diff(SP5[,4],lag=lag_spx)[period])) ,lty=2)
  # abline(h=-112.1899,lty=2)
  abline(v=as.vector(last(diff(cli_xts$oecd,lag=lag_month)[period])),lty=2)
  abline(lm(diff(SP5[,4],lag=lag_spx)[period] ~ diff(cli_xts$oecd,lag=lag_month)[period]))
  abline(v=0)
  abline(h=0)
}
func("2007::2019-03",5,5)
