
#
# draw spx hisgram, while reading monthly close drawned in the different color when cli delta is positive and negative.
#
#
#
lag_month <- 1
# mnt <- index(cli_xts$oecd[total_terms][cli_xts$oecd[total_terms]/as.vector(cli_xts$oecd["1999-07-01::2018-07-01"]) < 1])
total_terms <- paste("1990",index(last(cli_xts$oecd)),sep="::")

mnt_minus <- index(cli_xts$oecd[total_terms][na.omit(diff(cli_xts$oecd,lag=lag_month))[total_terms] < 0])
mnt_plus <- index(cli_xts$oecd[total_terms][na.omit(diff(cli_xts$oecd,lag=lag_month))[total_terms] > 0])

plot.zoo(merge(log(to.monthly(SP5)[mnt_plus][,4]),log(to.monthly(SP5)[mnt_minus][,4])),type='h',col = c("blue", "red"), plot.type = "single")
# abline(v=seq(as.Date("1991-01-01"),as.Date("2019-01-01"),by='years'), col=rgb(0,1,0,alpha=0.9),lty=2)
# abline(v=seq(1,12*29,12), col=rgb(0,1,0,alpha=0.9),lty=2)
par(new=T)
plot.default(log(to.monthly(SP5)[,4][total_terms]),axes=F,type='l')
abline(v=seq(1,12*29,12), col=rgb(0,1,0,alpha=0.9),lty=2)