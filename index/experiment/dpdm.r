# draw over lay histogram between daily return on cli delta plus and minus.
#
# l <- length(index(SP5))
# w <- SP5[,4][2:l]/as.vector(SP5[,4][1:l-1])
w <- to.monthly(SP5["1970::2018"])[,4]/to.monthly(SP5["1970::2018"])[,1]
# dm <- w[index(cli_xts$oecd[as.Date(index(w))] <100)]
# length(dm)
# dp <- w[index(cli_xts$oecd[as.Date(index(w))] >100)]
dm <- w[index(cli_xts$oecd[cli_xts$oecd < 100][index(w)])]
length(dm)
dp <- w[index(cli_xts$oecd[cli_xts$oecd > 100][index(w)])]
length(dp)
hist(dp,breaks=100,xlim=c(0.95,1.05),col=rgb(1,0.6,0,alpha=0.81),ylim=c(0,30))
par(new=T)
hist(dm,breaks=100,xlim=c(0.95,1.05),col=rgb(0,0.3,0.9,alpha=0.5),ylim=c(0,30))
t.test(dp,dm)