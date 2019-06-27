# draw over lay histogram between daily return on cli delta plus and minus.
#
l <- length(index(SP5))
w <- SP5[,4][2:l]/as.vector(SP5[,4][1:l-1])
dm <- w[diff(cli_xts$oecd)[substr((as.character(index(w))),1,7)] <0]
length(dm)
dp <- w[diff(cli_xts$oecd)[substr((as.character(index(w))),1,7)] >0]
hist(dm,breaks=300,xlim=c(0.95,1.05),col=rgb(0.3,0.6,0,alpha=0.81),ylim=c(0,600))
par(new=T)
hist(dp,breaks=200,xlim=c(0.95,1.05),col=rgb(0,0.3,0.9,alpha=0.5),ylim=c(0,600))
t.test(dp,dm)