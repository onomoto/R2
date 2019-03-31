hist(as.vector(VIX[as.Date(setdiff(seq(as.Date("1990-05-01"),as.Date("2019-01-01"),by='months') ,as.Date(index(na.trim(diff(cli_xts$oecd["1990::"],lag=4))[na.trim(diff(cli_xts$oecd["1990::"],lag=4)) < -0.2 ]))))][,4]),xlim=c(5,60),ylim=c(0,60),col=rgb(1,1,0,alpha=0.8),breaks = 10)
par(new=T)
hist(as.vector(VIX[as.Date(index(na.trim(diff(cli_xts$oecd["1990::"],lag=4))[na.trim(diff(cli_xts$oecd["1990::"],lag=4)) < -0.2 ]))][,4]),xlim=c(5,60),ylim=c(0,60),col=rgb(0,0,1,alpha=0.2),breaks=20)
