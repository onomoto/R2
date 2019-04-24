#
# https://00819.blogspot.com/2019/03/vix-vs-cli-6-month-delta.html
#
# downloads ^VIX historical data from Yahoo Finance.
#
# VIX <- as.xts(read.zoo(read.csv("~/VIX.csv")))
if(file.exists("~/Downloads/VIX.csv")){
  bp2018 <- read.csv("~/Downloads/VIX.csv")
  system("rm \"$HOME/Downloads/VIX.csv\"")
}else{
  print("!!!FILE VIX.csv DOESN'T EXIST!!!!!")
}
last(VIX)
# end_date <- "2018-12-01"
#
#  PARAMETER! CAUTION!!!
#
end_date <- last(index(cli_xts))
lag_month <- 5

# select dates when CLI moves negative during 6 months.
# mnt <- index(cli_xts$oecd["2000::2018"][cli_xts$oecd["2000::2018"]/as.vector(cli_xts$oecd["1999-07-01::2018-06-01"]) < 1])
#
mnt <- index(na.trim(diff(cli_xts$oecd[paste("1999-07-01",end_date,sep="::")],lag=lag_month))[na.trim(diff(cli_xts$oecd[paste("1999-07-01",end_date,sep="::")],lag=lag_month)) < 0])

# compare monthly high between negative vs. positve
t.test(as.vector(VIX[,2][mnt]),as.vector(VIX[,2][as.Date(setdiff(seq(as.Date("2000-01-01"),as.Date(end_date),by='months'),mnt))]))

# compare monthly close.
t.test(as.vector(VIX[,4][mnt]),as.vector(VIX[,4][as.Date(setdiff(seq(as.Date("2000-01-01"),as.Date(end_date),by='months'),mnt))]))

# draw translucent histgram to overay the graph in comparison.
# when rgb=(1,1,0), it creates "yellow" graph.
# alpha is the parameter to specify the density from 0 to 1.

hist(as.vector(VIX[,4][as.Date(setdiff(seq(as.Date("2000-01-01"),as.Date(end_date),by='months'),mnt))]),ylim=c(0,30),xlim=c(10,60),breaks=10,col=rgb(0, 1, 0, alpha=0.9))
par(new=T)
hist(as.vector(VIX[,4][mnt]),ylim=c(0,30),xlim=c(10,60),breaks=20,col=rgb(1, 1, 0, alpha=0.5))
