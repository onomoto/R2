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
end_date <- "2018-12-01"

# select dates when CLI moves negative during 6 months.
# mnt <- index(cli_xts$oecd["2000::2018"][cli_xts$oecd["2000::2018"]/as.vector(cli_xts$oecd["1999-07-01::2018-06-01"]) < 1])

mnt <- index(na.trim(diff(cli_xts$oecd[paste("1999-07-01",end_date,sep="::")],lag=6))[na.trim(diff(cli_xts$oecd[paste("1999-07-01",end_date,sep="::")],lag=6)) < 0])
#


# compare monthly high between negative vs. positve
t.test(as.vector(VIX[,2][mnt]),as.vector(VIX[,2][as.Date(setdiff(seq(as.Date("2000-01-01"),as.Date(end_date),by='months'),mnt))]))

#  Welch Two Sample t-test
#
# data:  as.vector(VIX[, 2][mnt]) and as.vector(VIX[, 2][as.Date(setdiff(seq(as.Date("2000-01-01"), as.vector(VIX[, 2][mnt]) and     as.Date(end_date), by = "months"), mnt))])
# t = 4.8495, df = 174.7, p-value = 2.725e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  4.164086 9.879529
# sample estimates:
# mean of x mean of y
#  28.55741  21.53560

# compare monthly close.
t.test(as.vector(VIX[,4][mnt]),as.vector(VIX[,4][as.Date(setdiff(seq(as.Date("2000-01-01"),as.Date(end_date),by='months'),mnt))]))

#  Welch Two Sample t-test
#
# data:  as.vector(VIX[, 4][mnt]) and as.vector(VIX[, 4][as.Date(setdiff(seq(as.Date("2000-01-01"), as.vector(VIX[, 4][mnt]) and     as.Date(end_date), by = "months"), mnt))])
# t = 4.7722, df = 178.21, p-value = 3.781e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  2.860369 6.893837
# sample estimates:
# mean of x mean of y
#  22.19598  17.31888

# draw translucent histgram to overay the graph in comparison.
# when rgb=(1,1,0), it creates "yellow" graph.
# alpha is the parameter of density from 0 to 1.
#

hist(as.vector(VIX[,4][as.Date(setdiff(seq(as.Date("2000-01-01"),as.Date(end_date),by='months'),mnt))]),ylim=c(0,30),xlim=c(10,60),breaks=10,col=rgb(0, 1, 0, alpha=0.9))
par(new=T)
hist(as.vector(VIX[,4][mnt]),ylim=c(0,30),xlim=c(10,60),breaks=20,col=rgb(1, 1, 0, alpha=0.5))
