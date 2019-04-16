# omit getsymbols when data are up-to-dated.
getSymbols('FAS',src="yahoo")
getSymbols('SPXL',src="yahoo")

# built time series for FAS share holding.
date_stream <- seq(as.Date("2014-01-01"),Sys.Date(),by="days")
fas_shares <- (as.xts(rep(3472*4,length(date_stream)),date_stream))

# sold 750 shares at 2016-12-19
l2 <- seq(as.Date("2016-12-19"),Sys.Date(),by="days")
s2 <- c(rep(0,length(date_stream)-length(l2)),rep(750,length(l2)))
# build time series record for materialized cash after 2016-12-19
#
# 41.44 was the price at the end of 2016-12-09
c2 <- c(rep(0,length(date_stream)-length(l2)),rep(750*41.44,length(l2)))

# sold 638 shares at 2017-12-04
l3 <- seq(as.Date("2017-12-04"),Sys.Date(),by="days")
s3 <- c(rep(0,length(date_stream)-length(l3)),rep(638,length(l3)))
# record for materialized cash after 2017-12-04
# 69.3 was the price to be soldl at  2017-12-04.
c3 <- c(rep(0,length(date_stream)-length(l3)),rep(638*69.3,length(l3)))
# special capital gain divident for SPXL $1.6. 10% US income tax for Firstrade
#
l4 <- seq(as.Date("2017-12-15"),Sys.Date(),by="days")
c4 <- c(rep(0,length(date_stream)-length(l4)),rep(23412+9326,length(l4)))
# calculate time series for FAS
#
# take changes of ownership by 2016-12-19 and 2017-12-04 trade
#
fas_shares <- fas_shares - s2
fas_shares <- fas_shares - s3

# calculate time series for SPXL
spxl_shares <- as.xts(rep(21000,length(date_stream)),date_stream)

# candleChart(to.weekly(fas_shares * FAS[,4] +spxl_shares * SPXL[,4]),theme='white')

candleChart(to.weekly(fas_shares * FAS[,4] +spxl_shares * SPXL[,4]+as.xts(c2+c3+c4,index(fas_shares))),theme='white')

# store xts data into weekly_pf

weekly_pf <- to.weekly(fas_shares * FAS[,4] +spxl_shares * SPXL[,4]+as.xts(c2+c3+c4,index(fas_shares)))
colnames(weekly_pf)[1] <- 'open'
colnames(weekly_pf)[2] <- 'high'
colnames(weekly_pf)[3] <- 'low'
colnames(weekly_pf)[4] <- 'close'
