# omit getsymbols when data are up-to-dated.
getSymbols('FAS',src="yahoo",auto.assign=TRUE)
getSymbols('SPXL',src="yahoo",auto.assign=TRUE)

# built time series for FAS share holding.
date_stream <- seq(as.Date("2014-01-01"),Sys.Date(),by="days")
fas_shares <- (as.xts(rep(3472*4,length(date_stream)),date_stream))

# sold 750 shares at 2016-12-19
fas_l2 <- seq(as.Date("2016-12-19"),Sys.Date(),by="days")
fas_s2 <- c(rep(0,length(date_stream)-length(fas_l2)),rep(750,length(fas_l2)))
# build time series record for materialized cash after 2016-12-19
#
# 41.44 was the price at the end of 2016-12-09
fas_c2 <- c(rep(0,length(date_stream)-length(fas_l2)),rep(750*41.44,length(fas_l2)))

# sold 638 shares at 2017-12-04
fas_l3 <- seq(as.Date("2017-12-04"),Sys.Date(),by="days")
fas_s3 <- c(rep(0,length(date_stream)-length(fas_l3)),rep(638,length(fas_l3)))
# record for materialized cash after 2017-12-04
# 69.3 was the price to be soldl at  2017-12-04.
fas_c3 <- c(rep(0,length(date_stream)-length(fas_l3)),rep(638*69.3,length(fas_l3)))
#
# sold 500 shares at 2018-12-24
#
fas_l4 <- seq(as.Date("2018-12-24"),Sys.Date(),by="days")
fas_s4 <- c(rep(0,length(date_stream)-length(fas_l4)),rep(500,length(fas_l4)))
#
# record for materialized cash after 2018-12-24
# 39.8312 was the price to be soldl at  2018-12-24.
#
fas_c4 <- c(rep(0,length(date_stream)-length(fas_l4)),rep(500*39.8312,length(fas_l4)))
#
# special capital gain divident for SPXL $1.6. 10% US income tax for Firstrade
#
spxl_l1 <- seq(as.Date("2017-12-15"),Sys.Date(),by="days")
spxl_c1 <- c(rep(0,length(date_stream)-length(spxl_l1)),rep(23412+9326,length(spxl_l1)))
#
# sold 1000 shares at 2018-12-24
#
spxl_l2 <- seq(as.Date("2018-12-24"),Sys.Date(),by="days")
spxl_s2 <- c(rep(0,length(date_stream)-length(spxl_l2)),rep(1000,length(spxl_l2)))
# record for materialized cash after 2017-12-04
# 69.3 was the price to be soldl at  2017-12-04.
spxl_c2 <- c(rep(0,length(date_stream)-length(spxl_l2)),rep(1000*29.557,length(spxl_l2)))





# calculate time series for FAS
#
# take changes of ownership by 2016-12-19 and 2017-12-04 trade
#
fas_shares <- fas_shares - fas_s2
fas_shares <- fas_shares - fas_s3
fas_shares <- fas_shares - fas_s4

# calculate time series for SPXL
spxl_shares <- as.xts(rep(21000,length(date_stream)),date_stream)
spxl_shares <- spxl_shares - spxl_s2

# candleChart(to.weekly(fas_shares * FAS[,4] +spxl_shares * SPXL[,4]),theme='white')

candleChart(to.weekly(fas_shares * FAS[,4] +spxl_shares * SPXL[,4]+as.xts(fas_c2+fas_c3+fas_c4+spxl_c1+spxl_c2,index(fas_shares))),theme='white')
t <- as.xts(rep(2150000,length(weekly_pf[,1])),index(weekly_pf))
addTA(t,on=1,legend="",lty=2,order=10)
open_v <- as.vector(weekly_pf[1,1])
close_v <- as.vector(weekly_pf[length(weekly_pf[,1]),4])
ratio <- (close_v/open_v)**(1/length(weekly_pf[,1]))
t <- as.xts(open_v*ratio**(1:length(weekly_pf[,1])),index(weekly_pf))
addTA(t,on=1,legend="",lty=2,order=10)


# store xts data into weekly_pf

weekly_pf <- to.weekly(fas_shares * FAS[,4] +spxl_shares * SPXL[,4]+as.xts(fas_c2+fas_c3+fas_c4+spxl_c1+spxl_c2,index(fas_shares)))
colnames(weekly_pf)[1] <- 'open'
colnames(weekly_pf)[2] <- 'high'
colnames(weekly_pf)[3] <- 'low'
colnames(weekly_pf)[4] <- 'close'
