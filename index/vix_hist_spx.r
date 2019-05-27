# 
# draw overlay graph of 
# 1) VIX close histgram and 
# 2)SPX line graph. the histgram is draw red when both CLI 5 month and 1month delta is less than 0.
# 
if(file.exists("~/VIX.csv")){
  # VIX <- read.csv("~/VIX.csv")
  VIX <- as.xts(read.zoo(read.csv("~/VIX.csv")))
  system("rm \"$HOME/VIX.csv\"")
}else{
  print("!!!FILE VIX.csv DOESN'T EXIST!!!!!")
}
start_date <- "1995-01-01"
# paste(start_date,"::",sep="")
lag_month <- 5
period <- paste(start_date,substr(as.character(index(last(cli_xts))),1,10),sep="::")
# mnt <- index(cli_xts$oecd["2000::2019"][cli_xts$oecd["2000::2019"]/as.vector(cli_xts$oecd["1999-07-01::2018-08-01"]) < 1])
mnt4 <- index(cli_xts$oecd[period][na.omit(diff(cli_xts$oecd,lag=lag_month))[period] < 0 & na.omit(diff(cli_xts$oecd))[period] < 0])
# plot.zoo(merge(VIX["2000::2019-02"][,4],VIX[mnt4][,4]),type='h',col = c("red", "blue"), plot.type = "single")
plot.zoo(merge(VIX[period][,4],VIX[mnt4][,4]),type='h',col = c("blue", "red"), plot.type = "single")
abline(v=seq(as.Date(start_date),as.Date("2019-01-01"),by='years'), col=rgb(0,1,0,alpha=0.9),lty=2)
par(new=T)
# plot.default(SP5[,4]["2000::2019-02"],axes=F,type='l')
plot.default(to.monthly(SP5)[,4][period],axes=F,type='l')
