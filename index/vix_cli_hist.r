#
# draw histgram according to frequency based upon monthly close price
# https://00819.blogspot.com/2019/03/vix-vs-cli-6-month-delta.html
#
# downloads ^VIX historical data from Yahoo Finance.
# adjust start date
#
# s is start date like "2011-01-01" OR "2010-01-01::2019-01-31"
# b1 is # of breaks during cli delta is positive
# b2 is # of negative
# d is # of months to calculate delta.
# yu is ylim upperlimit
# xu is xlim upperlimit
#
func <- function(s="2001-01-01",b1=10,b2=10,d=5,yu=60,xu=60){
  #
  # VIX <- as.xts(read.zoo(read.csv("~/VIX.csv")))
  start_date <- s
  if(file.exists("~/VIX.csv")){
    # VIX <- read.csv("~/VIX.csv")
    VIX <<- as.xts(read.zoo(read.csv("~/VIX.csv")))
    system("rm \"$HOME/VIX.csv\"")
  }else{
    print("!!!FILE VIX.csv DOESN'T EXIST!!!!!")
  }
  last(VIX)
  # end_date <- "2018-12-01"
  #
  #  PARAMETER! CAUTION!!!
  #
  if(nchar(s) == 10){  # for the case of "YYYY-MM-DD"
      end_date <- last(index(cli_xts))  # pick up the ending date from the last index.
      period <- paste(start_date,substr(as.character(index(last(cli_xts))),1,7),sep="::")
  }else if(nchar(s) == 22){ # for the case of "YYYY-MM-DD::YYYY-MM-DD"
      end_date <- substr(s,13,22)
      period <- s
  }else{ 
    # stop when neither of them is the case.
    stop("1st parameter should be YYYY-MM-DD or YYYY-MM-DD::YYYY-MM-DD!")
  }
  # end_date <- last(index(cli_xts))
  lag_month <- d
  #
  # CAUTION start_date must be "YYYY-MM-DD". OR "YYYY-MM-DD::YYYY-MM-DD" DON'T REMOVE MM-DD!!!
  #
  # select dates when CLI moves negative during 6 months.
  # mnt <- index(cli_xts$oecd["2000::2018"][cli_xts$oecd["2000::2018"]/as.vector(cli_xts$oecd["1999-07-01::2018-06-01"]) < 1])
  #
  mnt <- index(na.omit(diff(cli_xts$oecd,lag=lag_month))[period][na.omit(diff(cli_xts$oecd,lag=lag_month))[period] < 0])
  cat("total is ")
  cat(length(seq(as.Date(start_date),as.Date(end_date),by='months')))
  cat(" months\n")
  print(mnt)
  # compare monthly high between negative vs. positve
  print(t.test(as.vector(VIX[,2][mnt]),as.vector(VIX[,2][as.Date(setdiff(seq(as.Date(start_date),as.Date(end_date),by='months'),mnt))])))

  # compare monthly close.
  print(t.test(as.vector(VIX[,4][mnt]),as.vector(VIX[,4][as.Date(setdiff(seq(as.Date(start_date),as.Date(end_date),by='months'),mnt))])))

  # draw translucent histgram to overay the graph in comparison.
  # when rgb=(1,1,0), it creates "yellow" graph.
  # alpha is the parameter to specify the density from 0 to 1.

  hist(as.vector(VIX[,4][as.Date(setdiff(seq(as.Date(start_date),as.Date(end_date),by='months'),mnt))]),ylim=c(0,yu),xlim=c(10,xu),breaks=b1,col=rgb(0, 1, 0, alpha=0.9),xlab='VIX monthly close',ylab='VIX',main='vix_cli_hist.r')
  par(new=T)
  hist(as.vector(VIX[,4][mnt]),xlim=c(10,xu),ylim=c(0,yu),breaks=b2,col=rgb(1, 1, 0, alpha=0.5),xlab='',ylab='',main='')
}
func("2011-01-01",10,40,5,15,35)
