#
# 1)pick up months whose clie_xts$oecd  is up from the previous
# 2)create the stream of flags, in which up is 1 and down is 0
# 3)compare SPX close price between the end month of cli_xts delta is plus(or minus) and its start.
# 4)return xts objects which contains. start month of period, updown ration, length of months and monthly average return during period.

func <- function(pm="plus",s="1970-01-01",l=1){
  w <- c()
  cat("0")
  cat(length(s))
  if(nchar(s) == 10){ # use nchar() to measure strings length, not length()
    cat("1")
    last_date <- last(index(cli_xts$oecd))
    start_date <- s
    period <- paste(start_date,last_date,sep='::')
  }else{
     # last_date <- l
     period <- s
  }
  # last_date <- last(index(cli_xts$oecd))
  # start_date <- s
  # period <- paste(start_date,last_date,sep='::')
  start_index <- 1
  iteration <- 0
  performance_val <- c()
  period_length <- c()
  lag_month <- l
  result <- c()
  rate <- c()
  plus_or_minus <- pm
  open_p <- c()
  close_p <- c()

# put flag on the months accoring to the parameter. for "minus" cli delta is less than ZERO, for plus the opposite.
  for(i in seq(1,length(diff(cli_xts$oecd,lag=lag_month)[period]),1,)){
    if(plus_or_minus == "minus"){
      if(as.vector(diff(cli_xts$oecd,lag=lag_month)[period])[i] < 0){  # up is "> 0"
        w <- append(w,1)
      }else{
        w <- append(w,0)
      }
    }else if(plus_or_minus == "plus"){
      if(as.vector(diff(cli_xts$oecd,lag=lag_month)[period])[i] > 0){  # up is "> 0"
        w <- append(w,1)
      }else{
        w <- append(w,0)
      }
    }else{
      stop("please use plus or minus as 1st parameter")
    }
  }
  month_flag <- 0 # status flag
# check stream and when flag is changes 0 to 1. it is the start of period.
  for(i in seq(1,length(diff(cli_xts$oecd,lag=lag_month)[period]),1,)){
    if(w[i] == 1){
      if(month_flag == 0){ # when w is 1 and month_flag is 0, the period starts
        month_flag <- 1
        start_price <- as.vector(to.monthly(SP5[period])[,1][i]) #dc 0531
        # print(index(to.monthly(SP5[period])[,4][i]))
        # cat("from ")
        # cat(as.character(as.Date(index(to.monthly(SP5[period])[,4][i]))))
        start_index <- i
      }
      # dc 0602 add output at the end of loop
      if(i == length(diff(cli_xts$oecd,lag=lag_month)[period])){
        # print("end of the loop")
        result <- append(result,as.xts(as.vector(to.monthly(SP5[period])[,4][i]) / start_price,index(to.monthly(SP5[period])[,4][i])))
                period_length <- append(period_length,i-start_index)
        performance_val <- append(performance_val,as.vector(to.monthly(SP5[period])[,4][i]) / start_price) # bf 0605 put the end month of record at the end of the loop.
        open_p <- append(open_p,start_price)
        close_p <- append(close_p,as.vector(to.monthly(SP5[period])[,4][i])) # bf 0605 same as above.
        rate <- append(rate,last(performance_val)**(1/last(period_length))-1)
      }
# check stream and when flag is changes 1 to 0. it is the start of period.
    }else if(w[i] ==0){
      if(month_flag == 1){ # when w is 0 and month_flag is 1, the period ends
        # cat(" ")
        # cat(i - start_index)
        # cat(" month(s)")
        # cat("\n")
        iteration <- iteration +1
        cat(iteration)
        cat(" ")
        result <- append(result,as.xts(as.vector(to.monthly(SP5[period])[,4][i-1]) / start_price,index(to.monthly(SP5[period])[,4][i-1])))
        # print(as.xts(as.vector(to.monthly(SP5[period])[,4][i]) / start_price,index(to.monthly(SP5[period])[,4][i])))
        # print(i - start_index)
        month_flag <- 0 # when period ends, intialize the flag.
        period_length <- append(period_length,i-start_index)
        performance_val <- append(performance_val,as.vector(to.monthly(SP5[period])[,4][i-1]) / start_price)
        open_p <- append(open_p,start_price)
        close_p <- append(close_p,as.vector(to.monthly(SP5[period])[,4][i-1]))
        rate <- append(rate,last(performance_val)**(1/last(period_length))-1)
      }
    }
  }
  cat("\n")
  print(mean(period_length))
  print(mean(performance_val))
  return(merge(result,period_length,rate,open_p,close_p))

}
# t_minus <- performance_val
# t_plus <- performance_val
func("plus","1970-01-01")
func("minus","1970-01-01")
# histogram
hist(as.vector(func("minus","1970-01-01")[,1])-1,col=rgb(0.5,1,0),breaks=20,xlim=c(-0.6,0.5),ylim=c(0,8))
par(new=T)
hist(as.vector(func("plus","1970-01-01")[,1])-1,col=rgb(0.5,0,1,alpha=0.4),breaks=10,xlim=c(-0.6,0.5),ylim=c(0,8))
