#
# 1)pick up months whose clie_xts$oecd  is up from the previous
# 2)crate the stream of flags, in which up is 1 and down is 0
# 3)compare SPX close price between the end month of cli_xts delta is plus and its start.
#
w <- c()
last_date <- last(index(cli_xts$oecd))
start_date <- ["1995-01-01"]
period <- paste(start_date,last_date,sep='::')
start_index <- 1


for(i in seq(1,length(diff(cli_xts$oecd)[period]),1,)){
  if(as.vector(diff(cli_xts$oecd)[period])[i] > 0){  # up is "> 0"
    w <- append(w,1)
  }else{
    w <- append(w,0)
  }
}
month_flag <- 0
for(i in seq(1,length(diff(cli_xts$oecd)[period]),1,)){
  if(w[i] == 1){
    if(month_flag == 0){
      month_flag <- 1
      start_price <- as.vector(to.monthly(SP5[period])[,4][i])
      print(index(to.monthly(SP5[period])[,4][i]))
      start_index <- i
    }
  }else if(w[i] ==0){
    if(month_flag == 1){
      cat(" ")
      cat(i - start_index)
      print(" month(s)")
      print(as.xts(as.vector(to.monthly(SP5[period])[,4][i]) / start_price,index(to.monthly(SP5[period])[,4][i])))
      # print(i - start_index)
      month_flag <- 0
    }
  }
}
