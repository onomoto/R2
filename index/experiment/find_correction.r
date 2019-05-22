op <- options(digits.secs = 6)
Sys.time()
a <- Sys.time()
j <- 0
correction_ratio <- 0.9
hist_high <- c()
recent_high <- c()
correction_flag <- c()
output <- c()
status_flag <- 0
depth <- c()
# depth <- as.xts(0,0,0,as.Date("1950-01-01"))
depth_ratio <- 0
depth_new <- c()
depth_date <- as.Date("1950-01-01")
search_range <- 250
for( i in seq(search_range,length(index(SP5)),1)) {
  z <- i-search_range+1
  recent_high <- append(recent_high,as.xts(max(SP5[,4][z:i]),index(SP5[i])))
  if(SP5[,4][i] < correction_ratio * last(recent_high)[,1]){
      correction_flag <- append(correction_flag,1)
      # hist_high <- append(hist_high
      # c(hist_high,as.xts(hist_high,as.vector(SP5[,4][i]),index(SP5[,4][i])))
  }else{
      correction_flag <- append(correction_flag,0)
  }
  if(z > 17000){
    cat(z)
    cat(" ")
  }
  # cat(z)
  # cat(" ")
}
recent_high <- merge(recent_high,correction_flag)
recent_high <- merge(recent_high,SP5[,4][search_range:length(index(SP5))])
for(i in seq(1,length(index(recent_high)),1)){
  if(recent_high[,2][i] == 1 && status_flag == 0){
    # print("starts at ")
    # print(index(recent_high[i]))
    output <- append(output,
    as.xts(recent_high[,3][i]/recent_high[,1][i],index(recent_high[i])))
    status_flag <- 1
  }
  if(recent_high[,2][i] == 1 && status_flag == 1){
    # print("starts at ")
    # print(index(recent_high[i]))
    output <- append(output,
    as.xts(recent_high[,3][i]/recent_high[,1][i],index(recent_high[i])))
    # status_flag <- 1
  }
  if(recent_high[,2][i] == 0 && status_flag == 1){
    print("ends at ")
    print(index(recent_high[i]))
    # output <- append(output,
    # as.xts(recent_high[,3][i]/recent_high[,1][i],index(recent_high[i])))
    status_flag <- 0
  }

}
#
# use with plot.default() graph
#
period_date <- "1970::"
for(i in seq(1,length(output[period_date]),1)){
    abline(v=index(output[period_date])[i],col=rgb(0.5,0,0.5,alpha=0.1),lty=1,lwd=1)
    # cat(i)
    # cat(" ")
}
abline(v=index(output[period_date])[1],col=rgb(0.5,0,0.5,alpha=0.1),lty=1,lwd=1)
#
# or plot.xts() graph
#
plot(as.xts(output[,1]-1,index(output)),type='h')

Sys.time()
b <- Sys.time()
b - a
op <- options(digits.secs = 2)
