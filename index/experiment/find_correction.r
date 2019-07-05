op <- options(digits.secs = 6)
Sys.time()
a <- Sys.time()
# j <- 0
func <- function(){
  correction_ratio <- 0.9
  hist_high <- c()
  recent_high <- c()
  correction_flag <- c()
  output <- c()
  status_flag <- 0
  day_count <- 0
  # depth <- c()
  # # depth <- as.xts(0,0,0,as.Date("1950-01-01"))
  depth_ratio <- c()
  # depth_new <- c()
  # depth_date <- as.Date("1950-01-01")
  search_range <- 100
  for( i in seq(search_range,length(index(SP5)),1)) {
    z <- i-search_range+1
    recent_high <- append(recent_high,as.xts(max(SP5[,4][z:i]),index(SP5[i])))
    if(SP5[,4][i] < correction_ratio * last(recent_high)[,1]){ #correction starts when current is lower than the certain percentage of recent high
        correction_flag <- append(correction_flag,1)
        # hist_high <- append(hist_high
        # c(hist_high,as.xts(hist_high,as.vector(SP5[,4][i]),index(SP5[,4][i])))
    }else{
      # exit criateria is 5% above the one to enter.
      if(SP5[,4][i] > (0.05+correction_ratio) * last(recent_high)[,1]){
        correction_flag <- append(correction_flag,0)
      }else{
        if(last(correction_flag) == 1){ # even if current price is less than exit criteria
          correction_flag <- append(correction_flag,1)
        }else{
          correction_flag <- append(correction_flag,0) # if it is not already in correction. it is NOT correction.
        }
        # correction_flag <- append(correction_flag,1) #otherwise correction continues.
      }
    }
    if((z %% 500) == 0){
      cat(".")
      cat("")
    }
    # cat(z)
    # cat(" ")
  }
  recent_high <- merge(recent_high,correction_flag)
  recent_high <- merge(recent_high,SP5[,4][search_range:length(index(SP5))])
  for(i in seq(1,length(index(recent_high)),1)){
    #
    # in the middle of correction
    # don't put this after "at the start of correction" section
    if(recent_high[,2][i] == 1 && status_flag == 1){
      # print("starts at ")
      # print(index(recent_high[i]))
      output <- append(output,
      as.xts(recent_high[,3][i]/recent_high[,1][i],index(recent_high[i])))
      if(depth_ratio > as.vector(recent_high[,3][i]/recent_high[,1][i])){
        depth_ratio <- recent_high[,3][i]/recent_high[,1][i]
      }
      day_count <- day_count +1
      # status_flag <- 1
    }
    # at the start of correction
    if(recent_high[,2][i] == 1 && status_flag == 0){
      # print("starts at ")
      # print(index(recent_high[i]))
      output <- append(output,
      as.xts(recent_high[,3][i]/recent_high[,1][i],index(recent_high[i])))
      depth_ratio <- as.vector(recent_high[,3][i]/recent_high[,1][i])
      start_date <- index(recent_high[i])
      # put correction status = ON
      status_flag <- 1
      day_count <- day_count +1
    }

    # at the end of correction
    if(recent_high[,2][i] == 0 && status_flag == 1){

      depth_ratio <- 0 # initialize
      status_flag <- 0
      day_count <- 0
    }
    if((i %% 100) == 0){
      cat("@")
      cat("")
    }
  }
  # dc: add code to calculate day distance and append its result
  len <- c()
  for(i in seq(1,length(index(output)),1)) {
    len <- append(len,length(index(SP5[paste(as.character(index(SP5[1])),as.character(index(output[i])),sep="::")])))

    if((i %% 100) == 0){
      cat("#")
      cat("")
    }
  }
  output <- merge(output,len)
  return(output)
}
correction_data <- func()



plot(as.xts(correction_data[,1]-1,index(correction_data)),type='h')
#
Sys.time()
b <- Sys.time()
b - a
op <- options(digits.secs = 2)

period_date <- paste("1995-01-01",last(index(cli_xts)),sep="::")
plot.default(index(diff(cli_xts)[period_date]),diff(cli_xts[,1])[period_date],type='h')
for(i in seq(1,length(correction_data[period_date]),1)){
    abline(v=index(correction_data[period_date])[i],col=rgb(0.5,0,0.5,alpha=0.1),lty=1,lwd=1)
    # cat(i)
    # cat(" ")
}
abline(v=index(correction_data[period_date])[1],col=rgb(0.5,0,0.5,alpha=0.1),lty=1,lwd=1)
abline(v=seq(as.Date("1995-01-01"),as.Date("2019-03-01"),by='years'), col=rgb(0,1,0,alpha=0.9),lty=3)
