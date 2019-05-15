op <- options(digits.secs = 6)
Sys.time()
a <- Sys.time()
j <- 0
correction_ratio <- 0.9
hist_high <- c()
recent_high <- c() 
correction_flag <- 0
depth <- c()
# depth <- as.xts(0,0,0,as.Date("1950-01-01"))
depth_ratio <- 0
depth_new <- c()
depth_date <- as.Date("1950-01-01")
for( i in seq(2,length(index(SP5)),1)){
  if(SP5[,4][i] > as.vector(SP5[,4][i-1])){
    if(SP5[,4][i] > hist_high){
      hist_high <- hist_high
      # c(hist_high,as.xts(hist_high,as.vector(SP5[,4][i]),index(SP5[,4][i])))
    }
    # if today's price is higher than the previous day
    if(SP5[,4][i] > 1.1*as.vector(last(depth[,1]))) {
      # check if it is higher than historical high
      # if so update historical high record.
      recent_high <- as.vector(SP5[,4][i])
      correction_flag <- 0
      if(depth_ratio !=1){
        # cat("deepest is ")
        # cat(round(depth*100,digits=2))
        # cat(" % at the date of ")
        # cat(as.character(depth_date))
        print(last(depth))
        cat("\n ######## \n")
      }
      depth_ratio <- 1
      depth_new <- 1
    }

  }else{
    if(SP5[,4][i] < correction_ratio * recent_high){
      depth_new <- as.vector(SP5[,4][i] / recent_high)
      if(correction_flag == 0){

        cat(round(100-correction_ratio*100,digits=2))
        cat("% correction starts at ")
        cat(as.character(index(SP5[,4][i])))
        # print(as.Date(index(SP5[,4][i])))
        cat(" \n")
        correction_flag <- 1
      }
      # cat(depth)
      # cat(depth_new)
      if(depth_ratio >= depth_new){

          depth_ratio <- depth_new
          # depth_date <- index(SP5[,4][i])
          # depth_close <- as.vector(SP5[,4][i])
          if(length(index(depth)) == 1){
            print("1")
            depth <- as.xts(depth_new,as.vector(SP5[,4][i]),hist_high,index(SP5[,4][i]))
            print("2")
          }else{
            print("3")
            depth <- append(depth,as.xts(depth_new,as.vector(SP5[,4][i]),hist_high,index(SP5[,4][i])))  
            print("4")
          }

          # depth <- append(depth,as.xts(depth_new,as.vector(SP5[,4][i]),hist_high,index(SP5[,4][i])))

      }
    }
  }
}
Sys.time()
b <- Sys.time()
b - a
op <- options(digits.secs = 2)
