historical_high <- c()
historical_high <- as.vector(SP5[,4][1])
historical_high_breakthrough_record <- c()
breakback_record <- c()
period <- "1970-01-01::2019-10-30"

for(i in seq(2,length(index(SP5[period])),1)) {
  # historical_high <- as.vector(SP5[period][,4][1])
  if(SP5[period][,4][i] > as.vector(last(historical_high))) {
    # cat("high!")
    historical_high <-  append(historical_high,as.vector(SP5[period][,4][i]))
    breakback_record <- append(breakback_record,1)
    # cat("high2!")
    historical_high_breakthrough_record <- append(historical_high_breakthrough_record,as.xts(as.vector(diff(cli_xts$oecd)[substr(index(SP5[period][,4][i]),1,7)]),index(SP5[period][i])))
    # cat("high3!")
    if((i %% 250) == 0){
      cat(".")
      cat("")
    }
    # cat("high4!")
  }else{
    
    if(SP5[period][,4][i] < 0.95*as.vector(last(historical_high))){
      breakback_record[length(breakback_record)] <- 0
      # cat(SP5[period][,4][i])
    }
      
    # cat("low1!")
    historical_high <-  append(historical_high,last(historical_high))
    if((i %% 250) == 0){
      cat("@")
      cat("")
    }
  }
}
merge(historical_high_breakthrough_record,breakback_record)
