#
# 1.run "find_correction.r" before this file as its output beccomes the source of
# information. "correction_date" is asssumed for the output. pls, refer line#51.
# 2. return xts object contains a) start date of correction b) how many days lasted
# c) the rate to decline most d) CLI 1 month delta at the start.
#
func <- function(d=output){
  correction_data <- d
  day_count <- 0
  day_num <- correction_data[1,2]
  depth_ratio <- as.vector(correction_data[1,1])
  len <- correction_data[i,2]
  day_num <- correction_data[1,2]
  start_date <- index(correction_data[1])
  output_xts <- c()
  day_length <- c()
  delta <- c()
  for(i in seq(2,length(index(correction_data)),1)) {
    len <- correction_data[i,2]
    # if find the gap or reached at the end.
    if(day_num != as.vector(correction_data[i,2]) -1 || i == length(index(correction_data))-1){
      # cat(as.character(as.Date(start_date)))
      # cat(" bottom is ")
      # cat(round(100*depth_ratio,digits=2))
      # cat("% and last for ")
      # cat(day_count)
      # cat(" days oecd cli delta = ")
      # cat(round(diff(cli_xts$oecd)[substr(start_date,1,7)],digits=2))
      # cat(" \n")
      output_xts <- append(output_xts,as.xts(depth_ratio,as.Date(start_date)))
      # cli oecd is not available before 1961, then diff may return "blank". for that case append "NA"  instead.
      if(length(diff(cli_xts$oecd)[substr(start_date,1,7)]) == 0){
        delta <- append(delta,NA)
      }else{
        delta <- append(delta,diff(cli_xts$oecd)[substr(start_date,1,7)])
      }
      day_length <- append(day_length,day_count)
      day_count <- 0
      depth_ratio <- 1
      start_date <- index(correction_data[i])
    }
    day_num <- correction_data[i,2]
    day_count <- day_count+1
    if(depth_ratio > as.vector(correction_data[i,1])){
      depth_ratio <- as.vector(correction_data[i,1])
    }
  }
  output_xts <- merge(output_xts,day_length)
  output_xts <- merge(output_xts,delta)
  return(output_xts)
}
func(correction_data)
