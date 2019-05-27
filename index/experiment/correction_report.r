#
# 1.run "find_correction.r" before this file as its output beccomes the source of
# information.
# 2. print out report to contain a) start date of correction b) how many days lasted
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
  for(i in seq(2,length(index(correction_data)),1)) {
    len <- correction_data[i,2]

    if(day_num != as.vector(correction_data[i,2]) -1 || i == length(index(correction_data))-1){
      cat(as.character(as.Date(start_date)))
      cat(" bottom is ")
      cat(round(100*depth_ratio,digits=2))
      cat("% and last for ")
      cat(day_count)
      cat(" days oecd cli delta = ")
      cat(round(diff(cli_xts$oecd)[substr(start_date,1,7)],digits=2))
      cat(" \n")
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
}
func(correction_data)
