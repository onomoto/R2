
.Last <- function() {
  # save.image(file=paste(getwd(),Sys.time(),sep="/"))
  #
  # save working image in the parent directory. expected the length of the directory name is fixed.
  # adjust value "len" to fit the length of the result "getwd()".
  #
  start <- 1
  # len <- 23
  # change to decide based upon strings length.
  len <- nchar(as.character(getwd())) -2
  save.image(file=gsub(" ","-",paste(substr(getwd(),start,len),Sys.time(),sep="/")))
  # 
  # OR store working image at the working directory
  #
  # save.image(file=gsub(" ","-",paste(getwd(),Sys.time(),sep="/")))
  #
  cat("bye bye...\n")
}