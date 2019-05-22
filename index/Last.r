
.Last <- function() {
  # save.image(file=paste(getwd(),Sys.time(),sep="/"))
  #
  # save working image in the parent directory. expected the length of the directory name is fixed.
  # adjust value "len" to fit the length of the result "getwd()".
  #
  start_pos <- 1
  # len <- 23
  # change to decide based upon strings length.
  last_pos <- nchar(as.character(getwd())) -2
  save.image(file=gsub("[ :.]","-",paste(substr(getwd(),start_pos,last_pos),Sys.time(),sep="/")))
  #
  # OR store working image at the working directory
  #
  # save.image(file=gsub(" ","-",paste(getwd(),Sys.time(),sep="/")))
  #
  cat("bye bye...\n")
}
