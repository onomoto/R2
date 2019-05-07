func <- function(){
  work <- as.xts(read.zoo(read.csv("~/git/R/index/CSV/SP5.csv")))
  getSymbols("^GSPC",auto.assign=TRUE)
  SP5 <<- append(work["::2006"],merge(to.weekly(GSPC)[,c(1,2,3,4,6)],to.weekly(GSPC)[,5]))
}
func()
