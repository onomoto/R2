op <- options(digits.secs = 6)
# Sys.time()
a <- Sys.time()
counter <- 1
limit <- 5
Events <- c()
for( i in seq(2,length(index(SP5)),1)){
  if(SP5[,4][i] > as.vector(SP5[,4][i-1])){
    counter <- counter +1
  }else{
      if(counter >= limit){
        # cat(counter)
        # cat(" day at ")
        # cat(as.character(index(SP5[,4][i-1])))
        # cat("\n")
        Events <<- append(Events,as.xts(counter,index(SP5[,4][i-1])))
      }
      counter <- 0
  }
}
cat(counter)
# cat(" ends at ")
# cat(as.character(index(SP5[,4][i-1])))
# when loop ends check countr and output if necessary.
if(counter >= limit){
  # cat(counter)
  # cat(" day at ")
  # cat(as.character(index(SP5[,4][i-1])))
  # cat("\n")
  Events <<- append(Events,as.xts(counter,index(SP5[,4][i])))
}
# Sys.time()
Events
b <- Sys.time()
b - a
op <- options(digits.secs = 2)
