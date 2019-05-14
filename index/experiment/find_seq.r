op <- options(digits.secs = 6)
Sys.time()
a <- Sys.time()
counter <- 1

limit <- 4
for( i in seq(2,length(index(SP5)),1)){
  if(SP5[,4][i] < as.vector(SP5[,4][i-1])){
    counter <- counter +1
  }else{
      if(counter >= limit){
        cat(counter)
        cat(" day at ")
        cat(as.character(index(SP5[,4][i-1])))
        cat("\n")
      }
      counter <- 0
  }
}

Sys.time()
b <- Sys.time()
b - a
op <- options(digits.secs = 2)
