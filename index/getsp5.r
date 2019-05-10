func <- function(){
  # csv file below should be daily basis. otherwise it may have a wrong alingment after merged w/ GSPC.
  #
  work <- as.xts(read.zoo(read.csv("~/git/R/index/CSV/SP5.csv")))
  # getSymbols("^GSPC",auto.assign=TRUE)
  ## testcode to handle weekdays and locale starts here.
  Sys.setlocale("LC_ALL",'en_US')
  if(weekdays(Sys.Date()) != "Monday"){ 
    if(as.Date(last(index(GSPC)))+1 != Sys.Date()){
      getSymbols("^GSPC",auto.assign=TRUE)
      print("not monday")
      cat("the last update was")
      print(as.Date(last(index(GSPC))))
    }else{
      print("not monday but updated!")
    }
  }else{
        if(as.Date(last(index(GSPC)))+3 != Sys.Date()){
            getSymbols("^GSPC",auto.assign=TRUE)
            print("monday  lazy friday")
            cat("the last update was")
            print(as.Date(last(index(GSPC))))
        }else{
        print("monday and friday!")
        }
  }
  Sys.setlocale("LC_ALL",'ja_JP')
  ## testcode ends here.
  # coverting "GSPC" here is a bad idea. brought wrong alingment between years/weeks/months.
  # SP5 <<- append(work["::2006"],merge(to.weekly(GSPC)[,c(1,2,3,4,6)],to.weekly(GSPC)[,5]))
  # use daily.
  SP5 <<- append(work["::2006"],merge(GSPC[,c(1,2,3,4,6)],GSPC[,5]))
  
}
func()
