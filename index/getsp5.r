func <- function(){
  # csv file below should be daily basis. otherwise it may have a wrong alingment after merged w/ GSPC.
  #
  # work <- as.xts(read.zoo(read.csv("~/git/R/index/CSV/SP5.csv")))
  if(file.exists("~/git/R/index/CSV/SP5.csv")){
    work <- as.xts(read.zoo(read.csv("~/git/R/index/CSV/SP5.csv")))
  }else{
    print("!!!FILE SP5.csv DOESN'T EXIST!!!!!")
  }

  Sys.setlocale("LC_ALL",'en_US')
  today <- Sys.Date()
  # today <- as.Date("2019-05-10")
  wd <- weekdays(today)
  # wd <- weekdays(as.Date("2019-05-10"))
  last_update <- last(index(GSPC))
  # last_update <- "2019-05-08"

  if(wd == "Monday"){ 
    if(as.Date(last_update)+3 != today){
      getSymbols("^GSPC",auto.assign=TRUE)
      print("monday  lazy friday")
      cat("the last update was")
      print(as.Date(last_update))
    }else{
      print("monday and working friday!")
    }
  }else if(wd == "Sunday"){
    if(as.Date(last_update)+2 != today){
      getSymbols("^GSPC",auto.assign=TRUE)
      print("sunday  lazy friday")
      cat("the last update was")
      print(as.Date(last_update))
    }else{
      print("sunday and working friday!")
    }
  }else{
    if(as.Date(last_update)+1 != today){
      getSymbols("^GSPC",auto.assign=TRUE)
      print("not monday, lazy yesterday")
      cat("the last update was")
      print(as.Date(last_update))
    }else{
      print("not monday, worked yesterday")
    }
  }
  Sys.setlocale("LC_ALL",'ja_JP')

  # coverting "GSPC" here is a bad idea. caused wrong alingment between years/weeks/months.
  # SP5 <<- append(work["::2006"],merge(to.weekly(GSPC)[,c(1,2,3,4,6)],to.weekly(GSPC)[,5]))
  # append daily to daily to generate whole record.
  # 
  SP5 <<- append(work["::2006"],merge(GSPC[,c(1,2,3,4,6)],GSPC[,5]))
  
}
func()
