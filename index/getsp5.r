func <- function(){
# csv file below should be daily basis. otherwise it may have a wrong alingment after merged w/ GSPC.
#
# work <- as.xts(read.zoo(read.csv("~/git/R/index/CSV/SP5.csv")))
  if(file.exists("~/git/R/index/CSV/SP5.csv")){
    work <- as.xts(read.zoo(read.csv("~/git/R/index/CSV/SP5.csv")))
  }else{
    print("!!!FILE SP5.csv DOESN'T EXIST!!!!!")
  }
#
# skip unncessary download. recoginze Saturdy, Sunday and other days, but not bank holidays.
#
  if(my_update_check(GSPC,Sys.Date()) == "S"){
    print("SKIP")
  }
  if(my_update_check(GSPC,Sys.Date()) == "D"){
    print("DOWNLOAD")
    getSymbols("^GSPC",auto.assign=TRUE)
  }

# coverting "GSPC" here is a bad idea. caused wrong alingment between years/weeks/months.
# SP5 <<- append(work["::2006"],merge(to.weekly(GSPC)[,c(1,2,3,4,6)],to.weekly(GSPC)[,5]))
# append daily to daily to generate whole record.
#
  SP5 <<- append(work["::2006"],merge(GSPC[,c(1,2,3,4,6)],GSPC[,5]))

}
func()
