#
# https://00819.blogspot.com/2018/02/calculate-nikkei225-vol3.html
#

## test code to handle weekdays starts
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


last_update <- last(index(YJUSDJP))
# last_update <- "2019-05-08"

if(wd == "Monday"){ 
  if(as.Date(last_update)+3 != today){
    getSymbols("YJUSDJPY",src="yahooj")
    print("monday  lazy friday")
    cat("the last update was")
    print(as.Date(last_update))
  }else{
    print("monday and working friday!")
  }
}else if(wd == "Sunday"){
  if(as.Date(last_update)+2 != today){
    getSymbols("YJUSDJPY",src="yahooj")
    print("sunday  lazy friday")
    cat("the last update was")
    print(as.Date(last_update))
  }else{
    print("sunday and working friday!")
  }
}else{
  if(as.Date(last_update)+1 != today){
    getSymbols("YJUSDJPY",src="yahooj")
    print("not monday, lazy yesterday")
    cat("the last update was")
    print(as.Date(last_update))
  }else{
    print("not monday, worked yesterday")
  }
}

Sys.setlocale("LC_ALL",'ja_JP')
## test code to handle weekdays ends



k3 <- paste("2007-01-01", index(last(GSPC)),sep="::")
k3
# download other data
getSymbols("NIKKEI225",src="FRED",auto.assign=TRUE) # download nikkei 225
N225 <- NIKKEI225

result_nikkei <- lm(to.monthly(N225[k3])[,4] ~  to.monthly(GSPC[k3])[,4] + to.monthly(YJUSDJPY[k3])[,4])
result_nikkei$coefficients[2]*last(GSPC)[,4]+result_nikkei$coefficients[3]*as.vector(last(YJUSDJPY)[,4])+result_nikkei$coefficients[1]
# plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)))
beep(2)
plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)),main=paste(paste("NIKKEI225 =",round(result_nikkei$coefficients[2],4),"* GSPC +",round(result_nikkei$coefficients[3],2),"*USDJPY +",round(result_nikkei$coefficients[1],2))))

tmp.legend <- paste("R Squared is ",round(summary(result_nikkei)$r.squared,4)," \n","DF is ",round(summary(result_nikkei)$df[2],0),sep=' ')

addLegend(legend.loc = "topleft", legend.names = tmp.legend,col=3)
