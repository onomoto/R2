#
# https://00819.blogspot.com/2018/02/calculate-nikkei225-vol3.html
#
# getSymbols("^GSPC",auto.assign=TRUE)
## test code to handle weekdays starts
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
## test code to handle weekdays ends
Sys.setlocale("LC_ALL",'ja_JP')
k3 <- paste("2007-01-01", index(last(GSPC)),sep="::")
k3
# download other data
getSymbols("NIKKEI225",src="FRED",auto.assign=TRUE) # download nikkei 225
# getSymbols("DEXJPUS", src = "FRED")
# getSymbols("YJUSDJPY",src="yahooj")
getSymbols('YJUSDJPY', src="yahooj",auto.assign=TRUE)
N225 <- NIKKEI225
# result <- summary(lm(to.monthly(N225[k3])[,4] ~  to.monthly(GSPC[k3])[,4] + to.monthly(DEXJPUS[k3])[,4]))
result_nikkei <- lm(to.monthly(N225[k3])[,4] ~  to.monthly(GSPC[k3])[,4] + to.monthly(YJUSDJPY[k3])[,4])
result_nikkei$coefficients[2]*last(GSPC)[,4]+result_nikkei$coefficients[3]*as.vector(last(YJUSDJPY)[,4])+result_nikkei$coefficients[1]
# plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)))
beep(2)
plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)),main=paste(paste("NIKKEI225 =",round(result_nikkei$coefficients[2],4),"* GSPC +",round(result_nikkei$coefficients[3],2),"*USDJPY +",round(result_nikkei$coefficients[1],2))))

tmp.legend <- paste("R Squared is ",round(summary(result_nikkei)$r.squared,4)," \n","DF is ",round(summary(result_nikkei)$df[2],0),sep=' ')

addLegend(legend.loc = "topleft", legend.names = tmp.legend,col=3)
