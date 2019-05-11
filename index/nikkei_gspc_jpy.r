#
# https://00819.blogspot.com/2018/02/calculate-nikkei225-vol3.html
#



if(my_update_check(GSPC,Sys.Date()) == "F"){
  print("SKIP")
}
if(my_update_check(GSPC,Sys.Date()) == "T"){
  print("DOWNLOAD")
  getSymbols("^GSPC",auto.assign=TRUE)
}

if(my_update_check(YJUSDJPY,Sys.Date()) == "F"){
  print("SKIP")
}
if(my_update_check(GSPC,Sys.Date()) == "T"){
  print("DOWNLOAD")
  getSymbols("YJUSDJPY",auto.assign=TRUE)
}

if(my_update_check(N225,Sys.Date()) == "F"){
  print("SKIP")
}
if(my_update_check(GSPC,Sys.Date()) == "T"){
  print("DOWNLOAD")
  getSymbols("N225",auto.assign=TRUE)
}



k3 <- paste("2007-01-01", index(last(GSPC)),sep="::")
k3
# download other data
# getSymbols("NIKKEI225",src="FRED",auto.assign=TRUE) # download nikkei 225
N225 <- NIKKEI225

result_nikkei <- lm(to.monthly(N225[k3])[,4] ~  to.monthly(GSPC[k3])[,4] + to.monthly(YJUSDJPY[k3])[,4])
result_nikkei$coefficients[2]*last(GSPC)[,4]+result_nikkei$coefficients[3]*as.vector(last(YJUSDJPY)[,4])+result_nikkei$coefficients[1]
# plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)))

plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)),main=paste(paste("NIKKEI225 =",round(result_nikkei$coefficients[2],4),"* GSPC +",round(result_nikkei$coefficients[3],2),"*USDJPY +",round(result_nikkei$coefficients[1],2))))
tmp.legend <- paste("R Squared is ",round(summary(result_nikkei)$r.squared,4)," \n","DF is ",round(summary(result_nikkei)$df[2],0),sep=' ')
addLegend(legend.loc = "topleft", legend.names = tmp.legend,col=3)

beep(2)