#
# https://00819.blogspot.com/2018/02/calculate-nikkei225-vol3.html
#



# if(my_update_check(GSPC,Sys.Date()) == "S"){
#   print("SKIP")
# }
# if(my_update_check(GSPC,Sys.Date()) == "D"){
#   print("DOWNLOAD")
#   getSymbols("^GSPC",auto.assign=TRUE)
# }

# if(my_update_check(YJUSDJPY,Sys.Date()) == "S"){
#   print("SKIP")
# }
if(my_update_check(YJUSDJPY,Sys.Date()) == "D"){
  print("DOWNLOAD")
  # getSymbols("YJUSDJPY",auto.assign=TRUE,src="yahooj")
  getSymbols("JPY=X",auto.assign=TRUE)
  YJUSDJPY <- `JPY=X`
}

# if(my_update_check(N225,Sys.Date()) == "S"){
#   print("SKIP")
# }
if(my_update_check(N225,Sys.Date()) == "D"){
  print("DOWNLOAD")
  getSymbols("NIKKEI225",src="FRED",auto.assign=TRUE)
  # don't use auto.assign here.
  # getSymbols("N225",auto.assign=TRUE,src="FRED")
}



k3 <- paste("2008-04-01", index(last(SP5)),sep="::")
k3
# download other data
# getSymbols("NIKKEI225",src="FRED",auto.assign=TRUE) # download nikkei 225
N225 <- NIKKEI225

cbind(apply.monthly(na.omit(N225[k3]),mean,na.rm=T), as.vector(apply.monthly(SP5[k3][,4],mean,na.rm=T)), as.vector(apply.monthly(na.omit(YJUSDJPY)[k3][,4],mean,na.rm=T) )) -> lmdf
head(lmdf)
colnames(lmdf) <- c('N225','SP5','USDJPY')
summary(lm(N225 ~ SP5 + USDJPY,lmdf))
result_nikkei <- (lm(N225 ~ SP5 + USDJPY,data.frame(lmdf)))
predict(result_nikkei,newdata=data.frame(SP5=as.vector(last(SP5[,4])),USDJPY=as.vector(last(YJUSDJPY[,4]))),interval='prediction',level=0.95)

# result_nikkei <- lm(apply.monthly(na.omit(N225[k3]),mean)  ~  apply.monthly(SP5[k3][,4],mean) + apply.monthly(na.omit(YJUSDJPY)[k3][,4],mean))
# result_nikkei <- lm(to.monthly(N225[k3])[,4] ~  to.monthly(SP5[k3])[,4] + to.monthly(YJUSDJPY[k3])[,4])
# result_nikkei$coefficients[2]*last(SP5)[,4]+result_nikkei$coefficients[3]*as.vector(last(YJUSDJPY)[,4])+result_nikkei$coefficients[1]
# plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)))

# plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)),main=paste(paste("NIKKEI225 =",round(result_nikkei$coefficients[2],4),"* SP5 +",round(result_nikkei$coefficients[3],2),"*USDJPY +",round(result_nikkei$coefficients[1],2))))
# tmp.legend <- paste("R Squared is ",round(summary(result_nikkei)$r.squared,4)," \n","DF is ",round(summary(result_nikkei)$df[2],0),sep=' ')
# addLegend(legend.loc = "topleft", legend.names = tmp.legend,col=3)
# remove(tmp.legend)

beepr::beep(2)
