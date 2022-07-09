# source('~/R_proj/R/index/getsp5.r', echo=TRUE)
# source('~/R_proj/R/index/nikkei_gspc_jpy.r', echo=TRUE)
library(beepr)
source(paste(getwd(),"index/getsp5.r",sep='/'),echo=TRUE)
source(paste(getwd(),"index/nikkei_gspc_jpy.r",sep='/'),echo=TRUE)
COV <- apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean)

getSymbols("^NDX",src="yahoo",auto.assign=TRUE)
getSymbols("^TNX",src="yahoo",auto.assign=TRUE)
TNX <- TNX[!is.na(TNX[,4])]  # remove NA entry.
getSymbols("^DJI",src="yahoo",auto.assign=TRUE)
# plot(merge(tmp.predict[,c(4,6,7)],rep(as.vector(rep(last(SP5[,4]),length(index(tmp.predict))))))[,c(1,2,3)],type='p',pch='+')
# lines(merge(tmp.predict[,c(4,6,7)],rep(as.vector(rep(last(SP5[,4]),length(index(tmp.predict))))))[,4],col=4)
GSPC <- append(GSPC,cbind(SP5["::2006"][,-4],SP5["::2006"][,4]))
getSymbols("^VIX",src="yahoo",auto.assign=TRUE)
getSymbols("BTC-USD",src="yahoo",auto.assign=TRUE)
BTC <- `BTC-USD`
getSymbols("DX-Y.NYB",src="yahoo",auto.assign=TRUE); DXY <- `DX-Y.NYB`; DXY <- DXY[!is.na(DXY[,4]),] # dollar index

if(index(tmp.predict[length(index(tmp.predict)),c(1,2,3,4,5)]) == index(last(to.monthly(SP5))[,c(1,2,3,4,5)])){
  tmp.predict[length(index(tmp.predict)),c(1,2,3,4,5)] <- last(to.monthly(SP5))[,c(1,2,3,4,5)]
}else{
  print("A new month  has come. Please update!!!")
}