# source('~/R_proj/R/index/getsp5.r', echo=TRUE)
# source('~/R_proj/R/index/nikkei_gspc_jpy.r', echo=TRUE)

source(paste(getwd(),"index/getsp5.r",sep='/'),echo=TRUE)
source(paste(getwd(),"index/nikkei_gspc_jpy.r",sep='/'),echo=TRUE)
if(index(tmp.predict[length(index(tmp.predict)),c(1,2,3,4,5)]) == index(last(to.monthly(SP5))[,c(1,2,3,4,5)])){
    tmp.predict[length(index(tmp.predict)),c(1,2,3,4,5)] <- last(to.monthly(SP5))[,c(1,2,3,4,5)]
}else{
    print("A new month has come. Please update!!!")
}
plot(merge(tmp.predict[,c(4,6,7)],rep(as.vector(rep(last(SP5[,4]),length(index(tmp.predict))))))[,c(1,2,3)],type='p',pch='+')
lines(merge(tmp.predict[,c(4,6,7)],rep(as.vector(rep(last(SP5[,4]),length(index(tmp.predict))))))[,4],col=4)
