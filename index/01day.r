source('~/R_proj/R/index/getsp5.r', echo=TRUE)
source('~/R_proj/R/index/nikkei_gspc_jpy.r', echo=TRUE)
if(index(tmp.predict[length(index(tmp.predict)),c(1,2,3,4,5)]) == index(last(to.monthly(SP5))[,c(1,2,3,4,5)])){ 
    tmp.predict[length(index(tmp.predict)),c(1,2,3,4,5)] <- last(to.monthly(SP5))[,c(1,2,3,4,5)]
}else{
    print("A new month has come. Please update!!!")
}
