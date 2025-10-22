#
# https://00819.blogspot.com/2018/02/calculate-nikkei225-vol3.html
#

if(my_update_check(YJUSDJPY,Sys.Date()) == "D"){
  print("DOWNLOAD")
  # getSymbols("YJUSDJPY",auto.assign=TRUE,src="yahooj")
  getSymbols("JPY=X",auto.assign=TRUE)
  YJUSDJPY <- `JPY=X`
}

if(my_update_check(N225,Sys.Date()) == "D"){
  print("DOWNLOAD")
  getSymbols("NIKKEI225",src="FRED",auto.assign=TRUE)
  # don't use auto.assign here.
  # getSymbols("N225",auto.assign=TRUE,src="FRED")
}


START_DATE <- "2007-01-01"

k3 <- paste(START_DATE, index(last(SP5)),sep="::")
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

beepr::beep(2)

if(my_update_check(YJUSDJPY,Sys.Date()) == "D"){
  print("DOWNLOAD")
  # getSymbols("YJUSDJPY",auto.assign=TRUE,src="yahooj")
  getSymbols("JPY=X",auto.assign = FALSE,from ="2007-01-01")
  `JPY=X` -> YJUSDJPY
}


if(my_update_check(N225,Sys.Date()) == "D"){
  print("DOWNLOAD")
  getSymbols("NIKKEI225",src="FRED")
  # don't use auto.assign here.
  # getSymbols("N225",auto.assign=TRUE,src="FRED")
}

k3 <- paste(START_DATE, index(last(SP5)),sep="::")



df <- data.frame(
  i=as.vector(apply.monthly(na.omit(N225[k3]),mean)),
  # i=as.vector(to.monthly(N225[k3])[,4]),
  g=as.vector(predict(result_nikkei)),
  r=as.vector(residuals(result_nikkei)),
  # r=as.vector(residuals(result_nikkei)[,1]),
  t=as.Date(index(apply.monthly(na.omit(N225[k3]),mean)))
  # t=as.Date(index(residuals(result_nikkei)[,1]))
)

colnames(df)[1] <- 'i'
colnames(df)[2] <- 'g'
colnames(df)[3] <- 'r'
colnames(df)[4] <- 't'

# standardize date in df$t to the last day of each month.
df$t[length(df$t)] <- last(lubridate::ceiling_date(df$t,'month'))-1

output.label <- paste("N225 = ",round(result_nikkei$coefficients[2],digits=2)," * SPX + ",round(result_nikkei$coefficients[3],digits=2)," * USDJPY + ",round(result_nikkei$coefficients[1],digits=2),"\nR Squared is ",round(summary(result_nikkei)$r.squared,4)," \nDF is ",round(summary(result_nikkei)$df[2],0),sep=' ')
# addLegend(legend.loc = "topleft", legend.names = tmp.legend,col=3)

p <- ggplot(df,aes(x=t))
#
# need to investigate mapping= designator. somehow it's necessary to overlay y-axis coordinated graph.
#
p <- p + geom_bar(aes(y=r),stat = "identity",fill='pink',colour="black") # need identity to draw value itself.
p <- p + geom_point(aes(y=i),stat="identity", position="identity",colour="green",size=0.8)
p <- p + geom_path(aes(y=i),stat="identity", position="identity",colour="black",linetype="dotted")
p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
# same as above about mapping=
p <- p + geom_path(aes(y=g),colour='red')
# p <- p + geom_path(aes(y=e),colour='blue')
# p <- p+annotate("text",x=-Inf,y=Inf,label=output.label,hjust=-.2,vjust=2)
p <- p + xlab("") + ylab("")
# p <- p + geom_point(aes(y=case_per_capita,size=sign,color=r),alpha=1)
# p <- p+annotate("text",label=pref_db[,1],x=df[,2], y=df[,1]+0.1,colour='black',family = "HiraKakuProN-W3",size=3)
p <- p + theme_gray (base_family = "HiraKakuPro-W3")
p <- p+annotate("text",label=output.label,x=as.Date(START_DATE), y=max(as.vector(to.monthly(N225[k3])[,4]))*0.9,hjust = 0)


plot(p)
# remove unnecessary function.
output.label
paste("nikkei theoretical is ",round(last(df$g),2))
remove(output.label)
remove(df)

beepr::beep(2)
