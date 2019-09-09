

# plot(merge(as.xts(predict(result_nikkei),index(residuals(result_nikkei))),to.monthly(N225[k3])[,4],residuals(result_nikkei)),main=paste(paste("NIKKEI225 =",round(result_nikkei$coefficients[2],4),"* SP5 +",round(result_nikkei$coefficients[3],2),"*USDJPY +",round(result_nikkei$coefficients[1],2))))
# tmp.legend <- paste("R Squared is ",round(summary(result_nikkei)$r.squared,4)," \n","DF is ",round(summary(result_nikkei)$df[2],0),sep=' ')
# addLegend(legend.loc = "topleft", legend.names = tmp.legend,col=3)
# remove(tmp.legend)


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
  getSymbols("YJUSDJPY",auto.assign=TRUE,src="yahooj")
}

# if(my_update_check(N225,Sys.Date()) == "S"){
#   print("SKIP")
# }
if(my_update_check(N225,Sys.Date()) == "D"){
  print("DOWNLOAD")
  getSymbols("NIKKEI225",src="FRED")
  # don't use auto.assign here.
  # getSymbols("N225",auto.assign=TRUE,src="FRED")
}

k3 <- paste("2007-01-01", index(last(SP5)),sep="::")

df <- data.frame(
  i=as.vector(to.monthly(N225[k3])[,4]),
  g=as.vector(predict(result_nikkei)),
  r=as.vector(residuals(result_nikkei)[,1]),
  t=as.Date(index(residuals(result_nikkei)[,1]))
)

colnames(df)[1] <- 'i'
colnames(df)[2] <- 'g'
colnames(df)[3] <- 'r'
colnames(df)[4] <- 't'

output.label <- paste("N225 = ",round(result_nikkei$coefficients[2],digits=2)," * SPX + ",round(result_nikkei$coefficients[3],digits=2)," * USDJPY + ",round(result_nikkei$coefficients[1],digits=2),"\n R Squared is ",round(summary(result_nikkei)$r.squared,4)," \n","DF is ",round(summary(result_nikkei)$df[2],0),sep=' ')
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
p <- p+annotate("text",label=output.label,x=as.Date("2010-01-01"), y=max(as.vector(to.monthly(N225[k3])[,4]))*0.9)

plot(p)
# remove unnecessary function.
output.label
remove(output.label)
remove(df)

beep(2)
