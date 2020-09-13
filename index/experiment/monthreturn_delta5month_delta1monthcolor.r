# 0) Dropbox/R-script/monthreturn_delta5month_delta1monthcolor.r 参照。
# 1)Y軸はCLI1ヶ月変化値、X軸はCLI5ヶ月変化値、同じく月間騰落率を色と形で表す。
# 2)原点からの角度をX、月間騰落率をYに取ったグラフ。
# 3)同じく、2)原点からの角度をX、COVをYに取ったグラフ。

w <- (to.monthly(SP5)[,4]/to.monthly(SP5)[,1])["1970::2018"]
c <- (apply.monthly(SP5[,4],sd) / apply.monthly(SP5[,4],mean) )["1970::2018"]
w <- w-1
# w <- (apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean))["1970::2018"]
d <- na.omit(diff(cli_xts$oecd,5))["1970::2018"]
func <- function(x){
  if(x > 0.1){return("a")}
  if(x > 0.025){return("b")}
  if(x > 0){return("c")}
  if(x > -0.025){return("d")}
  if(x > -0.05){return("e")}
  if(x < -0.05){return("f")}
}
# df <- data.frame(monthlyreturn=as.vector(w),delta=as.vector(d),sign=as.vector(apply(diff(cli_xts$oecd)["1970::2018"],1,func)))

df <- data.frame(monthlyreturn=as.vector(apply(w,1,func)),
          delta=as.vector(d),sign=as.vector(diff(cli_xts$oecd)["1970::2018"]))
  # as.vector(w),delta=as.vector(d),sign=as.vector(apply(diff(cli_xts$oecd)["1970::2018"],1,func)))
p <- ggplot(df, aes(x=delta,y=sign))
p <- p + theme(panel.background = element_rect(fill = "black",
                                               colour = "lightblue"),
               legend.key = element_rect(fill='black',colour='white'))
               # legend.box.background=element_rect(fill='black',colour='lightblue'))

p <- p + geom_point(alpha=1,aes(color=monthlyreturn,shape=monthlyreturn))
p <- p + geom_vline(xintercept =as.vector(last(diff(cli_xts$oecd,5))),size=0.5,linetype=2,colour="white",alpha=0.5)
p <- p + geom_hline(yintercept =as.vector(last(diff(cli_xts$oecd))),size=0.5,linetype=2,colour="white",alpha=0.5)
p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
# p <- p + geom_smooth(method = "auto")
# p <- p + scale_shape_manual(values=c(0,1,2,10,11,12))
# p <- p + scale_color_brewer(label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.1","less than -0.1"))  #x-axis label
p <- p + scale_color_brewer(palette="Spectral", label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.05","less than -0.05"))  #x-axis label
p <- p + scale_shape_manual(values=c(0,1,2,10,11,12),label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.05","less than -0.05"))
p <- p + stat_smooth(aes(x=delta,y=sign),method="loess",color='white',size=0.3,se=FALSE)
# p <- p + guides(shape = FALSE)
plot(p)




wdf <- data.frame(dist=sqrt(df$delta**2+df$sign**2),
                  angle=atan2(df$delta,df$sign),
                  return=w,
                  mon=seq(1,length(df[,1]),1),
                  cov=c)
colnames(wdf)[3] <- "return"
# wdf <- cbind(wdf,seq(1,length(wdf[,1]),1))
colnames(wdf) [4] <- "mon"
# wdf <- cbind(wdf,c)
# wdf
colnames(wdf)[5] <- "cov"


df <- data.frame(r<-wdf[,3],a<-wdf[,2],mon<-wdf[,4],cov <- wdf[,5])
colnames(df)[1] <- 'r'
colnames(df)[2] <- 'a'
colnames(df)[3] <- 'mon'
colnames(df)[4] <- 'c'

# df <- last(df,284)
p <- ggplot(df,aes(x=a,y=r))
p <- p + geom_point(alpha=1,aes(color=mon))
p <- p + geom_vline(xintercept =as.vector(atan2(last(diff(cli_xts$oecd,5),1),last(diff(cli_xts$oecd),1))),size=0.5,linetype=2,colour="red",alpha=0.5)
p <- p + geom_vline(xintercept =as.vector(seq(-0.9,0.9,0.1))*pi,size=0.5,linetype=2,colour="white",alpha=0.5)
# p <- p + scale_color_brewer(palette="Spectral")
p <- p + stat_smooth(aes(x=a,y=r),method="loess",color='white',size=0.3,se=FALSE)
p <- p + scale_color_gradient(low = "red", high = "green")
p <- p + theme(panel.background = element_rect(fill = "black",
                                              colour = "lightblue"))
             # panel.grid = element_blank())
plot(p)

p <- ggplot(df,aes(x=a,y=c))
p <- p + geom_point(alpha=1,aes(color=mon))
p <- p + geom_vline(xintercept =as.vector(atan2(last(diff(cli_xts$oecd,5),1),last(diff(cli_xts$oecd),1))),size=0.5,linetype=2,colour="red",alpha=0.5)
p <- p + geom_vline(xintercept =as.vector(seq(-0.9,0.9,0.1))*pi,size=0.5,linetype=2,colour="white",alpha=0.5)
# p <- p + scale_color_brewer(palette="Spectral")
p <- p + stat_smooth(aes(x=a,y=c),method="loess",color='white',size=0.3,se=FALSE)
p <- p + scale_color_gradient(low = "red", high = "green")
p <- p + theme(panel.background = element_rect(fill = "black",
                                              colour = "lightblue"))
             # panel.grid = element_blank())
plot(p)
