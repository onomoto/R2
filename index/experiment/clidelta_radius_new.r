# 0) Dropbox/R-script/monthreturn_delta5month_delta1monthcolor.r 参照。
# 1)Y軸はCLI1ヶ月変化値、X軸はCLI二次微分値、同じく月間騰落率を色と形で表す。
# 2)原点からの角度をX、月間騰落率をYに取ったグラフ。
# 3)同じく、2)原点からの角度をX、COVをYに取ったグラフ。
#
CLI <- cli_g20
k1970 <- paste("1970",substr(index(last(CLI)),1,7),sep="::")
my_palette <- colorRampPalette(c("#FF0000","#FFFF00","#00FF00","#00FFFF","#0000FF"))
plot_col <- my_palette(5)[5:1]

w <- (to.monthly(SP5)[,4]/to.monthly(SP5)[,1])[k1970] #2020
c <- (apply.monthly(SP5[,4],sd) / apply.monthly(SP5[,4],mean) )[k1970] #2020
w <- w-1
# w <- (apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean))["1970::2018"]
d <- na.omit(diff(CLI,1))[k1970] #2020
func <- function(x){
  if(x > 0.1){return("a")}
  if(x > 0.025){return("b")}
  if(x > 0){return("c")}
  if(x > -0.025){return("d")}
  if(x > -0.05){return("e")}
  if(x < -0.05){return("f")}
}
# df <- data.frame(monthlyreturn=as.vector(w),delta=as.vector(d),sign=as.vector(apply(diff(CLI)["1970::2018"],1,func)))

df <- data.frame(monthlyreturn=as.vector(apply(w,1,func)),
          delta=as.vector(d),sign=as.vector(diff(diff(CLI))[k1970])) #2020
  # as.vector(w),delta=as.vector(d),sign=as.vector(apply(diff(CLI)["1970::2018"],1,func)))
p <- ggplot(df, aes(x=delta,y=sign))
p <- p + theme(panel.background = element_rect(fill = "black",
                                               colour = "lightblue"),
               legend.key = element_rect(fill='black',colour='white'))
               # legend.box.background=element_rect(fill='black',colour='lightblue'))

p <- p + geom_point(alpha=1,aes(color=monthlyreturn,shape=monthlyreturn))
p <- p + geom_vline(xintercept =as.vector(last(diff(CLI))),size=0.5,linetype=2,colour="white",alpha=0.5)
p <- p + geom_hline(yintercept =as.vector(last(diff(diff(CLI)))),size=0.5,linetype=2,colour="white",alpha=0.5)
p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
# p <- p + geom_smooth(method = "auto")
# p <- p + scale_shape_manual(values=c(0,1,2,10,11,12))
# p <- p + scale_color_brewer(label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.1","less than -0.1"))  #x-axis label
p <- p + scale_color_brewer(palette="Spectral", label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.05","less than -0.05"))  #x-axis label
p <- p + scale_shape_manual(values=c(0,1,2,10,11,12),label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.05","less than -0.05"))
# p <- p + stat_smooth(aes(x=delta,y=sign),method="loess",color='white',size=0.3,se=FALSE)
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
# p <- p + geom_vline(xintercept =as.vector(atan2(last(diff(CLI),5), last(diff(diff(CLI)),5)) ),size=0.5,linetype=4,colour=RColorBrewer::brewer.pal(5,"Spectral")[5:1],alpha=0.9)

p <- p + geom_vline(xintercept =as.vector(atan2(last(diff(CLI),5), last(diff(diff(CLI)),5)) ),size=0.5,linetype=4,colour=plot_col,alpha=0.9)

# p <- p + geom_hline(yintercept =as.vector(last(df$r,5)),size=0.5,linetype=4,colour=RColorBrewer::brewer.pal(5,"Spectral")[5:1],alpha=0.9)

p <- p + geom_hline(yintercept =as.vector(last(df$r,5)),size=0.5,linetype=4,colour=plot_col,alpha=0.9)



p <- p + geom_vline(xintercept =as.vector(seq(-0.9,0.9,0.1))*pi,size=0.5,linetype=2,colour="white",alpha=0.5)
# p <- p + scale_color_brewer(palette="Spectral")
p <- p + stat_smooth(aes(x=a,y=r),method="loess",color='white',size=0.3,se=T)
p <- p + scale_color_gradient(low = "red", high = "green")
p <- p + theme(panel.background = element_rect(fill = "black",
                                              colour = "lightblue"))
             # panel.grid = element_blank())
plot(p)

p <- ggplot(df,aes(x=a,y=c))
p <- p + geom_point(alpha=1,aes(color=mon))

p <- p + geom_vline(xintercept =as.vector(atan2(last(diff(CLI),5), last(diff(diff(CLI)),5)) ),size=0.5,linetype=4,colour=plot_col,alpha=0.9)
# p <- p + geom_vline(xintercept =as.vector(atan2(last(diff(CLI),1),last(diff(diff(CLI)),1))),size=0.5,linetype=2,colour="red",alpha=0.5)
p <- p + geom_vline(xintercept =as.vector(seq(-0.9,0.9,0.1))*pi,size=0.5,linetype=2,colour="white",alpha=0.5)
# p <- p + scale_color_brewer(palette="Spectral")
p <- p + stat_smooth(aes(x=a,y=c),method="loess",color='white',size=0.3,se=FALSE)
p <- p + scale_color_gradient(low = "red", high = "green")
p <- p + theme(panel.background = element_rect(fill = "black",
                                              colour = "lightblue"))
             # panel.grid = element_blank())
plot(p)



inc <- 0.2*pi
deg <- seq(-1*pi,pi-inc, inc)
w <- c(); for(i in deg){w <- append(w,sd(wdf$return[wdf$angle > i & wdf$angle < (i+inc)]))}
plot( deg+inc*0.5 ,w,xlim=c(-1*pi,pi))
abline(v=0.5*pi)
abline(v=0)
abline(v=-0.5*pi)
s <- w
w <- c(); for(i in deg){w <- append(w,mean(wdf$return[wdf$angle > i & wdf$angle < (i+inc)]))}
plot( deg+inc*0.5 ,w,xlim=c(-1*pi,pi))
abline(v=-0.5*pi)
abline(v=0.5*pi)
abline(v=0*pi)
abline(h=0)
r <- w
pnorm(0,r,s)
w <- c(); for(i in deg){w <- append(w,length(wdf$return[wdf$angle > i & wdf$angle < (i+inc)]))}
n <- w
cbind(r,s,n,1 - pnorm(0,r,s))
plot( deg+inc*0.5  ,1 - pnorm(0,r,s)    ,xlim=c(-1*pi,pi))
abline(v=-0.5*pi)
abline(v=0.5*pi)
abline(v=0*pi)
abline(h=0.5)

v <- atan2(diff(CLI),diff(diff(CLI))) %>% last(.,240)
w <- monthlyReturn(GSPC)[paste0("::",substr(index(last(CLI)),1,7))] %>% last(.,240) # update if necessary.
par(bg="grey",fg="white")
plot.default(v,w,type='p')
tmp <- par('usr')
plot.new()
plot.default(v,w,xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),type='p')
abline(v=pi/2)
abline(v=-pi/2)
abline(h=0)
abline(v=0)
par(new=T)
i <- 12
plot.default(last(v,i),last(w,i),xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),type='b',pch='+',col='brown')
for(k in seq(1,9,1)){
  j <- k %% 8 + 1
  par(new=T)
  print(i)
  print(j)
  plot.default(last(v,i)[k],last(w,i)[k],xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),type='p',pch=as.character(k),col=j)
}

# 0) Dropbox/R-script/monthreturn_delta5month_delta1monthcolor.r 参照。
# 1)Y軸はCLI1ヶ月変化値、X軸はCLI二次微分値、同じく月間騰落率を色と形で表す。
# 2)原点からの角度をX、月間騰落率をYに取ったグラフ。
# 3)同じく、2)原点からの角度をX、COVをYに取ったグラフ。
#
k1970 <- paste("1970",substr(index(last(CLI)),1,7),sep="::")
my_palette <- colorRampPalette(c("#FF0000","#FFFF00","#00FF00","#00FFFF","#0000FF"))
plot_col <- my_palette(5)[5:1]

w <- (to.monthly(SP5)[,4]/to.monthly(SP5)[,1])[k1970] #2020
c <- (apply.monthly(SP5[,4],sd) / apply.monthly(SP5[,4],mean) )[k1970] #2020
w <- w-1
# w <- (apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean))["1970::2018"]
d <- na.omit(diff(CLI,1))[k1970] #2020
func <- function(x){
  if(x > 0.1){return("a")}
  if(x > 0.025){return("b")}
  if(x > 0){return("c")}
  if(x > -0.025){return("d")}
  if(x > -0.05){return("e")}
  if(x > -0.1){return("f")}
  if(x < -0.1){return("g")}
}
# df <- data.frame(monthlyreturn=as.vector(w),delta=as.vector(d),sign=as.vector(apply(diff(CLI)["1970::2018"],1,func)))

df <- data.frame(monthlyreturn=as.vector(apply(w,1,func)),
                 delta=as.vector(d),sign=as.vector(diff(diff(CLI))[k1970])) #2020
df <- df[seq(-602,-608,-1),] # remove 2020/3 and some months.
# df <- df[seq(-1*(dim(df)[1]-13) , -1*(dim(df)[1]-8) ,-1),]
# wdf <- df
# df <- df[-1*seq(597,602,1),]
# as.vector(w),delta=as.vector(d),sign=as.vector(apply(diff(CLI)["1970::2018"],1,func)))
p <- ggplot(df, aes(x=delta,y=sign))
p <- p + theme(panel.background = element_rect(fill = "black",
                                               colour = "lightblue"),
               legend.key = element_rect(fill='black',colour='white'))
# legend.box.background=element_rect(fill='black',colour='lightblue'))

p <- p + geom_point(alpha=1,aes(color=monthlyreturn,shape=monthlyreturn))
p <- p + geom_vline(xintercept =as.vector(last(diff(CLI))),size=0.5,linetype=2,colour="white",alpha=0.5)
p <- p + geom_hline(yintercept =as.vector(last(diff(diff(CLI)))),size=0.5,linetype=2,colour="white",alpha=0.5)
p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
# p <- p + geom_smooth(method = "auto")
# p <- p + scale_shape_manual(values=c(0,1,2,10,11,12))
# p <- p + scale_color_brewer(label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.1","less than -0.1"))  #x-axis label

p <- p + scale_color_brewer(palette="Spectral", label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.05","more than -0.1","less than -0.1"))  #x-axis label
p <- p + scale_shape_manual(values=c(0,1,2,10,11,12,14),label=c("more than 0.1","more than 0.025","more than ZERO","more then -0.025","more than -0.05","more than -0.1","less than -0.1"))
# p <- p + stat_smooth(aes(x=delta,y=sign),method="loess",color='white',size=0.3,se=FALSE)
# p <- p + guides(shape = FALSE)
plot(p)
df %>% last(.,6)
df %>% last(.,6) %>% dplyr::summarise(.,atan2(delta,sign))
