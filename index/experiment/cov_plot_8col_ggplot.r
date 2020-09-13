# 2018年末の指数の下落は大変厳しいものだった。過去と比較してどれくらい激しかったのかをグラフにしてみた。縦軸がSPXのCOV、
# 横軸がCLIの5ヶ月デルタ、ドットの色は同1ヶ月デルタ。線は縦軸と横軸の移動回帰。データは過去40年分。
# 直感的に昨年末の指数の変動は景気後退を伴わない下落としては1998/10以来の規模ではないかなと思っていたがデータにもほぼ裏
# 付けられた感じ。それにしても1987/10のanormally ぶりは目を見張る。

k1970 <- "1970::2020-07"
w <- (apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean))[k1970]
d <- na.omit(diff(cli_xts$oecd,5))[k1970]
func <- function(z,a,b,c,d,e,f,g){
  w <- watermark
  x <- z
  if(is.na(x)){return(NA)}
  if(x > a){return("a")}
  if(x > b){return("b")}
  if(x > c){return("c")}
  if(x > d){return("d")}
  if(x > e){return("e")}
  if(x > f){return("f")}
  if(x >= g){return("g")}
  if(x < g){return("h")}
  return(x)
}
watermark <-
sort(as.vector(diff(cli_xts$oecd)[k1970]),decreasing = T)[(floor(length(na.omit(as.vector(diff(cli_xts$oecd)[k1970]))))/9)*seq(2,8,1)]
df <-
data.frame(sd=as.vector(w),
delta=as.vector(d),
p=predict(loess(as.vector(w) ~ as.vector(d)))*2.28,
p2=predict(loess(as.vector(w) ~ as.vector(d)))+0.028,
sign=as.vector(mapply(func,diff(cli_xts$oecd)[k1970],watermark[1],watermark[2],watermark[3],watermark[4],watermark[5],watermark[6],watermark[7])))
p <- ggplot(df, aes(x=delta,y=sd))
p <- p + geom_point(alpha=0.95,aes(color=sign))
p <- p +scale_color_brewer(palette="Spectral",na.value = "grey50",name = "CLI Delta", labels = c(as.character(round(watermark,digits=2)),"Less than above","NA"))
p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
p <- p + theme(legend.key = element_rect(fill='grey50',colour='white'))
p <- p + geom_line(aes(y=p),size=0.3,linetype='dotted',color='white')
# p <- p + geom_line(aes(y=p2),size=0.3,linetype='dotted',color='white')
p <- p + theme(panel.background = element_rect(fill = "grey50",colour = "lightblue"))
p <- p + geom_hline(yintercept =as.vector(last(COV)),size=0.5,linetype=2,colour="white",alpha=0.5)
# p <- p + geom_vline(xintercept =as.vector(last(df$delta)),size=0.5,linetype=2,colour="red",alpha=0.5)
# p <- p + geom_hline(yintercept =as.vector(last(df$sd)),size=0.5,linetype=2,colour="red",alpha=0.5)
# p <- p + geom_vline(xintercept=as.vector(diff(cli_xts$oecd,5)["2018-12"]), colour="white",size=0.5,alpha=0.5,linetype=2)
p <- p + geom_smooth(aes(x=delta,y=sd),method="loess",size=0.3,se=FALSE,color='white')
# p <- p + xlim(0,0.2)
#
#  objects in postion should be placed accoding to time sequence.
#
# pick up dates as "YYYY-MM" when COV is more than 0.04
position <- substr(index(w[w > 0.03]),1,7)
position <- append(position,c("2018-12"))
position
# remove data of "1998-10" to avoid label conflict
# need to sort otherwise ggplot reshufle sequece itself.
# seq num will vary according to the criteria, adjust!
position <- sort(position)[-grep("1998-10",position)]
p <- p + annotate("text",label=as.character(position),x=as.vector(diff(cli_xts$oecd,5)[position]), y=as.vector(w[position])+0.002,colour='white',size = 3)
position <- "1998-10"
p <- p + annotate("text",label=as.character(position),x=as.vector(diff(cli_xts$oecd,5)[position]), y=as.vector(w[position])-0.002,colour='white',size = 3)
position <- "2018-12"
p <- p + annotate("text",label=as.character(position),x=as.vector(diff(cli_xts$oecd,5)[position]), y=as.vector(w[position])+0.002,colour='red',size = 3)
position <-  substr(k1970,7,15)
# position <- substr(seq(as.Date("2019-01-01"),as.Date("2019-07-01"),by='months'),1,7)
p <- p + annotate("text",label=as.character(position),x=as.vector(diff(cli_xts$oecd,5)[position]), y=as.vector(w[position])+0.002,colour='red',size = 3)
# p <- p + annotate("text",label=as.character("*"),x=as.vector(diff(cli_xts$oecd,5)[position]), y=as.vector(COV[position])-0.0004,colour='red',size=7)
#
# choose above or below
# p <- p + geom_smooth(aes(y=delta,x=sd),method="loess",size=0.3,se=FALSE)
#
p <- p + stat_smooth(aes(x=delta,y=sd),method="loess",color='white',size=0.3,se=F,level=0.8)
plot(p)
