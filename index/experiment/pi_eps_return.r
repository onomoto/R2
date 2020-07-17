# グラフその1　プロット散布図
#    横軸= personal income四半期平均差分
#    縦軸= 四半期EPS差分
#    color = SPX quarterly return
#    size = SPX COV
# グラフその2　ヒストグラム
#    SPXリターン　頻度図　PIがプラスの場合とマイナスの場合

k <- "1990::2020-06"
v <- to.quarterly(SP5)[,4]/as.vector(lag(to.quarterly(SP5))[,4])-1
w <- merge(
# quarterlyReturn(GSPC)[k],
v[k],
as.vector( diff(apply.quarterly(PI,mean))[k] ),
as.vector(diff(eps_year_xts)[k]),
as.vector((apply.quarterly(SP5[,4],sd)/apply.quarterly(SP5[,4],mean))[k]),
suffixes = c("r","p","e","v")
)
colnames(w)[1] <- "r"
df <- data.frame(
  return=w$r,
  eps=w$e,
  pi=w$p,
  cov=w$v
)
p <- ggplot(df, aes(x=p,y=e,color=r,size=v))
p <- p + geom_point(alpha=1)
p <- p + scale_color_gradient(low = "red", high = "green",name="return")
p <- p + scale_size(name="COV")
plot(p)
func <- function(x){if(x > 0){return("p")}else{return("m")}}
df <- data.frame(
  return=w$r,
  sign=as.vector(apply(w$p,1,func))
)
p <- ggplot(df, aes(x=r,fill=sign))
p <- p + geom_histogram(bins=50,position = "identity", alpha = 0.6)
p <- p + scale_fill_brewer(name = "PI",palette="Accent",)
plot(p)
