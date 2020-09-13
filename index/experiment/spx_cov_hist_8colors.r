
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


# spx_mean <- apply.monthly(SP5[,4],mean)["1970::"]
cov <- apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean)["1970::"]
cov <- as.xts(as.vector(cov),seq(as.Date("1970-01-01"),as.Date("2999-01-01"),by='months')[1:length(cov)])
lag <- 5
delta <- append(as.vector(diff(cli_xts$oecd,lag)[paste(as.Date(head(index(cov),1)),"::",sep="")]),rep(NA,length(index(cov)) - length(diff(cli_xts$oecd,lag)[paste(as.Date(head(index(cov),1)),"::",sep="")])))
# calculate threshold. output 7 thresholds for 8 groups. store into watermark
# use mapply shown in the sample when data.frame() is done.
# when number of colors =8 floor(length(na.omit(delta))/##9##)*seq(2,##8##,1)
watermark <- sort(delta,decreasing = T)[floor(length(na.omit(delta))/9)*seq(2,8,1)]
# mapply(func,delta,watermark[1],watermark[2],watermark[3],watermark[4],watermark[5] )
df <- data.frame(
  i=as.vector(cov),
  # when number of colors =8 watermark[1] till watermark[7] are used.
  c=as.vector(mapply(func,delta,watermark[1],watermark[2],watermark[3],watermark[4],watermark[5],watermark[6],watermark[7])),
  t=as.Date(index(cov)))



p <- ggplot(df, aes(x=i,fill=factor(c)))
# p <- p + geom_histogram(bins=80,position = "stack", alpha = 0.9)
p <- p + geom_histogram(bins=160,position = "stack", alpha = 0.8)
# p <- p +scale_fill_brewer(na.value = "grey50",name = "CLI Delta", labels = c(as.character(round(watermark,digits=2)),"Less than above","NA"))
legendlable <- c(paste("more than ",as.character(round(watermark,digits=2)),sep=""),"Less than above","NA")
legendlable
p <- p +scale_fill_brewer(palette="Spectral",na.value = "grey50",name = "CLI Delta", labels = legendlable)
p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
p <- p + theme(panel.background = element_rect(fill = "black",
                                               colour = "lightblue"))
               # panel.grid = element_blank())
# p <- p +scale_fill_brewer(palette="Spectral",na.value = "grey50",name = "CLI Delta", labels = c(as.character(round(watermark,digits=2)),"Less than above","NA"))

plot(p)
# remove(s)
# remove(df)
remove(watermark)
remove(delta)
remove(cov)
