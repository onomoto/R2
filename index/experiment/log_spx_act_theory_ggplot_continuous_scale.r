#
#
# 1)spx log scale since 1985
# 2)stand for CLI 5 month delta by the point color.
# 3)clidelta is categorized in to 6 group. they are labeled as "a","b","c","d","e" and NA.
# 4)the number of groups is FIXED.
# 5)thresholds between them are automatically calculated as to equalize the number of samples in each group.
#


func <- function(z,a,b,c,d,e){
  w <- watermark
  x <- z
  if(is.na(x)){return(NA)}
  if(x > a){return("a")}
  if(x > b){return("b")}
  if(x > c){return("c")}
  if(x > d){return("d")}
  if(x >= e){return("e")}
  if(x < e){return("f")}
  return(x)
}

spx_mean <- apply.monthly(SP5[,4],mean)["1985::"]
lag <- 5
delta <- append(as.vector(diff(cli_xts$oecd,lag)[paste(as.Date(head(index(spx_mean),1)),"::",sep="")]),rep(NA,length(index(spx_mean)) - length(diff(cli_xts$oecd,lag)[paste(as.Date(head(index(spx_mean),1)),"::",sep="")])))
# calculate threshold. output 5 thresholds for 6 groups. store into watermark
# use mapply shown in the sample when data.frame() is done.
watermark <- sort(delta,decreasing = T)[floor(length(na.omit(delta))/7)*seq(2,6,1)]
# mapply(func,delta,watermark[1],watermark[2],watermark[3],watermark[4],watermark[5] )


df <- data.frame(
  i=log(as.vector(spx_mean)),
  c=as.vector(mapply(func,delta,watermark[1],watermark[2],watermark[3],watermark[4],watermark[5])),
  t=as.Date(index(spx_mean)))

colnames(df)[1] <- 'i'
colnames(df)[2] <- 'clidelta'

p <- ggplot(df,aes(x=t))
#
# need to investigate mapping= designator. somehow it's necessary to overlay y-axis coordinated graph.
#
# p <- p + geom_bar(aes(y=r),stat = "identity",fill='pink',colour="black") # need identity to draw value itself.
# p <- p + geom_point(aes(y=i),stat="identity", position="identity",colour="green",size=0.8)
p <- p + geom_path(aes(y=i,color=clidelta),stat="identity", position="identity",colour="black",linetype="solid")
p <- p + geom_point(aes(y=i,color=clidelta),stat="identity", position="identity",size=2)
p <- p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
s <- seq((1+floor(min(spx_mean)/500))*500,floor(max(spx_mean/500))*500,500)
p <- p + geom_hline(yintercept = log(s),size=0.4,linetype=1,colour="white",alpha=0.5) #horizontal line
p <- p + geom_hline(yintercept = log(s+250),size=0.4,linetype=2,colour="white",alpha=0.5) #horizontal line
p <- p + geom_vline(xintercept=seq(as.Date(paste(substr(index(head(spx_mean,1)),1,7),"-01",sep="")),as.Date("2019-01-01"),by='years'), colour="white",size=0.4,alpha=0.5)
for( i in s){ p <- p+annotate("text",label=as.character(s),x=as.Date(paste(substr(index(head(spx_mean,1)),1,7),"-01",sep=""))
, y=log(s*1.03),colour='white')}
# invalidate x-axis title text and ticks.
p <- p+theme( rect = element_rect(fill = "lightgoldenrodyellow", colour = "white",
                                  size = 0, linetype = 1),
              panel.background = element_rect(fill = "grey40",
                                              colour = "lightblue"),
                                              # size = 0.5, linetype = "solid"),
              panel.grid = element_blank(),
              axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
#
p <- p + labs(title = "SPX on log scale + CLI 5 mon delta by point colors",fill="",color="")
# p <- p + scale_color_continuous(low = "black" , high = "yellow1", na.value = "red")
# p <- p + scale_color_brewer(palette="Spectral")
# p <- p + scale_color_brewer(palette="PiYG")
p <- p +scale_color_brewer(palette="Spectral",na.value = "grey10",name = "CLI Delta", labels = c(as.character(round(watermark,digits=2)),"Less than The Last","NA"))
# p <- p + theme(legend.position = 'none')  # erase legend
p <- p + theme(legend.key=element_rect(fill="grey40"))

plot(p)
remove(s)
remove(df)
remove(watermark)
remove(delta)
remove(spx_mean)
