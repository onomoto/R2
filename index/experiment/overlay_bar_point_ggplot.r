#
# practice 1) use ggplot 2) make a overlay graph.
# need to investigate mapping=
#
# func <- function(x){
#   if(is.na(x)){return("NA")}
#   if(x > 0.1){return("upper")}
#   if(x > 0){return("uppermiddle")}
#   if(x > -0.1){return("lowermiddle")}
#   if(x < -0.1){return("lower")}
# }
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

# df <- data.frame(d=as.vector(w["1995::2019-06"]),t=index(w["1995::2019-06"]),sign=as.vector(apply(diff(cli_xts$oecd)["1995::2019-06"],1,func)))
# convert geometory of data df$i to df$d.
posconv <- function(x,sl,sh,ml,mh){
  # (x-ml)/(mh-ml) = (r-sl)/(sh-sl)
  a <- x-ml
  b <- mh-ml
  d <- sh-sl
  r <- sl+d*(a/b)
  return(r)
}
kikan <- "1995::"
# this should be here
idx <- log(apply.monthly(SP5[,4],mean))/100
#
# use mapply shown in the sample when data.frame() is done.

w <- apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean)
delta <- append(as.vector(diff(cli_xts$oecd)[kikan]),rep(NA,length(index(w[kikan])) - length(diff(cli_xts$oecd)[kikan])))
watermark <- sort(delta,decreasing = T)[floor(length(na.omit(delta))/7)*seq(2,6,1)]
#
# in the case of line graph, x-axis vector should continuous. thus, "t=index(w["1995::2018"])" is right.
# don't put descrete data into x-axis.
mi <- posconv(as.vector(idx[kikan]),range(as.vector(w[kikan]))[1],range(as.vector(w[kikan]))[2],range(as.vector(idx[kikan]))[1],range(as.vector(idx[kikan]))[2])

df <- data.frame(i=mi,
  d=as.vector(w[kikan]),
  t=index(w[kikan]),
  # sign=as.vector(apply(matrix(delta,ncol=1),1,func)))
  sign=as.vector(mapply(func,delta,watermark[1],watermark[2],watermark[3],watermark[4],watermark[5])),
  t=as.Date(index(w[kikan])))
  # sign=as.vector(apply(diff(cli_xts$oecd)[kikan],1,func)))

p <- ggplot(df,aes(x=t,y=d))
#
# need to investigate mapping= designator. somehow it's necessary to overlay y-axis coordinated graph.
#
p <- p + geom_bar(mapping=aes(x=t,y=d,fill=sign),stat = "identity",alpha=0.84,width = 10) # need identity to draw value itself.
p <- p + geom_path(df,mapping=aes(x=t,y=i),stat="identity", position="identity",colour="black")
p <- p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
# same as above about mapping=
p <- p + geom_point(df,mapping=aes(x=t,y=i,color=sign),size=1)
p <- p+theme( rect = element_rect(fill = "grey77", colour = "black",
                                  size = 0, linetype = 1),
              panel.background = element_rect(fill = "grey77",
                                              colour = "lightblue"),
              # size = 0.5, linetype = "solid"),
              panel.grid = element_blank(),
              axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
# parameter for position and lable of v-lines.
vlabel <- seq(500,3250,500)
# convert raw values into the positon.
s <- posconv(log(vlabel)/100,range(df$d)[1],range(df$d)[2],
            range(as.vector(idx[kikan]))[1],range(as.vector(idx[kikan]))[2])

# s <- seq((1+floor(min(tmp.predict[,c(4,6,7)])/500))*500,floor(max(tmp.predict[,c(4,6,7)])/500)*500,500)
# output h-line
p <- p + geom_hline(yintercept = s,size=0.4,linetype=1,colour="white",alpha=0.4)  #horizontal line
# output lable of h-line
for( i in seq(1,length(vlabel),1)){ p <- p+annotate("text",label=as.character(vlabel[i]),x=as.Date("1995-01-01"), y=s[i]+0.002,colour='white');print(vlabel[i])};
# p <- p + geom_hline(yintercept = log(s+250),size=0.4,linetype=2,colour="white") #horizontal line
p <- p + geom_vline(xintercept=seq(as.Date("1996-01-15"),as.Date("2019-01-15"),by='years'), colour="white",size=0.6,alpha=0.4)
p <- p + labs(title = "VIX + SPX + CLI Delta",sign="",fill="")
p <- p +scale_color_brewer(palette="Spectral",na.value = "grey10",name = "CLI Delta", labels = c(as.character(round(watermark,digits=2)),"Less than The Last","NA"))
p <- p +scale_fill_brewer(palette="Spectral",na.value = "grey10",name = "CLI Delta", labels = c(as.character(round(watermark,digits=2)),"Less than The Last","NA"))
# p <- p + theme(legend.position = 'none')  # erase legend
p <- p + theme(legend.key=element_rect(fill="grey40"))


plot(p)
# remove unnecessary function.
remove(posconv)
remove(s)
remove(mi)
remove(w)
remove(idx)
remove(df)
