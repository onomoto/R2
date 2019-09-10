#
# practice 1) use ggplot 2) make a overlay graph.
# need to investigate mapping=
#
func <- function(x){
  if(x > 0.1){return("upper")}
  if(x > 0){return("uppermiddle")}
  if(x > -0.1){return("lowermiddle")}
  if(x < -0.1){return("lower")}
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
# this should be here
idx <- log(apply.monthly(SP5[,4],mean))/100
# 
w <- apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean)
#
# in the case of line graph, x-axis vector should continuous. thus, "t=index(w["1995::2018"])" is right.
# don't put descrete data into x-axis.
mi <- posconv(as.vector(idx["1995::2019-06"]),range(as.vector(w["1995::2019-06"]))[1],range(as.vector(w["1995::2019-06"]))[2],range(as.vector(idx["1995::2019-06"]))[1],range(as.vector(idx["1995::2019-06"]))[2])

df <- data.frame(i=mi,d=as.vector(w["1995::2019-06"]),t=index(w["1995::2019-06"]),sign=as.vector(apply(diff(cli_xts$oecd)["1995::2019-06"],1,func)))

p <- ggplot(df,aes(x=t,y=d))
#
# need to investigate mapping= designator. somehow it's necessary to overlay y-axis coordinated graph.
#
p <- p + geom_bar(mapping=aes(x=t,y=d,fill=sign),stat = "identity",alpha=0.84,width = 15) # need identity to draw value itself.
p <- p + geom_path(df,mapping=aes(x=t,y=i),stat="identity", position="identity",colour="black")
p <- p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
# same as above about mapping=
p <- p + geom_point(df,mapping=aes(x=t,y=i,color=sign),size=1)
p <- p+theme( rect = element_rect(fill = "white", colour = "black",
                                  size = 0, linetype = 1),
              panel.background = element_rect(fill = "lightgrey",
                                              colour = "lightblue"),
              # size = 0.5, linetype = "solid"),
              panel.grid = element_blank(),
              axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
# 
vlabel <- seq(500,3250,500)
s <- posconv(log(vlabel)/100,range(df$d)[1],range(df$d)[2],
            range(as.vector(idx["1995::2019-06"]))[1],range(as.vector(idx["1995::2019-06"]))[2])

# s <- seq((1+floor(min(tmp.predict[,c(4,6,7)])/500))*500,floor(max(tmp.predict[,c(4,6,7)])/500)*500,500)
p <- p + geom_hline(yintercept = s,size=0.4,linetype=1,colour="white") #horizontal line
j <- 1
for( i in vlabel){ p <- p+annotate("text",label=as.character(i),x=as.Date("1995-01-01"), y=s[j]*1.02);
j<-j+1}
# p <- p + geom_hline(yintercept = log(s+250),size=0.4,linetype=2,colour="white") #horizontal line
p <- p + geom_vline(xintercept=seq(as.Date("1996-01-01"),as.Date("2019-01-01"),by='years'), colour="white",size=0.4,alpha=0.4) 

plot(p)
# remove unnecessary function.
remove(posconv)
remove(s)
remove(j)
remove(mi)
remove(w)
remove(idx)
remove(df)
