#
# practice 1) use ggplot 2) make a overlay graph.
# need to investigate mapping= 
#
# this function() must be here as the name func() is reused latter.
func <- function(x,sl,sh,ml,mh){
  # (x-ml)/(mh-ml) = (r-sl)/(sh-sl)
  a <- x-ml
  b <- mh-ml
  d <- sh-sl
  r <- sl+d*(a/b)
  return(r)
}
#
mi <- func(as.vector(idx["1995::2018"]),range(df$d)[1],range(df$d)[2],range(as.vector(idx["1995::2018"]))[1],range(as.vector(idx["1995::2018"]))[2])
# mi <- as.vector(idx["1995::2018"])

func <- function(x){
  if(x > 0.1){return("upper")}
  if(x > 0){return("uppermiddle")}
  if(x > -0.1){return("lowermiddle")}
  if(x < -0.1){return("lower")}
}
w <- apply.monthly(SP5[,4],sd)/apply.monthly(SP5[,4],mean)
idx <- log(apply.monthly(SP5[,4],mean))/100
#
# in the case of line graph, x-axis vector should continuous. thus, "t=index(w["1995::2018"])" is right.
# don't put descrete data into x-axis.

df <- data.frame(i=mi,d=as.vector(w["1995::2018"]),t=index(w["1995::2018"]),sign=as.vector(apply(diff(cli_xts$oecd)["1995::2018"],1,func)))



p <- ggplot(df,aes(x=t,y=d))
#
# need to investigate mapping= designator. somehow it's necessary to overlay y-axis coordinated graph.
#
p <- p + geom_bar(aes(x=t,y=d,fill=sign),stat = "identity",alpha=0.4) # need identity to draw value itself.
p <- p + geom_path(df,mapping=aes(x=t,y=i),stat="identity", position="identity",colour="black")
p <- p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
# same as above about mapping=
p <- p + geom_point(df,mapping=aes(x=t,y=i,color=sign),size=1)

plot(p)