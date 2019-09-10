kikan <- "2000-01-01::2019-06-01"
func <- function(x){
  if(x > 0.1){return("upper")}
  if(x > 0){return("uppermiddle")}
  if(x > -0.1){return("lowermiddle")}
  if(x < -0.1){return("lower")}
}

#  [kikan]
df <- data.frame(i=as.vector(tmp.predict[kikan][,4]),
g=as.vector(tmp.predict[kikan][,6]),
e=as.vector(tmp.predict[kikan][,7]),
r=as.vector(tmp.predict[kikan][,4]-tmp.predict[kikan][,7]),
c=as.vector(apply(diff(cli_xts$oecd)[kikan],1,func)),
t=as.Date(index(tmp.predict[kikan])))

colnames(df)[1] <- 'i'
colnames(df)[2] <- 'g'
colnames(df)[3] <- 'e'
colnames(df)[4] <- 'r'
colnames(df)[5] <- 'clidelta'

p <- ggplot(df,aes(x=t))
#
# need to investigate mapping= designator. somehow it's necessary to overlay y-axis coordinated graph.
#
p <- p + geom_bar(aes(y=r,fill=clidelta),stat = "identity",colour="black") # need identity to draw value itself.
p <- p + geom_point(mapping=aes(y=i,colour=clidelta),stat="identity", position="identity",size=0.8)
p <- p + geom_path(aes(y=i),stat="identity", position="identity",colour="black",linetype="dotted")
p <- p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
# same as above about mapping=
p <- p + geom_path(aes(y=g),colour='red')
p <- p + geom_path(aes(y=e),colour='blue')
p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
# p <- p + labs(fill = "CLI Delta")

plot(p)
# remove unnecessary function.
remove(func)
remove(df)
