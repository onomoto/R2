func <- function(x){
  if(is.na(x)){return("NA")}
  if(x > 0.1){return("upper")}
  if(x > 0){return("uppermiddle")}
  if(x > -0.1){return("lowermiddle")}
  if(x < -0.1){return("lower")}
}

delta <- append(as.vector(diff(cli_xts$oecd)[paste(as.Date(head(index(tmp.predict),1)),"::",sep="")]),rep(NA,length(index(tmp.predict)) - length(diff(cli_xts$oecd)[paste(as.Date(head(index(tmp.predict),1)),"::",sep="")])))


df <- data.frame(
  i=log(as.vector(tmp.predict[,4])),
  g=log(as.vector(tmp.predict[,6])),
  e=log(as.vector(tmp.predict[,7])),
  r=as.vector(tmp.predict[,4]-tmp.predict[,7]),
  c=as.vector(apply(matrix(delta,ncol=1),1,func)),
  t=as.Date(index(tmp.predict)))
colnames(df)[1] <- 'i'
colnames(df)[2] <- 'g'
colnames(df)[3] <- 'e'
colnames(df)[4] <- 'r'
colnames(df)[5] <- 'clidelta'

p <- ggplot(df,aes(x=t))
#
# need to investigate mapping= designator. somehow it's necessary to overlay y-axis coordinated graph.
#
# p <- p + geom_bar(aes(y=r),stat = "identity",fill='pink',colour="black") # need identity to draw value itself.
# p <- p + geom_point(aes(y=i),stat="identity", position="identity",colour="green",size=0.8)
p <- p + geom_path(aes(y=i),stat="identity", position="identity",colour="black",linetype="solid")
p <- p + geom_point(aes(y=i,color=clidelta),stat="identity", position="identity",size=1.4)
p <- p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
# same as above about mapping=
p <- p + geom_path(aes(y=g),colour='red')
p <- p + geom_path(aes(y=e),colour='blue')
# s <- seq(1000,3500,500)
s <- seq((1+floor(min(tmp.predict[,c(4,6,7)])/500))*500,floor(max(tmp.predict[,c(4,6,7)])/500)*500,500)
p <- p + geom_hline(yintercept = log(s),size=0.4,linetype=1,colour="white") #horizontal line
p <- p + geom_hline(yintercept = log(s+250),size=0.4,linetype=2,colour="white") #horizontal line
p <- p + geom_vline(xintercept=seq(as.Date("2001-01-01"),as.Date("2019-01-01"),by='years'), colour="white",size=0.4)
for( i in s){ p <- p+annotate("text",label=as.character(s),x=as.Date("2000-01-01"), y=log(s*1.03),colour='white')}
# p <- p + theme(plot.background = element_rect(fill = "darkblue"))
# p <- p + theme_classic()
# p <- p +  theme_grey()
# P <- p + theme(panel.border = element_blank(),
#                    panel.grid.major = element_blank(),
#                    rect = element_blank(),
#                    panel.grid.minor = element_blank(),
#                    axis.line = element_line(size = 0.5, linetype = "solid",
#                                             colour = "black"))
# invalidate x-axis title text and ticks.
p <- p+theme( rect = element_rect(fill = "lightblue", colour = "white",
                                  size = 0, linetype = 1),
              panel.background = element_rect(fill = "lightblue",
                                              colour = "lightblue"),
                                              # size = 0.5, linetype = "solid"),
              panel.grid = element_blank(),
              axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
#
p <- p + labs(title = "SPX + Theory on log scale",fill="",color="")
plot(p)
remove(s)
remove(df)
