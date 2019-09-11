#
# 1) draw graph PAEMS delta and its moving average. use 'mov_mon' to adjust it.
# 2) start is fixed to 2012-01-01 and the end is the current most updated data available month.
# 3) rewrite to use ggplot()
#

func <- function(m=6){
    len_mon <- length(seq(as.Date("2012-01-01"),index(last(PA)),by='months'))
    mov_mon <- m
    w <- merge(last(na.omit(diff(PA)),n=len_mon),last(na.omit(filter(diff(PA),rep(1,mov_mon))/mov_mon),n=len_mon),last(na.omit(filter(diff(PA),rep(1,mov_mon*2))/(mov_mon*2)),n=len_mon),suffixes=c("reading","mvblue","mvred"))
    df<- data.frame(d=as.vector(w[,1]),six=as.vector(w[,2]),twelve=as.vector(w[,3]),t=as.Date(index(w)))
    return(df)
}
df <- func(6)
func <- function(df,m=6){
  p <- ggplot(df,aes(x=t))
  p <- p + geom_bar(aes(y=d,fill=t),stat = "identity" ,width=15,colour='blue') # need identity to draw value itself.
  p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  # same as above about mapping=
  p <- p + geom_path(aes(y=six),colour='red',size=0.8)
  p <- p + geom_path(aes(y=twelve),colour='yellow',size=0.8)
  p <- p+theme(   # rect = 
                  # element_rect(fill = "white", colour = "black",
                  #                   size = 0, linetype = 1),
                axis.title.x=element_blank(),axis.title.y=element_blank(),
                panel.background = element_rect(fill = "skyblue",
                                                colour = "lightblue"))
  plot(p)
}
func(df,6)
func <- function(df,m=6){
  print(last(df,m)[,c(4,1,2,3)])
}
func(df,6)
