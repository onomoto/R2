#
# 1) draw graph PAEMS delta and its moving average. use 'mov_mon' to adjust it.
# 2) start is fixed to 2012-01-01 and the end is the current most updated data available month.
#
func <- function(m=6){
    len_mon <- length(seq(as.Date("2012-01-01"),index(last(PA)),by='months'))
    mov_mon <- m
    w <- merge(last(na.omit(diff(PA)),n=len_mon),last(na.omit(filter(diff(PA),rep(1,mov_mon))/mov_mon),n=len_mon),last(na.omit(filter(diff(PA),rep(1,mov_mon*2))/(mov_mon*2)),n=len_mon),suffixes=c("reading","mvbue","mvred"))
    return(w)
    plot(w[,1],type='h',ylim=c(0,max(w[,1]*1.05)))
    lines(w[,2],col=4)
    lines(w[,3],col=2)
    # print(last(w,m))
}
o <- func(6)
func <- function(w,m=6){
  plot(w[,1],type='h',ylim=c(0,max(w[,1]*1.05)))
  lines(w[,2],col=4)
  lines(w[,3],col=2)  
}
func(o,6)
func <- function(w,m=6){
  print(last(w,m))
}
func(o,6)