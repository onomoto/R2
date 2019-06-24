#
# 1) draw graph PAEMS delta and its moving average. use 'mov_mon' to adjust it.
# 2) start is fixed to 2012-01-01 and the end is the current most updated data available month.
#
func <- function(m=6){
    len_mon <- length(seq(as.Date("2012-01-01"),index(last(PA)),by='months'))
    mov_mon <- m
    w <- merge(last(na.omit(diff(PA)),n=len_mon),last(na.omit(filter(diff(PA),rep(1,mov_mon))/mov_mon),n=len_mon),last(na.omit(filter(diff(PA),rep(1,mov_mon*2))/(mov_mon*2)),n=len_mon))
    plot(w[,1],type='h')
    lines(w[,2],col=4)
    lines(w[,3],col=2)
}
func(6)