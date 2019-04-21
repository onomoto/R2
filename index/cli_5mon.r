#
# plese refer the latter half of
# https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html
# lag is set to  5 months.
#
#   s = start date of the spiral like "1992-01-01::" DON'T FORGET DOUBLE COLON!!
#   l = length of years like 9. use equal or less than 9
#
func <- function(s="1992-01-01::",l=9){

  head_of_record <- "1964-01-01::"
  print(head_of_record)
  offset <- length(seq(as.Date(head_of_record),as.Date(s),by='months'))
  len_mon <- l*12-1
  # len_mon <- l
  print(offset)
  print(len_mon)
  print(seq(as.Date(head_of_record),as.Date(s),by='months')[offset])
  print(seq(as.Date(head_of_record),as.Date("2100-01-01"),by='months')[offset+len_mon])

  print(offset)
  plot.default(na.trim(diff(cli_xts$oecd,lag=5))[head_of_record][offset:(offset+len_mon)],cli_xts$oecd[head_of_record][offset:(offset+len_mon)],type='b')
  # print(offset)
  tmp <- par('usr')
  # par(new=T)
  plot.default(na.trim(diff(cli_xts$oecd,lag=5))[head_of_record][offset:(offset+len_mon)],cli_xts$oecd[head_of_record][offset:(offset+len_mon)],type='b',xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),lwd=1)
  par(new=T)
  for(i in seq(0,l-1,1)){
    print(i)
    # print(offset)
    print(offset+i*12)
    print(offset+i*12+11)
    par(new=T)
    plot.default(na.trim(diff(cli_xts$oecd,lag=5))[head_of_record][(offset+i*12):(offset+i*12+11)],cli_xts$oecd[head_of_record][(offset+i*12):(offset+i*12+11)],type='b',xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=i+1,lwd=2)
    par(new=T)
  }
  abline(v=0)
  abline(h=100)
  abline(v=seq(0.5,-1,-0.1),col=6,lty=3)
}
