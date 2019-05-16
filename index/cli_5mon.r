#
# plese refer the latter half of
# https://00819.blogspot.com/2019/03/vix-cli-6-month-delta-and-s.html
# lag is set to  5 months.
#
#   s = start date of the spiral like "1992-01-01::" DON'T FORGET DOUBLE COLON!!
#       Don't set before "1964-01-01"
#   l = length of years like 9. recommend to use equal or less than 9. Don't exceed the current end of data.
#
#   use like  > func("2001-01-01::",5)
#
func <- function(s="2011-01-01::",l=9){

  head_of_record <- "1964-01-01::"
  print(head_of_record)
  offset <- length(seq(as.Date(head_of_record),as.Date(s),by='months'))
  max_length <- length(seq(as.Date(head_of_record),as.Date(index(last(cli_xts))),by='months'))
  len_mon <- l*12-1
  lag_month <- 5
  # when the end period exceeds the current end, adjust # of months and years to avoid the counters go beyond the limit.
  if(offset + len_mon > max_length){
    len_mon <- max_length - offset
    l <- ceiling(len_mon/12) # ceiling is to round up
  }
  # len_mon <- l
  # print(offset)
  # print(len_mon)
  # print(seq(as.Date(head_of_record),as.Date(s),by='months')[offset])
  # print(seq(as.Date(head_of_record),as.Date("2100-01-01"),by='months')[offset+len_mon])

  # print(offset)
  plot.default(na.trim(diff(cli_xts$oecd,lag=lag_month))[head_of_record][offset:(offset+len_mon)],cli_xts$oecd[head_of_record][offset:(offset+len_mon)],type='b')
  # print(offset)
  tmp <- par('usr')
  # par(new=T)
  plot.default(na.trim(diff(cli_xts$oecd,lag=lag_month))[head_of_record][offset:(offset+len_mon)],cli_xts$oecd[head_of_record][offset:(offset+len_mon)],type='b',xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),lwd=1,main=paste("from",substr(s,1,10),"for",len_mon+1,"months",sep=" "),ylab="",xlab="")
  par(new=T)
  for(i in seq(0,l-1,1)){
    print(i)
    # print(offset)
    # print(offset+i*12)
    # print(offset+i*12+11)
    par(new=T)
      # when the end period exceeds the current end, adjust # of months to avoid OOB
      # otherwise months to go in each iteration is always 11.
    if(offset+i*12+11 < max_length){
      m <- 11
    }else{
      #adjust months to go as go within max_length.
      m <- max_length - (offset+i*12)
    }
    # print(m)
    # print(max_length)
    # print(len_mon)
    # print(offset)
    plot.default(na.trim(diff(cli_xts$oecd,lag=lag_month))[head_of_record][(offset+i*12):(offset+i*12+m)],cli_xts$oecd[head_of_record][(offset+i*12):(offset+i*12+m)],type='b',xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),col=i+1,lwd=2,ylab="",xlab="",axes = F)
    if(i == 6){
      # when i ==6 "yellow" is used for dots, but offers poor visibility. plot 'x' upon them to improve.
      par(new=T)
      plot.default(na.trim(diff(cli_xts$oecd,lag=lag_month))[head_of_record][(offset+i*12):(offset+i*12+m)],cli_xts$oecd[head_of_record][(offset+i*12):(offset+i*12+m)],type='p',xlim=c( tmp[1],tmp[2]), ylim=c(tmp[3], tmp[4]),pch='x',ylab="",xlab="",axes = F)
    }
    par(new=T)
  }
  abline(v=0)
  abline(h=100)
  # abline(v=seq(0.5,-1,-0.1),col=6,lty=3)
  # automatically calculate the upper and lower limit of vline.
  # might be able to use 'floor(tmp[2]*10)/10' instead of round(x,digit=1)
  abline(v=seq(ceiling((tmp[1]*10))/10,floor(tmp[2]*10)/10,0.1),col=6,lty=3)

}
func("2011-01-01::",9)
