#
# pick up months whose monthlyreturn is less than 0.05 and put them to events
#
# argument 's' could be either "2000-01-01::" or "2000-01-01::2018-12-31"
#
func <- function(s="2000-01-01::",c=0.95){
  start_date <- s
  change_rate <- c
  # events <- xts(round(monthlyReturn(GSPC[start_date])[monthlyReturn(GSPC[start_date]) < -0.05],digits = 3),as.Date(mondate(index(monthlyReturn(GSPC[start_date])[monthlyReturn(GSPC[start_date]) < -0.05]))))
  SP5 <- to.monthly(SP5)
  events <- xts(round(SP5[start_date][,4]/SP5[start_date][,1][SP5[start_date][,4]/SP5[start_date][,1] < change_rate],digits =4),as.Date(mondate(index(SP5[start_date][,4]/SP5[start_date][,1][SP5[start_date][,4]/SP5[start_date][,1] < change_rate]))))

  #
  # draw graph of cli 5 months delta of oecd all, usa and china.
  #
  plot(diff(cli_xts$oecd,lag=5)[start_date],type='h')
  # addEventLines(events, srt=90, pos=2,col=10)  # this causes misalinghment
  # addEventLines(events[c(6,7)], srt=90, pos=2,col=10)
  #
  # somehow "addEventLines(events, srt=90, pos=2,col=10)" works well
  # vertical lines are put at wrong places. instead of it, put loop.
  #
  for(i in seq(1,length(events),1)){
    addEventLines(events[i], srt=90, pos=2,col=4)
  }
  events
  #
  # and only the first entry to place.
  #
  addEventLines(events[1], srt=90, pos=2,col=4)
}
func("2000-01-01::",0.95)
