#
# pick up months whose monthlyreturn is less than 0.05 and put them to events
#
start_date <- "2007::"
events <- xts(round(monthlyReturn(GSPC[start_date])[monthlyReturn(GSPC[start_date]) < -0.05],digits = 3),as.Date(mondate(index(monthlyReturn(GSPC[start_date])[monthlyReturn(GSPC[start_date]) < -0.05]))))
#
# draw graph of cli 5 months delta of oecd all, usa and china.
#
plot(diff(cli_xts,lag=5)[start_date],type='l')
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
