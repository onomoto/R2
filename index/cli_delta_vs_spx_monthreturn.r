#
# pick up months whose monthlyreturn is less than 0.05 and put them to events
#
# argument 'p' could be either "2000-01-01::" or "2000-01-01::2018-12-31"
# argument c is between 0 and 1
# argument l is between 1 and 12
#
func <- function(p="2000-01-01",c=0.95,l=5,d="C"){
    if(nchar(p) > 12){
        period <- p
    } else{
        period <- paste(p,index(last(cli_xts)),sep="::")
    }
    print(period)
    lag_month <- l
    change_rate <- c
  # events <- xts(round(monthlyReturn(GSPC[start_date])[monthlyReturn(GSPC[start_date]) < -0.05],digits = 3),as.Date(mondate(index(monthlyReturn(GSPC[start_date])[monthlyReturn(GSPC[start_date]) < -0.05]))))
    SP5 <- to.monthly(SP5)
    # events <- xts(round(SP5[start_date][,4]/SP5[start_date][,1][SP5[start_date][,4]/SP5[start_date][,1] < change_rate],digits =4),as.Date(mondate(index(SP5[start_date][,4]/SP5[start_date][,1][SP5[start_date][,4]/SP5[start_date][,1] < change_rate]))))

    event_date <- as.Date(mondate(index(SP5[period][,4]/SP5[period][,1][SP5[period][,4]/SP5[period][,1] < change_rate])))
    mnt_bm <- index(cli_xts$oecd[period][na.omit(diff(cli_xts$oecd,lag=lag_month))[period] < 0 & na.omit(diff(cli_xts$oecd))[period] < 0])
    mnt_mp <- index(cli_xts$oecd[period][na.omit(diff(cli_xts$oecd,lag=lag_month))[period] < 0 & na.omit(diff(cli_xts$oecd))[period] > 0])
    mnt_pm <- index(cli_xts$oecd[period][na.omit(diff(cli_xts$oecd,lag=lag_month))[period] > 0 & na.omit(diff(cli_xts$oecd))[period] < 0])
    if(d == "V"){
        VDATA <- VIX[,4]
    }else if(d == "C"){
        VDATA <- na.omit(diff(cli_xts$oecd,lag=lag_month))[period]
    }else{
        stop("wrong arg! V or C")
    }

    plot.zoo(merge(VDATA[period],VDATA[mnt_bm],VDATA[mnt_mp],VDATA[mnt_pm]),type='h',col = c("blue", "red",rgb(1,0,1,alpha=0.9),rgb(1,0,1,alpha=0.9)), plot.type = "single",lwd=3)
    #
    # draw yearly v-line.
    #
    if(nchar(p) > 12){
        y_start <- seq(as.Date(substr(period,1,10)),as.Date(paste(substr(period,13,16),"-01-01",sep="")),by='years')
    } else{
        y_start <- seq(as.Date(substr(period,1,10)),as.Date("2019-01-01"),by='years')
    }
    for(i in seq(1,length(y_start),1)){
        abline(v=y_start[i],col=rgb(0,1,0,alpha=0.9),lty=1)
    }
    abline(v=y_start[1],col=rgb(0,1,0,alpha=0.9),lty=1)
    #
    # draw event v line.
    #
    for(i in seq(1,length(event_date),1)){
        abline(v=event_date[i],col = "gray60",lty=2,lwd=2)
        print(i)
    }
    abline(v=event_date[1],col = "gray60",lty=2,lwd=2)
  #
  # draw graph of cli 5 months delta of oecd all, usa and china.
  #
  # addEventLines(events, srt=90, pos=2,col=10)  # this causes misalinghment
  # addEventLines(events[c(6,7)], srt=90, pos=2,col=10)
  #
  # somehow "addEventLines(events, srt=90, pos=2,col=10)" works well
  # vertical lines are put at wrong places. instead of it, put loop.
  #
#   for(i in seq(1,length(events),1)){
#     addEventLines(events[i], srt=90, pos=2,col=4)
#   }
#   events
#   #
#   # and only the first entry to place.
#   #
#   addEventLines(events[1], srt=90, pos=2,col=4)
}
func("2000-01-01",0.95,5,"C")
