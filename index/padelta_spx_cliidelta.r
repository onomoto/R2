 # draw the graph to overlay PA 1 month delta, SPX, oecd cli and its 6 month delta.
 #  https://00819.blogspot.com/2019/02/plot-abline-eps-gspc.html for the reference
 #
start_date <- "1992-01-01"
end_date <- last(index(cli_xts))
period <- paste(start_date,end_date,sep="::")
lag_month <- 1
#
year_seq <- seq(as.Date(start_date),as.Date(end_date),by='years')
mnt <- seq(as.Date(start_date),as.Date(end_date),by='months')
plot.default(mnt,na.omit(diff(PA))[period],type='h',axes=F,col=3,main='padelta_spx_cliidelta.r',xlab='',ylab='')
par(new=T)
# mnt <- seq(as.Date("1992-02-01"),as.Date("2018-12-31"),by='months')
plot.default(mnt,log(to.monthly(SP5[period])[,4]),type='l',xlab='',ylab='')
abline(v=year_seq,col = "gray60",lty=3)
abline(h=log(seq(500,2500,500)),col="gray60",lty=2)
abline(h=1191.5,col=2,lty=2)
# abline(v=as.Date("2014-01-01"),col = "gray60",lty=3)
par(new=T)
plot.default(mnt,as.vector(cli_xts$oecd[period]),axes=F,col=4,type='l',xlab='',ylab='')
abline(h=100,col=4,lty=3)
par(new=T)
plot.default(mnt,na.trim(diff(cli_xts$oecd,lag=lag_month))[period],axes=F,col=6,type='l',xlab='',ylab='')
abline(h=0,col=6,lty=3)
abline(v=as.Date("2000-03-01"),col=2,lty=2)
abline(v=as.Date("2007-06-01"),col=2,lty=2)
# abline(v=as.Date(index(cli_xts$oecd[period][cli_xts$oecd[period] > 101])),col=5,lty=4)
