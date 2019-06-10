
#
# 1)draw spx hisgram, while reading monthly close drawned in the different color when cli delta is positive and negative.
# 2)overlaid correction period by lime green.
#
#
lag_month <- 1
# mnt <- index(cli_xts$oecd[total_terms][cli_xts$oecd[total_terms]/as.vector(cli_xts$oecd["1999-07-01::2018-07-01"]) < 1])
total_terms <- paste("1990",index(last(cli_xts$oecd)),sep="::")

mnt_minus <- index(cli_xts$oecd[total_terms][na.omit(diff(cli_xts$oecd,lag=lag_month))[total_terms] < 0])
mnt_plus <- index(cli_xts$oecd[total_terms][na.omit(diff(cli_xts$oecd,lag=lag_month))[total_terms] > 0])

# plot.zoo(merge(log(to.monthly(SP5)[mnt_plus][,4]),log(to.monthly(SP5)[mnt_minus][,4])),type='h',col = c("blue", "red"), plot.type = "single")
# # abline(v=seq(as.Date("1991-01-01"),as.Date("2019-01-01"),by='years'), col=rgb(0,1,0,alpha=0.9),lty=2)
# # abline(v=seq(1,12*29,12), col=rgb(0,1,0,alpha=0.9),lty=2)
# par(new=T)
# need to have 'x' aligning with index(correction_data)
plot.default(seq(as.Date("1990-01-01"),index(last(cli_xts$oecd)),by='months'),log(to.monthly(SP5)[,4][total_terms]),axes=F,type='l') 
for(i in seq(1,length(correction_data[total_terms]),1)){
    abline(v=index(correction_data[total_terms])[i],col=rgb(0.5,0.9,0,alpha=0.1),lty=1,lwd=1)
    # cat(i)
    # cat(" ")
}
abline(v=index(correction_data[total_terms])[1],col=rgb(0.5,0.9,0,alpha=0.1),lty=1,lwd=1)  # need here 
# abline(v=seq(1,12*29,12), col=rgb(0,1,0,alpha=0.9),lty=2)
# same as plot.default(), now has indexed x lim, need to align vline with them.
abline(v=seq(as.Date("1990-01-01"),index(last(cli_xts$oecd)),by='years'), col=rgb(0,1,1,alpha=0.9),lty=2)
par(new=T)
# adjust lwd= for better looking. draw after other graph as to show histogram the most forefront.
plot.zoo(merge(log(to.monthly(SP5)[mnt_plus][,4]),log(to.monthly(SP5)[mnt_minus][,4])),type='h',col = c("blue", "red"), plot.type = "single",lwd=4)
abline(h=log(seq(500,2500,500)),col=rgb(0,1,1,alpha=0.9),lty=2)  # horizontal line from 500 to 2500 interval=500.