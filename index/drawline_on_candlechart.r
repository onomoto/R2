# > candleChart(to.weekly(fas_shares * FAS[,4] +spxl_shares * SPXL[,4]+as.xts(c2+c3+c4,index(fas_shares))),theme='white')
# > weekly_pf[106]
#                open     high      low    close
# 2016-01-08 796732.6 804098.1 684234.9 684234.9
# > length(index(weekly_pf))-106
# [1] 140
# >  tmp <- as.xts(append(rep(684234.9,106),approx(seq(1,2,1),c(684234.9,2170000),n=140,method="linear")$y),append(rep(as.Date("2016-01-08"),106),last(index(weekly_pf),n=140)))
# > addTA(tmp,on=1,legend="slope")
# # length(index(weekly_pf)) - 158 = 88
# > tmp <- as.xts(append(rep(1189422,158),asspprox(seq(1,2,1),c(1189422,2170000),n=88,method="linear")$y),append(rep(as.Date("2017-01-06"),158),last(index(weekly_pf),n=88)))
# > addTA(tmp,on=1,legend="slope")
# > weekly_pf[158]
#               open    high     low   close
# 2017-01-06 1160008 1189422 1160008 1189422
# > weekly_pf[210]
#               open    high     low   close
# 2018-01-05 1906271 1991231 1906271 1991231
# > plot(addLines(v=c(106,158,210)))
# > addTA(as.xts(approx(seq(1,2,1),c(600000,2170000),n=246,method="linear")$y,index(weekly_pf)),on=1,name='2100000')
# > addTA(tmp,on=1,legend="slope")
#
function(par_xts,start_val,start_date,end_val,end_date){
  len <- length(par_xts)
  len_start <- length(par_xts[paste("::",start_date,sep="")])
  len_end <- length(par_xts[paste(end_date,"::",sep="")])
  first_date <- first(index(par_xts))
  last_date <- last(index(par_xts))
  tmp <- as.xts(c(rep(start_val,len_start),approx(seq(1,2,1),c(sart_val,end_val),n=len-len_start-len_end,method="linear")$y,
  rep(end_val,len_end)),
  c(rep(as.Date(start_date),len_start),last(index(weekly_pf),n=len-len_start-len_end),
  rep(as.Date(index(weekly_pf)[len-len_end]),len_end)))
  addTA(tmp,on=1,legend="slope")
}
#
# my_draw_line_on_candle(weekly_pf,weekly_pf[weekly_pf[,2] == max(weekly_pf)][,4],index(weekly_pf[weekly_pf[,2] == max(weekly_pf)][,4]),last(weekly_pf)[,4],index(last(weekly_pf)))
# my_draw_line_on_candle(weekly_pf,2189051,"2018-01-19",2189051,"2018-09-19")
#
my_draw_line_on_candle <- function(par_xts,start_val,start_date,end_val,end_date){
  len <- length(seq(as.Date(start_date),as.Date(end_date),by='weeks'))
  plot_data <- approx(seq(1,2,1),c(start_val,end_val),n=len,method='linear')$y
  tmp_xts <- as.xts(plot_data,seq(as.Date(start_date),as.Date(end_date),by='weeks'))
  addTA(tmp_xts,on=1,legend="slope")
}

# > weekly_pf[weekly_pf[,2] == max(weekly_pf)]
#               open    high     low   close
# 2018-01-26 2192256 2273486 2192256 2273486
# > weekly_pf["2016"][1]
#                open     high      low    close
# 2016-01-08 796732.6 804098.1 684234.9 684234.9
# > as.xts(c(684239,2273486)),c(as.Date("2016-01-08"),as.Date("2018-01-26"))
#  エラー:  予想外の ',' です  in "as.xts(c(684239,2273486)),"
# > as.xts(c(684239,2273486)),c(as.Date("2016-01-08"),as.Date("2018-01-26")))
#  エラー:  予想外の ',' です  in "as.xts(c(684239,2273486)),"
# > as.xts(c(684239,2273486),c(as.Date("2016-01-08"),as.Date("2018-01-26")))
#               [,1]
# 2016-01-08  684239
# 2018-01-26 2273486
# > tmp <- as.xts(c(684239,2273486),c(as.Date("2016-01-08"),as.Date("2018-01-26")))
# > addTA(tmp,on=1,legend="slope")
# > approx(seq(1,2,1),c(684239,2273486),n=2,method='linear')$y
# [1]  684239 2273486
# > tmp <- as.xts(approx(seq(1,2,1),c(684239,2273486),n=2,method='linear')$y,c(as.Date("2016-01-08"),as.Date("2018-01-26")))
# > addTA(tmp,on=1,legend="slope")
# > length(seq(as.Date("2016-01-08"),as.Date("2018-01-26"),by='weeks'))
# [1] 108
# > tmp <- as.xts(approx(seq(1,2,1),c(684239,2273486),n=108,method='linear')$y,seq(as.Date("2016-01-08"),as.Date("2018-01-26"),by='weeks'))
