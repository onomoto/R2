start_date <- "1992-01-01"
end_date <- "2016-07-01"
(summary(lm(to.quarterly(SP5[paste(start_date,end_date,sep='::')])[,4] ~
                + to.quarterly(PAYEMS[paste(start_date,end_date,sep='::')])[,4] *
                + to.quarterly(UNDCONTSA[paste(start_date,end_date,sep='::')])[,4] *
                + GDPC96[paste(start_date,end_date,sep='::')])))


my_sp5 <- function(g,p,u)
{
  # x <- GDP  :like 17556
  # y <- PAYEMS :like 150000
  # z <- UNDCONTSA :like 1037
  # 2.371e+04+y*-2.615e-01+z*-5.720e+00+x*-1.707e+00+y*z*1.590e-04+y*x*1.926e-05+z*x*-5.615e-04+y*z*x*-3.970e-09
  #
  # start_date <- "1992-01-01"
  # end_date <-  "2016-04-01"
  # end_date  <- "2016-07-01"
  # kikan <- paste(start_date,end_date,sep='::')
  ###########
  # print("m_m params! to.quarter w/ GDPC96")
  # m_m <- summary(lm(to.quarterly(SP5[kikan])[,4] ~ to.quarterly(PA[kikan])[,4] * to.quarterly(UC[kikan])[,4] * G[kikan]))$coefficients
  # print(m_m[1]+p*m_m[2]+u*m_m[3]+g*m_m[4]+p*u*m_m[5]+p*g*m_m[6]+u*g*m_m[7]+g*p*u*m_m[8])
  # #############
  # m_m <- summary(lm(apply.quarterly(SP5[kikan],mean)[,1] ~ apply.quarterly(PA[kikan],mean) * apply.quarterly(UC[kikan],mean) * G[kikan]))$coefficients
  # print("m_m params! apply.quarter w/ GDPC96")
  # print(m_m[1]+p*m_m[2]+u*m_m[3]+g*m_m[4]+p*u*m_m[5]+p*g*m_m[6]+u*g*m_m[7]+g*p*u*m_m[8])
  # # print(m_m[1]+y*m_m[2]+z*m_m[3]+x*m_m[4]+y*z*m_m[5]+y*x*m_m[6]+z*x*m_m[7]+y*z*x*m_m[8])
  # ###############
  # m_m <-summary(lm(apply.quarterly(SP5[kikan],mean)[,1] ~ apply.quarterly(PA[kikan],mean) * apply.quarterly(UC[kikan],mean) * G[kikan] - apply.quarterly(UC[kikan],mean) ))$coefficients
  #   print("m_m params! apply.quarter - UC w/ GDPC96")
  # # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  # print(m_m[1]+p*m_m[2]+g*m_m[3]+p*u*m_m[4]+p*g*m_m[5]+u*g*m_m[6]+g*p*u*m_m[7])
  summary(lm(apply.quarterly(SP5[kg],mean)[,1] ~ apply.quarterly(PA[kg],mean) * apply.quarterly(UC[kg],mean) * GDP[kg] - apply.quarterly(UC[kg],mean) ))
  m_m <-summary(lm(apply.quarterly(SP5[kg],mean)[,1] ~ apply.quarterly(PA[kg],mean) * apply.quarterly(UC[kg],mean) * GDP[kg] - apply.quarterly(UC[kg],mean) ))$coefficients
    print("m_m params! apply.quarter - UC w/ nominal GDP")
  # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  print(m_m[1]+p*m_m[2]+g*m_m[3]+p*u*m_m[4]+p*g*m_m[5]+u*g*m_m[6]+g*p*u*m_m[7])
  cat("kg = "); print(kg)

  # print("from 1992 till 2016Q3")
  # print(2.737e+04+y*-2.913e-01+z*-9.770e+00+x*-1.957e+00+y*z*1.925e-04+y*x*2.130e-05+z*x*-3.003e-04+y*z*x*-6.143e-09)
}



my_sp5 <- function(k,g,p,u)
{

  summary(lm(apply.quarterly(SP5[k],mean)[,1] ~ apply.quarterly(PA[k],mean) * apply.quarterly(UC[k],mean) * GDP[k] - apply.quarterly(UC[k],mean) ))
  m_m <-summary(lm(apply.quarterly(SP5[k],mean)[,1] ~ apply.quarterly(PA[k],mean) * apply.quarterly(UC[k],mean) * GDP[k] - apply.quarterly(UC[k],mean) ))$coefficients
    print("m_m params! apply.quarter - UC w/ nominal GDP")
  # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  print(m_m[1]+p*m_m[2]+g*m_m[3]+p*u*m_m[4]+p*g*m_m[5]+u*g*m_m[6]+g*p*u*m_m[7])
  cat("k = "); print(k)

  # print("from 1992 till 2016Q3")
  # print(2.737e+04+y*-2.913e-01+z*-9.770e+00+x*-1.957e+00+y*z*1.925e-04+y*x*2.130e-05+z*x*-3.003e-04+y*z*x*-6.143e-09)
}


my_sp5_fc <- function(x)
{
  # x <- span of forecast by # of quarter. 4 = 1 yr, 8= 2yrs.
   print(my_sp5(predict(VAR(v_GPC_q_1992_2016,lag.max=VARselect(v_GPC_q_2001_2016)$selection[1]),n.ahead=x)$fcst$GDPC96[,1],predict(VAR(v_GPC_q_1992_2016,lag.max=VARselect(v_GPC_q_2001_2016)$selection[1]),n.ahead=x)$fcst$PAYEMS[,1],predict(VAR(v_GPC_q_1992_2016,lag.max=VARselect(v_GPC_q_2001_2016)$selection[1]),n.ahead=x)$fcst$UNDCONTSA[,1]))
   # seq(as.Date(Sys.Date()),as.Date(last(seq(mondate(Sys.Date()), by=1, length.out=x*3))),
   # plot(seq(as.Date(Sys.Date()),as.Date(last(seq(mondate(Sys.Date()), by=1, length.out=x*3))), by="quarters"),my_sp5(predict(VAR(v_GPC_q_1992_2016,lag.max=10),n.ahead=x)$fcst$GDPC96[,1],predict(VAR(v_GPC_q_1992_2016,lag.max=10),n.ahead=x)$fcst$PAYEMS[,1],predict(VAR(v_GPC_q_1992_2016,lag.max=10),n.ahead=x)$fcst$UNDCONTSA[,1]),ylab="",xlab="s&p500")
 }

my_sp5_fc <- function(x,var_p)
{
  # x <- span of forecast by # of quarter. 4 = 1 yr, 8= 2yrs.
  # VAR structre like v_GPC_q_2001_2016
  print(my_sp5(predict(VAR(var_p,lag.max=10),n.ahead=x)$fcst$GDPC96[,1],predict(VAR(var_p,lag.max=10),n.ahead=x)$fcst$PAYEMS[,1],predict(VAR(var_p,lag.max=10),n.ahead=x)$fcst$UNDCONTSA[,1]))
  # seq(as.Date(Sys.Date()),as.Date(last(seq(mondate(Sys.Date()), by=1, length.out=x*3))),
  plot(seq(as.Date(Sys.Date()),as.Date(last(seq(mondate(Sys.Date()), by=1, length.out=x*3))), by="quarters"),my_sp5(predict(VAR(var_p,lag.max=10),n.ahead=x)$fcst$GDPC96[,1],predict(VAR(var_p,lag.max=10),n.ahead=x)$fcst$PAYEMS[,1],predict(VAR(var_p,lag.max=10),n.ahead=x)$fcst$UNDCONTSA[,1]),ylab="",xlab="s&p500")
}


#my_sp5(predict(VAR(v_GPC_q_2001_2016,lag.max=10))$fcst$GDPC96[,1],predict(VAR(v_GPC_q_2001_2016,lag.max=10))$fcst$PAYEMS[,1],predict(VAR(v_GPC_q_2001_2016,lag.max=10))$fcst$UNDCONTSA[,1])


plot(to.quarterly(SP5[paste(start_date,end_date,sep='::')])[,4],col=2,ylim=c(0,2500))
par(new=T)
plot(predict(lm(to.quarterly(SP5[paste(start_date,end_date,sep='::')])[,4] ~
                                    to.quarterly(PAYEMS[paste(start_date,end_date,sep='::')])[,4] *
                                    to.quarterly(UNDCONTSA[paste(start_date,end_date,sep='::')])[,4] *
                                    GDPC96[paste(start_date,end_date,sep='::')])),ylim=c(0,2500),ylab="",type='l')
