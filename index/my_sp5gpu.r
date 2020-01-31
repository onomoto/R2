# start_date <- "1992-01-01"
# end_date <- "2016-07-01"
# (summary(lm(to.quarterly(SP5[paste(start_date,end_date,sep='::')])[,4] ~
#                 + to.quarterly(PAYEMS[paste(start_date,end_date,sep='::')])[,4] *
#                 + to.quarterly(UNDCONTSA[paste(start_date,end_date,sep='::')])[,4] *
#                 + GDPC96[paste(start_date,end_date,sep='::')])))
#



# result.gpu <<- lm(apply.quarterly(SP5[k2k],mean)[,1] ~
# +PAq[k2k] * UCq[k2k]
# +PAq[k2k]  * G[k2k]
# +UCq[k2k]  * G[k2k]
# +apply.quarterly(diff(cli_xts$oecd,lag=lag_month)[k2k],mean)
# - UCq[k2k]
# - PAq[k2k])
#
# summary(result.gpu)
# > result.gpu$coefficients
#                                                     (Intercept)
#                                                   -2.334489e+03
#                                                          G[k2k]
#                                                   -6.790126e-01
# apply.quarterly(diff(cli_xts$oecd, lag = lag_month)[k2k], mean)
#                                                    1.098955e+02
#                                               PAq[k2k]:UCq[k2k]
#                                                    3.901568e-05
#                                                 PAq[k2k]:G[k2k]
#                                                    7.150026e-06
#                                                 UCq[k2k]:G[k2k]
#                                                   -4.099912e-04



my_sp5gpu <- function(k,g,p,u,d,i=5,cli=cli_xts$oecd)
{
  # k = kikan
  # g = GDP
  # p = NFP/PA
  # u = UNDCONTSA/UC
  # d = cli_xts Delta
  # m = Delta months

  # summary(lm(apply.quarterly(SP5[k],mean)[,1] ~ apply.quarterly(PA[k],mean) * apply.quarterly(UC[k],mean) * GDP[k] - apply.quarterly(UC[k],mean) ))


  result.gpu <<- lm(apply.quarterly(SP5[k2k],mean)[,1] ~
  +PAq[k2k] * UCq[k2k]
  +PAq[k2k]  * G[k2k]
  +UCq[k2k]  * G[k2k]
  +apply.quarterly(diff(cli,lag=i)[k2k],mean)
  - UCq[k2k]
  - PAq[k2k])
  summary(result.gpu)
  m_m <- result.gpu$coefficients
  print("m_m params! apply.quarter - UC w/ nominal GDP")
  # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  print(m_m[1]+g*m_m[2]+ d*m_m[3]+p*u*m_m[4]+p*g*m_m[5]+g*u*m_m[6])
  cat("k = "); print(k)

  # print("from 1992 till 2016Q3")
  # print(2.737e+04+y*-2.913e-01+z*-9.770e+00+x*-1.957e+00+y*z*1.925e-04+y*x*2.130e-05+z*x*-3.003e-04+y*z*x*-6.143e-09)
}
