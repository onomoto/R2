
# k = duration
# e = eps
# p = PAYEMS
# u = UNDCONTSA
# c = case-shhiller home price index
# o = oecd composite leading indicator for oecd countries given month delta. the default is 5. the value like 0.5,0.1,0,-0.1,-0.5
# i = number of months to calculate delta. default is 5.
#
# usage and output is like below.
#
# my_sp5xts(k2k,eps_year_xts["2020::"][3],as.vector(last(PA)),as.vector(last(UC)),as.vector(CS["2019-04-01"]*1.02),0.64)
#
# Call:
#    <SKIP>
#
# [1] "m_m params! apply.quarter - EPS,PA,UC,CS w/ CLI delta"
#                [,1]
# 2020-07-01 2967.698


my_sp5xts <- function(k,e,p,u,c,o,i=5,cli=cli_xts$oecd)
{
  k2k <- k  # 0627 added to bug fix.

  m_m <- summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(diff(cli,lag=i)[k2k],mean)))$coefficients

  print(summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(diff(cli,lag=i)[k2k],mean))))

#  m_m <- summary(lm(apply.quarterly(SP5[k],mean)[,1] ~ apply.quarterly(PA[k],mean)[k] * apply.quarterly(UC[k],mean)[k] * G[k]*apply.quarterly(CS[k],mean)[k] - apply.quarterly(UC[k],mean)[k] -G[k] - apply.quarterly(PA[k],mean)[k]*G[k] - apply.quarterly(UC[k],mean)[k]*G[k]*apply.quarterly(CS[k],mean)[k]))$coefficients
  print("m_m params! apply.quarter - EPS,PA,UC,CS w/ CLI delta")
  # cat(summary(m_m))   # this doesn't work
  # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  # print(m_m[1]+e*m_m[2]+p*m_m[3]+u*m_m[4]+c*m_m[5]+o*m_m[6])
  return(m_m[1]+e*m_m[2]+p*m_m[3]+u*m_m[4]+c*m_m[5]+o*m_m[6])
  # print(m_m)
  cat("k = "); print(k)
#
}
