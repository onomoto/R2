

# k = duration
# e = eps
# p = PAYEMS
# u = UNDCONTSA
# c = case-shhiller home price index
# o = oecd composite leading indicator for oecd countries.
# i = number of months to calculate delta. default is 5.



my_sp5eps <- function(k,e,p,u,c,o,i=5)
{
  k2k <- k  # 0627 added to bug fix.

  m_m <- summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=i)[k2k],mean)))$coefficients

#  m_m <- summary(lm(apply.quarterly(SP5[k],mean)[,1] ~ apply.quarterly(PA[k],mean)[k] * apply.quarterly(UC[k],mean)[k] * G[k]*apply.quarterly(CS[k],mean)[k] - apply.quarterly(UC[k],mean)[k] -G[k] - apply.quarterly(PA[k],mean)[k]*G[k] - apply.quarterly(UC[k],mean)[k]*G[k]*apply.quarterly(CS[k],mean)[k]))$coefficients
  print("m_m params! apply.quarter - EPS,PA,UC,CS w/ CLI delta")
  # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  print(m_m[1]+e*m_m[2]+p*m_m[3]+u*m_m[4]+c*m_m[5]+o*m_m[6])
  # print(m_m)
  cat("k = "); print(k)
#
}


