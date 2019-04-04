

# k = duration
# e = eps
# p = PAYEMS
# u = UNDCONTSA
# c = case-shhiller home price index
# o = oecd composite leading indicator for oecd countries.

my_sp5eps <- function(k,e,p,u,c,o)
{

  m_m <- summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=6)[k2k],mean)))$coefficients
  
#  m_m <- summary(lm(apply.quarterly(SP5[k],mean)[,1] ~ apply.quarterly(PA[k],mean)[k] * apply.quarterly(UC[k],mean)[k] * G[k]*apply.quarterly(CS[k],mean)[k] - apply.quarterly(UC[k],mean)[k] -G[k] - apply.quarterly(PA[k],mean)[k]*G[k] - apply.quarterly(UC[k],mean)[k]*G[k]*apply.quarterly(CS[k],mean)[k]))$coefficients
 
    print("m_m params! apply.quarter - EPS,PA,UC,CS w/ CLI delta")
  # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  print(m_m[1]+e*u*m_m[2]+p*c*m_m[3]+u*m_m[4]+c*m_m[5]+o*[6])
  cat("k = "); print(k)

#
# result.eps$coefficients[1]+result.eps$coefficients[2]*eps_year_xts["2019-01"]+result.eps$coefficients[3]*as.vector((last(PA)))+result.eps$coefficients[4]*as.vector((last(CS)))+result.eps$coefficients[5]*as.vector((last(UC)))+result.eps$coefficients[6]*as.vector(last(diff(cli_xts$oecd, lag = 6)))

# 
}
