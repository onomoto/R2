

# k = duration
# e = eps
# p = PAYEMS
# u = UNDCONTSA
# c = case-shhiller home price index
# o = oecd composite leading indicator for oecd countries given month delta. the default is 5. the value like 0.5,0.1,0,-0.1,-0.5
# t = target date like "2020-12-01"
# i = number of months to calculate delta. default is 5.
#
# usage and output is like below.
#
# my_sp5eps_seq("2000-01-01::2020-06-30",eps_year_xts["2020::"][4],as.vector(last(PA)),as.vector(last(UC)),as.vector(CS["2019-09-01"]*1.03),0.64,"2020-12-01")
# 84
# [1] "m_m params! apply.quarter - EPS,PA,UC,CS w/ CLI delta"
#                [,1]
# 2020-10-01 3177.852
# k = [1] "2000-01-01::2020-06-30"



my_sp5eps_seq <- function(k,e,p,u,c,o,t,i=5)
{
  k2k <- k  # 0627 added to bug fix.

  l <- length(seq(as.Date(strsplit( k2k,"::")[[1]][1]), as.Date(strsplit( k2k,"::")[[1]][2]),by='quarters')) + length(seq(as.Date(strsplit( k2k,"::")[[1]][2]), as.Date(t)  , by='quarters' ))
  # cat(l)
  # cat("\n")

  m_m <- summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=i)[k2k],mean) + seq(1,length(apply.quarterly(SP5[,4][k2k],mean) )  ,1)))$coefficients
  result.eps <<- m_m

  # print(summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=i)[k2k],mean) + seq(1,length(apply.quarterly(SP5[,4][k2k],mean) )  ,1))))

#  m_m <- summary(lm(apply.quarterly(SP5[k],mean)[,1] ~ apply.quarterly(PA[k],mean)[k] * apply.quarterly(UC[k],mean)[k] * G[k]*apply.quarterly(CS[k],mean)[k] - apply.quarterly(UC[k],mean)[k] -G[k] - apply.quarterly(PA[k],mean)[k]*G[k] - apply.quarterly(UC[k],mean)[k]*G[k]*apply.quarterly(CS[k],mean)[k]))$coefficients
  print("m_m params! apply.quarter - EPS,PA,UC,CS w/ CLI delta")
  # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  return(m_m[1]+e*m_m[2]+p*m_m[3]+u*m_m[4]+c*m_m[5]+o*m_m[6]+l*m_m[7])
  # print(m_m)
  cat("k = "); print(k)
#
}
