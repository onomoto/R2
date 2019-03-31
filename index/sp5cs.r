

# lm(apply.quarterly(SP5[kikan],mean)[,1] ~ PAq[kikan] * UCq[kikan] * G[kikan]*CSq[kikan] - UCq[kikan] -G[kikan] - PAq[kikan]*G[kikan] - UCq[kikan]*G[kikan]*CSq[kikan])
# Coefficients:
#                                             Estimate Std. Error t value Pr(>|t|)
# (Intercept)                               -1.250e+03  4.070e+02  -3.071  0.00311 **
# PAq[kikan]:UCq[kikan]                      8.127e-05  6.593e-06  12.327  < 2e-16 ***
# PAq[kikan]:CSq[kikan]                     -2.300e-04  3.765e-05  -6.109 6.27e-08 ***
# PAq[kikan]:UCq[kikan]:G[kikan]            -5.465e-09  4.813e-10 -11.355  < 2e-16 ***
# PAq[kikan]:UCq[kikan]:CSq[kikan]          -1.731e-07  3.413e-08  -5.070 3.54e-06 ***
# PAq[kikan]:G[kikan]:CSq[kikan]             2.503e-08  1.927e-09  12.988  < 2e-16 ***
# PAq[kikan]:UCq[kikan]:G[kikan]:CSq[kikan]  9.177e-12  1.954e-12   4.697 1.41e-05 ***
# k = duration
# g = gdp
# p = PAYEMS
# u = UNDCONTSA
# c = case-shhiller home price index

my_sp5cs <- function(k,g,p,u,c)
{

# summary(lm(apply.quarterly(SP5[k2k],mean)[,1] ~ PAq[k2k] * UCq[k2k] * G[k2k]*CSq[k2k] - UCq[k2k] -G[k2k] - PAq[k2k]*G[k2k] - UCq[k2k]*G[k2k]*CSq[k2k]))

  m_m <- summary(lm(apply.quarterly(SP5[k],mean)[,1] ~ apply.quarterly(PA[k],mean)[k] * apply.quarterly(UC[k],mean)[k] * G[k]*apply.quarterly(CS[k],mean)[k] - apply.quarterly(UC[k],mean)[k] -G[k] - apply.quarterly(PA[k],mean)[k]*G[k] - apply.quarterly(UC[k],mean)[k]*G[k]*apply.quarterly(CS[k],mean)[k]))$coefficients

    print("m_m params! apply.quarter - UC w/ nominal GDP")
  # print(m_m[1]+y*m_m[2]+x*m_m[3]+y*z*m_m[4]+y*x*m_m[5]+z*x*m_m[6]+y*z*x*m_m[7])
  print(m_m[1]+p*u*m_m[2]+p*c*m_m[3]+p*u*g*m_m[4]+p*u*c*m_m[5]+p*g*c*m_m[6]+g*p*u*c*m_m[7])
  cat("k = "); print(k)

  # print("from 1992 till 2016Q3")
  # print(2.737e+04+y*-2.913e-01+z*-9.770e+00+x*-1.957e+00+y*z*1.925e-04+y*x*2.130e-05+z*x*-3.003e-04+y*z*x*-6.143e-09)
}
