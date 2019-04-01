# when k2k is like
k2k
# [1] "2000-01-01::2018-12-31"
# calculate cli 6 month delta
diff(cli_xts,lag=6)[k2k]
summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=6)[k2k],mean)))

# Call:
# lm(formula = apply.quarterly(SP5[, 4][k2k], mean) ~ eps_year_xts[k2k] +
#     apply.quarterly(PA[k2k], mean) + apply.quarterly(CS[k2k],
#     mean) + apply.quarterly(UC[k2k], mean) + apply.quarterly(diff(cli_xts$oecd,
#     lag = 6)[k2k], mean))
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -154.102  -50.869   -2.623   56.146  165.534
#
# Coefficients:
#                                                           Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                             -9.881e+03  3.509e+02 -28.158  < 2e-16 ***
# eps_year_xts[k2k]                                        5.913e+00  5.475e-01  10.800  < 2e-16 ***
# apply.quarterly(PA[k2k], mean)                           8.689e-02  2.975e-03  29.204  < 2e-16 ***
# apply.quarterly(CS[k2k], mean)                          -5.506e+00  4.333e-01 -12.708  < 2e-16 ***
# apply.quarterly(UC[k2k], mean)                           1.126e-01  3.999e-02   2.816  0.00632 **
# apply.quarterly(diff(cli_xts$oecd, lag = 6)[k2k], mean)  7.684e+01  9.961e+00   7.715 6.12e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 74.13 on 70 degrees of freedom
# Multiple R-squared:  0.9806,	Adjusted R-squared:  0.9792
# F-statistic: 706.6 on 5 and 70 DF,  p-value: < 2.2e-16

result.eps <- lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=6)[k2k],mean))

result.gpuc <- lm(apply.quarterly(SP5[k2k],mean)[,1] ~ PAq[k2k] * UCq[k2k] * G[k2k]*CSq[k2k] - UCq[k2k] -G[k2k] - PAq[k2k]*G[k2k] - UCq[k2k]*G[k2k]*CSq[k2k])

SP5.result <- merge(residuals(result.gpuc),predict(result.gpuc),residuals(result.eps),predict(result.eps))
GSPC.predict <- merge(to.monthly(GSPC)[substr(k2k,11,23)],last(spline(seq(1,length(SP5.result[,1]),1),as.vector(SP5.result[,2]),n=length(SP5.result[,1])*3-2)$y,n=length(to.monthly(GSPC)[,1][substr(k2k,11,23)])),last(spline(seq(1,length(SP5.result[,1]),1),as.vector(SP5.result[,4]),n=length(SP5.result[,1])*3-2)$y,n=length(to.monthly(GSPC)[,1][substr(k2k,11,23)])),suffixes=c('','spline','eps'))


plot(merge(GSPC.predict[,4],GSPC.predict[,7],GSPC.predict[,8],GSPC.predict[,4]-GSPC.predict[,7],GSPC.predict[,4]-GSPC.predict[,8]),main="GSPC.predict[,4] vs. GSPC.predict[,7]",grid.ticks.on='months')
tmp.legend <- "Black: actual \nRed: spline\nGreen: eps"
addLegend(legend.loc = "topleft", legend.names = tmp.legend,col=3)
tmp.addTA <- as.xts(rep(2800,length(index(GSPC.predict))),index(GSPC.predict))
addSeries(tmp.addTA,on=1,col=6,lwd=1)
