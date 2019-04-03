#
# https://00819.blogspot.com/2018/09/prepare-data-getsymbols-autoarima.html
#

len_mon <- 48  # # of months to predict
gdp_g_r <- 1.04 # pesumed GDP growth rate
i <- seq(2,len_mon/3,1)  # seq of quarters to predict
d <- as.Date(as.yearqtr(seq(Sys.Date(),as.Date("2100-12-31"),by="quarters")[i])) # pick up the first day of each quarters.

getSymbols("PAYEMS",src="FRED")
PA <- PAYEMS
m_PA <- as.xts(forecast(auto.arima(PA),h=len_mon)$mean[1:len_mon],as.Date(as.yearmon(mondate(index(last(PA)))+seq(1,len_mon,1)),frac=0))[as.Date(as.yearqtr(mondate(index(last(PA)))+seq(3,len_mon,3)),frac=0)]
PAq <- apply.quarterly(PA[k2k],mean)
length(PAq)

getSymbols("UNDCONTSA",src="FRED")
UC <- UNDCONTSA
m_UC <- as.xts(forecast(auto.arima(UC),h=len_mon)$mean[1:len_mon],as.Date(as.yearmon(mondate(index(last(UC)))+seq(1,len_mon,1)),frac=0))[as.Date(as.yearqtr(mondate(index(last(UC)))+seq(3,len_mon,3)),frac=0)]
UCq <- apply.quarterly(UC[k2k],mean)
length(UCq)

getSymbols('SPCS10RSA',src='FRED')
CS <- SPCS10RSA
m_CS_2012 <- as.xts(forecast(auto.arima(CS["2012::"]),h=len_mon)$mean[1:len_mon],as.Date(as.yearmon(mondate(index(last(CS)))+seq(1,len_mon,1)),frac=0))[as.Date(as.yearqtr(mondate(index(last(CS)))+seq(3,len_mon,3)),frac=0)]
CSq <- apply.quarterly(CS[k2k],mean)
length(CSq)

getSymbols("GDP",src="FRED")
G <- GDP
# m_GDP <- as.xts(as.vector(last(GDP)) * r**(i/4),d)
m_GDP <- as.xts(as.vector(last(G)) * gdp_g_r**(seq(1,len_mon/3,1)/4),as.Date(as.yearqtr(mondate(index(last(G)))+seq(3,len_mon,3)),frac=0))
kikan <- paste("1992-01-01::",as.Date(as.yearmon((mondate(index(last(G)))+2)),frac=1),sep="")
k2k <- paste("2000-01-01::",as.Date(as.yearmon((mondate(index(last(G)))+2)),frac=1),sep="")

SP5 <- as.xts(read.zoo(read.csv("~/SP5.csv")))

length(CSq)
length(UCq)
length(PAq)

# summary(lm(apply.quarterly(SP5[k2k],mean)[,1] ~ PAq[k2k] * UCq[k2k] * G[k2k]*CSq[k2k] - UCq[k2k] -G[k2k] - PAq[k2k]*G[k2k] - UCq[k2k]*G[k2k]*CSq[k2k]))
# my_sp5cs(k2k,m_GDP[d[1:9]],m_PA[d[1:9]],m_UC[d[1:9]],m_CS_2012[d[1:9]])
# result.eps <- lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean))
# result.gpuc <- lm(apply.quarterly(SP5[k2k],mean)[,1] ~ PAq[k2k] * UCq[k2k] * G[k2k]*CSq[k2k] - UCq[k2k] -G[k2k] - PAq[k2k]*G[k2k] - UCq[k2k]*G[k2k]*CSq[k2k])
# summary(lm(apply.quarterly(SP5[k2k],mean)[,1] ~ PAq[k2k] * UCq[k2k] * G[k2k]*CSq[k2k] - UCq[k2k] -G[k2k] - PAq[k2k]*G[k2k] - UCq[k2k]*G[k2k]*CSq[k2k]))
# SP5.result <- merge(residuals(result.gpuc),predict(result.gpuc),residuals(result.eps),predict(result.eps))
#
# GSPC.predict <- merge(to.monthly(GSPC)[substr(k2k,11,23)],last(spline(seq(1,length(SP5.result[,1]),1),as.vector(SP5.result[,2]),n=length(SP5.result[,1])*3+1)$y,n=length(to.monthly(GSPC)[,1][substr(k2k,11,23)])),last(spline(seq(1,length(SP5.result[,1]),1),as.vector(SP5.result[,4]),n=length(SP5.result[,1])*3+1)$y,n=length(to.monthly(GSPC)[,1][substr(k2k,11,23)])),suffixes=c('','spline','eps'))
