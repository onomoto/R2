# データは
# https://catalog.data.metro.tokyo.lg.jp/dataset/t000010d0000000068/resource/c2d997db-1450-43fa-8037-ebb11ec28d4c
# から取得した「リソース」ボタンを押すこと。
# データは公表年月日データが記録されれているだけなので、これをapply.daily()を使用して日毎に集計する必要がある。結果は日々差分データとなる。
# nod は平滑化のための期間パラメータ（日数）である。ここでは　7　とする。
# Kは、（十進LOG（過去nod日間の新規感染確認者数計）ー　十進LOG(さらにその前のnod日間の新規感染確認者数計））／ nod
# L = 平均潜伏期間 = 4.76 (7 in china)
# from https://www.fukuishimbun.co.jp/articles/-/1071227
# 福井県内で新型コロナウイルスの感染が確認された人のうち、ほかの感染者と接触し発症するまでの潜伏期間が推定できるケースの平均値は「４．７６日」だった。
# D = 平均潜伏感染期間 = 9 あるいはL＋2。2は中国での実測値。
# R = K^2*(L*D)+K*(L+D)+1

l <- 4.76
d <- l+2
nod <- 7


w <- c()
# w <- read.csv("~/R/R2/covid/summary.csv")
# https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv
curl <- "https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv"
cdestfile <- "~/R/R2/covid/tokyo.csv"
download.file(curl,cdestfile)
w <- read.csv("~/R/R2/covid/tokyo.csv")

w <- apply.daily(as.xts(rep(1,length(w[,1])),as.Date(w[,5])),sum)
last(w)

# as.xts(15,Sys.Date())
# w <- append(w,as.xts(3,Sys.Date()))

# w <- w[c(1,2,3,4,9,10,11,12,14,15)]
# w <- as.xts(w[,c(4,5,6,7,8,9,10)],as.Date(paste(w[,1],w[,2],w[,3],sep='-')))
# colnames(w)[1] <- "positive"
# colnames(w)[2] <- "hospitalized"
# colnames(w)[3] <- "asymptom"
# colnames(w)[4] <- "discharged"
# colnames(w)[5] <- "discharged-incheck"
# colnames(w)[6] <- "deceased"
# colnames(w)[7] <- "deceased-incheck"
# last(w,12)
len <- length(w[,1])
# len-4
# len-5
# len-9
k <- c()
for(i in seq(2*nod+1,len,1)) {
 k <- append(k,((log10(sum(w[,1][(i-nod+1):i]))) - log10(sum(w[,1][(i-2*nod+1):(i-nod)])))/nod)
 # cat(i)
 # cat(" 1: ")
 # cat((sum(diff(w[,1])[(i-nod+1):i])))
 # cat(" 2: ")
 # cat(sum(diff(w[,1])[(i-2*nod+1):(i-nod)]))
 # print(last(k))
}
r <- round(k**2*(l*d) + k*(l+d) +1,2)
w <- merge(w,as.xts(r,last(index(w),length(r))))
w


colnames(w)[2] <- "effective_repro"
last(w,len-11)
# plot(last(w[,2],60))

# R と新規感染者数を混在させるためスケール調整のために係数を計算する。
multi <- (max(na.omit(last(w[,1],length_graph))) / max(na.omit(last(w[,2],length_graph))))
length_graph <- 60　# グラフは過去45日間が対象
# merge(w,)

# as.xts(as.vector(round(last((na.omit(filter(w[,1],rep(1,7))/7)),length_graph),2)),as.Date(last(index(w),length_graph)))
w <- merge(w,as.xts(as.vector(round(last((na.omit(filter(w[,1],rep(1,7))/7)),length_graph),2)),as.Date(last(index(w),length_graph))))
w[,2] <- w[,2]*multi
positive <- as.vector(last(w[,1],length_graph))
repro <- as.vector(round(last(w[,2],length_graph),2))
moving_a <- as.vector(last(w[,3],length_graph))
date <- as.Date(last(index(w),length_graph))


df <- data.frame(
                p=positive,
                r=repro,
                m=moving_a,
                # s=seq(1,45,1))
                t=date)


# df <- data.frame(p=as.vector(last(w[,1],length_graph)),
#                  # r=as.vector(last(w[,2],length_graph)*multi),
#                  r=as.vector(last(w[,2],length_graph)),
#                  # m=as.vector(last(w[,3],length_graph)),
#                  t=as.Date(last(index(w),length_graph)))
#
# colnames(df)[1] <- 'p'
# colnames(df)[2] <- 'r'
# colnames(df)[3] <- 'm'
# colnames(df)[4] <- 't'

p <- ggplot(df,aes(x=t))

p <- p + geom_bar(aes(y=p),stat="identity", colour="limegreen",fill="limegreen")
# p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%M")
p <- p + geom_path(aes(y=r),colour='red')
p <- p + geom_path(aes(y=m),colour='blue')
# p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%M")


p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
p <- p+annotate("text",label=as.character("1.0"),x=as.Date(df$t[length_graph]), y=5+1*multi,colour='black')
p <- p + geom_hline(yintercept = 1*multi,size=0.5,linetype=2,colour="red",alpha=1)
p <- p+annotate("text",label=as.character("2.0"),x=as.Date(df$t[length_graph]), y=5+2*multi,,colour='black')
p <- p + geom_hline(yintercept = 2*multi,size=0.5,linetype=2,colour="red",alpha=1)
png("03tokyo.png", width = 800, height = 600)
plot(p)
dev.off()
