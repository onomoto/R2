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

length_graph <- 90　# グラフは過去length_graph日間が対象
l <- 4.76
d <- l+2
nod <- 7

# w <- read.csv("~/R/R2/covid/summary.csv")
w <- read.csv("~/R/R2/covid/tokyo.csv")
w <- apply.daily(as.xts(rep(1,length(w[,1])),as.Date(w[,5])),sum)


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
tokyo <- w
colnames(tokyo)[1] <- "positive"
colnames(tokyo)[2] <- "effective_repro"
last(tokyo,10)
w <- c();for(i in seq(1,length(tokyo[,1]),1)){ w <- append(w,sum(as.vector(tokyo[,1])[1:i]))}
tokyo[,1] <- w
last(tokyo,10)

# データは https://raw.githubusercontent.com/kaz-ogiwara/covid19/master/data/summary.csv から取得した。
#  [1] "年"                                       "月"
#  [3] "日"                                       "PCR検査陽性者"
#  [5] "PCR検査実施人数"                          "有症状者"
#  [7] "無症状者"                                 "症状有無確認中"
#  [9] "入院治療を要する者"                       "入院治療を要する者.無症状."
# [11] "退院者"                                   "退院者.突合作業中を含む."
# [13] "人工呼吸器又は集中治療室に入院している者" "死亡者"
# [15] "死亡者.突合作業中を含む."                 "URL"

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


w <- read.csv("~/R/R2/covid/summary.csv")
w <- w[c(1,2,3,4,9,10,11,12,14,15)]
w <- as.xts(w[,4],as.Date(paste(w[,1],w[,2],w[,3],sep='-')))
colnames(w)[1] <- "positive"
# colnames(w)[2] <- "hospitalized"
# colnames(w)[3] <- "asymptom"
# colnames(w)[4] <- "discharged"
# colnames(w)[5] <- "discharged-incheck"
# colnames(w)[6] <- "deceased"
# colnames(w)[7] <- "deceased-incheck"
last(w,12)
len <- length(w[,1])
# len-4
# len-5
# len-9
# k <- c()
# for(i in seq(2*nod+1,len,1)) {
#  k <- append(k,((log10(sum(diff(w[,1])[(i-nod+1):i]))) - log10(sum(diff(w[,1])[(i-2*nod+1):(i-nod)])))/nod)
# }
# r <- round(k**2*(l*d) + k*(l+d) +1,2)
# w <- merge(w,as.xts(r,last(index(w),length(r))))
# as.xts(as.vector(round(last((na.omit(filter(diff(w[,1]),rep(1,7))/7)),length_graph),2)),as.Date(last(index(w),length_graph)))
#
# colnames(w)[2] <- "effective_repro"

zenkoku <- w[,1]

tokyo <- tokyo[paste("::",as.character(last(index(zenkoku))),sep="")]


all <- merge(zenkoku,tokyo)["2020-03-01::"]

for(j in seq(1,length(colnames(all)),1)){
  for(i in seq(1,length(index(all)),1)){ if(is.na(all[i,j])){all[i,j] <- all[i-1,j]} }
}

for(j in seq(3,length(colnames(all)),1)){
  # for(i in seq(1,length(index(all))-1,1)){ all[i+1,j] <- all[i,j] }
  # 東京と全国データでは日付が1日ずれているのでそれを修正する。5/1付の東京は5/2の全国に対応する。
  #　また逐次的に処理すると全てのデータが塗り潰されるので注意！
  all[2:(length(index(all))),j] <- as.vector(all[1:(length(index(all))-1),j])
}

# for(j in seq(1,length(colnames(all)),1)){
#   for(i in seq(1,length(index(all)),1)){ if(all[i,j] < 0){all[i,j] <- 0} }
# }

# multi <- max(last(diff(all[,1]),length_graph)) / max(last(all[,c(2,4)],length_graph))
colnames(all)[1] <- 'others'
colnames(all)[2] <- 'tokyo'
  # merge(last(diff(all[,1]),length_graph),last(diff(all[,3]),length_graph))

# df <- data.frame(t=index(all),o=all[,1]-all[,3],t=all[,3])

df <- data.frame(t=last(index(all),length_graph),
                 # o=last(diff(all[,1]-all[,3],t=all[,3]),length_graph),
                 o=last(diff(all[,1]-all[,2]),length_graph),
                 k=last(diff(all[,2]),length_graph)
                 # or=last(all[,2],length_graph)*multi,
                 # kr=last(all[,2],length_graph)*multi
               )

               # df <- data.frame(t=last(index(all),length_graph),
               #                  # o=last(diff(all[,1]-all[,3],t=all[,3]),length_graph),
               #                  o=last(diff(all[,1]-all[,3]),length_graph)[-length_graph],
               #                  k=last(diff(all[,3]),length_graph)[-length_graph]
               #                  # or=last(all[,2],length_graph)*multi,
               #                  # kr=last(all[,2],length_graph)*multi
               #                )
df.melt <- melt(data=df, id.vars="t", measure.vars=c("others", "tokyo"))
head(df.melt)
df <- df.melt
# g <- ggplot(x, aes(x = t, y = value, fill = variable))
# # g <- ggplot(x, aes(x = t, y = d))
# g <- g + geom_bar(stat = "identity")
# # g <- g + scale_fill_nejm()
g <- ggplot(df, aes(x = t, y = value, fill = variable))
g <- g + geom_bar(stat = "identity")
# p <- p + geom_path(aes(y=or),colour='red')
# p <- p + geom_path(aes(y=kr),colour='blue')
# plot(g)

png("01tokyo_vs_other.png", width = 1400, height = 600)
plot(g)
dev.off()
