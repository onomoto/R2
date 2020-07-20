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
# length_graph <- 110　# グラフは過去60日間が対象
length_graph <- length(seq(as.Date("2020-03-20"),Sys.Date(),by='days')) # 2020/3/20 start
remove(df)

# curl <- "https://raw.githubusercontent.com/kaz-ogiwara/covid19/master/data/summary.csv"
curl <- "https://www.mhlw.go.jp/content/pcr_positive_daily.csv"
# curl <- "https://github.com/kaz-ogiwara/covid19/blob/master/data/summary.csv"
cdestfile <- "~/R/R2/covid/tmp.csv"
download.file(curl,cdestfile)
if(system("diff ~/R/R2/covid/tmp.csv ~/R/R2/covid/all_daily.csv", ignore.stdout = T, ignore.stderr = T)){
  print("****** found update at 全国新規陽性者数 ***********")
  system("cp ~/R/R2/covid/tmp.csv ~/R/R2/covid/all_daily.csv")
  w <- read.csv("~/R/R2/covid/all_daily.csv")
  w[,1] <- gsub("/","-",w[,1])
  w <- as.xts(w[,2],as.Date(w[,1]))
  j <- c()
  for(i in seq(1,length(w[,1]),1)){
    j[i] <- sum(w[1:i,1])

  }
  w[,1] <- j
  # w <- w[c(1,2,3,4,9,10,11,12,14,15)]
  # w <- as.xts(w[,4],as.Date(paste(w[,1],w[,2],w[,3],sep='-')))
  # w <- as.xts(w[,c(4,5,6,7,8,9,10)],as.Date(paste(w[,1],w[,2],w[,3],sep='-')))
  colnames(w)[1] <- "positive"
  last(w,12)
  len <- length(w[,1])
  # len-4
  # len-5
  # len-9
  k <- c()
  for(i in seq(2*nod+1,len,1)) {
   k <- append(k,((log10(sum(diff(w[,1])[(i-nod+1):i]))) - log10(sum(diff(w[,1])[(i-2*nod+1):(i-nod)])))/nod)
  }
  r <- k**2*(l*d) + k*(l+d) +1
  w <- merge(w,as.xts(r,last(index(w),length(r))))
  # as.xts(as.vector(round(last((na.omit(filter(diff(w[,1]),rep(1,7))/7)),length_graph),2)),as.Date(last(index(w),length_graph)))

  colnames(w)[2] <- "effective_repro"
  # last(w,len-11)
  # plot(last(w[,8],60))

  # R と新規感染者数を混在させるためスケール調整のために係数を計算する。
  multi <- (max(na.omit(last(diff(w[,1]),length_graph))) / max(na.omit(last(w[,2],length_graph))))

  df <- data.frame(p=last(diff(w[,1]),length_graph),
                   r=last(w[,2],length_graph)*multi,
                   t=as.Date(last(index(w),length_graph)),
                   m=as.vector(round(last((na.omit(filter(as.vector(diff(w[,1])),rep(1,7))/7)),length_graph),2)))

  colnames(df)[1] <- 'p'
  colnames(df)[2] <- 'r'
  p <- ggplot(df,aes(x=t))
  p <- p + geom_bar(aes(y=p),stat="identity", colour="blue",fill="blue")
  p <- p + geom_path(aes(y=r),colour='red')
  p <- p + geom_path(aes(y=m),colour='green')
  p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  p <- p+annotate("text",label=as.character("1.0"),x=as.Date(df$t[length_graph]), y=10+1*multi,colour='red')

  p <- p + geom_hline(yintercept = 1*multi,size=0.5,linetype=2,colour="red",alpha=1)
  p <- p+annotate("text",label=as.character("2.0"),x=as.Date(df$t[length_graph]), y=10+2*multi,colour='red')
  # (max(na.omit(last(diff(w[,1]),length_graph))) / max(na.omit(last(w[,8],length_graph)))),colour='red')
  p <- p + geom_hline(yintercept = 2*multi,size=0.5,linetype=2,colour="red",alpha=1)
  # plot(p)

  png("~/Dropbox/R-script/covid/02all.png", width = 800, height = 600)
  plot(p)
  dev.off()
}
