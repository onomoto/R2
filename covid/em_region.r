# データは"https://raw.githubusercontent.com/kaz-ogiwara/covid19/master/data/prefectures.csv"
# から取得
# データは各県のデータが日付と県名をキーにシーケンシャルに格納されている。必要なデータだけを抽出、ソートの上使用する。　
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
curl <- "https://raw.githubusercontent.com/kaz-ogiwara/covid19/master/data/prefectures.csv"
cdestfile <- "~/R/R2/covid/tmp.csv"
download.file(curl,cdestfile)
system(" awk 'NR==1' ~/R/R2/covid/tmp.csv > ~/R/R2/covid/tmp2.csv")
system(" awk 'NR>91' ~/R/R2/covid/tmp.csv >> ~/R/R2/covid/tmp2.csv")
if(system("diff ~/R/R2/covid/tmp2.csv ~/R/R2/covid/pref.csv", ignore.stdout = T, ignore.stderr = T)){
  print("****** found update at 全都道府県新規陽性者数 ***********")
  system("cp ~/R/R2/covid/tmp2.csv ~/R/R2/covid/pref.csv")
  # system(" awk 'NR==1' ~/R/R2/covid/tmp.csv > ~/R/R2/covid/pref.csv")
  # system(" awk 'NR>59' ~/R/R2/covid/tmp.csv >> ~/R/R2/covid/pref.csv")
  # curl <- "https://github.com/kaz-ogiwara/covid19/blob/master/data/summary.csv"
  # cdestfile <- "~/R/R2/covid/pref.csv"
  # download.file(curl,cdestfile)
  w <- read.csv("~/R/R2/covid/pref.csv")
  # w <- as.xts(w[,c(5,6)],as.Date(paste(w[,1],w[,2],w[,3],sep='-')))
  # とりあえずdata.frameを作って必要なデータを抜きす。
  df <- c()

  df <- data.frame(t=as.Date(paste(w[,1],w[,2],w[,3],sep='-')),
                  r=w[,5],
                  p=w$testedPositive)
  df$p[index(df)[df$p == ""] ] <- 0
  df$p <- as.numeric(as.vector(df$p))
  #
  # データフレームから県名を抜き出し、unique(w[,5])で県名一覧を作る。
  # 行列を作成し、差分を計算して日次新規陽性者数を計算する。2番目の要素から1番目、3番目の要素から2番目を引く。以下順次最後の要素まで計算する。
  # 計算結果の行列に日付データを付加してdata.frameにする。
  # カラム名をつける。
  #
  # rowsize <- dim(matrix(df$p,ncol=length(unique(w[,5])),byrow=T))[1]  # 行列のサイズを計算し、行数を格納しておく。
  # testmat <- matrix(df$p,ncol=length(unique(w[,5])),byrow=T)[2:rowsize,] - matrix(df$p,ncol=length(unique(w[,5])),byrow=T)[1:(rowsize-1),]　#　CSVから読み込んだデータを行列に格納し、差分を取る。
  testmat <- diff(matrix(df$p,ncol=length(unique(w[,5])),byrow=T))
  testdf <- transform(as.data.frame(testmat),t=unique(df$t)[-1])
  colnames(testdf)[1:(length(unique(w[,5])))]  <- as.character(unique(w[,5]))
  mdf <- testdf

  df.melt <- melt(data=mdf, id.vars="t", measure.vars=as.character(unique(w[,5])))
  head(df.melt)
  df <- df.melt
  g <- ggplot(df, aes(x = t, y = value, fill = variable))
  g <- g + geom_bar(stat = "identity")
  g <- g + scale_fill_hue(name='regions')

  png("~/Dropbox/R-script/covid/04em.png", width = 1400, height = 600)
  plot(g)
  dev.off()


  df <- c()

  df <- data.frame(t=as.Date(paste(w[,1],w[,2],w[,3],sep='-')),
                  r=w[,5],
                  p=w$deaths)
  df$p[index(df)[df$p == ""] ] <- 0             # input csv includes "" entry. replace them with ZERO
  df$p <- as.numeric(as.vector(df$p))

  #
  # データフレームから県名を抜き出し、unique(w[,5])で県名一覧を作る。
  # 行列を作成し、差分を計算して日次新規陽性者数を計算する。2番目の要素から1番目、3番目の要素から2番目を引く。以下順次最後の要素まで計算する。
  # 計算結果の行列に日付データを付加してdata.frameにする。
  # カラム名をつける。
  # rowsize <- dim(matrix(df$p,ncol=length(unique(w[,5])),byrow=T))[1]  # 行列のサイズを計算し、行数を格納しておく。
  # testmat <- matrix(df$p,ncol=length(unique(w[,5])),byrow=T)[2:rowsize,] - matrix(df$p,ncol=length(unique(w[,5])),byrow=T)[1:(rowsize-1),]　#　CSVから読み込んだデータを行列に格納し、差分を取る。
  testmat <- diff(matrix(df$p,ncol=length(unique(w[,5])),byrow=T))
  testdf <- transform(as.data.frame(testmat),t=unique(df$t)[-1])
  colnames(testdf)[1:(length(unique(w[,5])))]  <- as.character(unique(w[,5]))
  dmdf <- testdf

  tokyo_death <- as.xts(dmdf[,colnames(dmdf) == "Tokyo"],dmdf$t)
  # 積み上げヒストグラムに適合するようにmelt()を使用して変換する。
  df.melt <- melt(data=dmdf, id.vars="t", measure.vars=as.character(unique(w[,5])))
  # head(df.melt)
  df <- df.melt
  g <- ggplot(df, aes(x = t, y = value, fill = variable))
  g <- g + geom_bar(stat = "identity")
  # g <- g + scale_fill_brewer(palette="Spectral",na.value = "black",name = "regions")
  g <- g + scale_fill_hue(name='regions')
  # plot(g)
  png("~/Dropbox/R-script/covid/09em_death.png", width = 1400, height = 600)
  plot(g)
  dev.off()
}
