#
# No long don't use this file. functions are integrated in to em_region.r
#
l <- 4.76
d <- l+2
nod <- 7

w <- c()
curl <- "https://raw.githubusercontent.com/kaz-ogiwara/covid19/master/data/prefectures.csv"
# curl <- "https://github.com/kaz-ogiwara/covid19/blob/master/data/summary.csv"
cdestfile <- "~/R/R2/covid/tmp.csv"
download.file(curl,cdestfile)
if(system("diff ~/R/R2/covid/tmp.csv ~/R/R2/covid/pref.csv", ignore.stdout = T, ignore.stderr = T)){
  system("cp ~/R/R2/covid/tmp.csv ~/R/R2/covid/pref.csv")
  w <- read.csv("~/R/R2/covid/pref.csv")

# w <- as.xts(w[,c(5,6)],as.Date(paste(w[,1],w[,2],w[,3],sep='-')))
# とりあえずdata.frameを作って必要なデータを抜きす。
  df <- c()

  df <- data.frame(t=as.Date(paste(w[,1],w[,2],w[,3],sep='-')),
                  r=w[,5],
                  p=w[,9])
  #
  # for the case to push back start date
  # df <- df[df$t > as.Date('2020-04-01'),] # might work to throw away all rows before 2020/04/01.
  #
  #　データフレームから県名を抜き出し、unique(w[,5])で県名一覧を作る。
  #　行列を作成し、各県のデータを抜き出し、順次行列にbindしていく。
  #  使用するヒストリカルデータのサイズに合わせたmatrixを初期化する。diff()を取るので先頭はNAが入る。先頭日付は削除する。


  mtx <- matrix(nrow=length(unique(df$t)[-1]))
  # mtx <- matrix(diff(df$p[df$r == unique(w[,5])[1]]))
  for( i in seq(1,length(unique(w[,5])),1)){
    mtx <- cbind(mtx,diff(df$p[df$r == unique(w[,5])[i]]))
  }
  mtx <- mtx[,-1]　#　初期化時に使用した空の列を削除する。
  #　行列をデータフレームに変換し、そののち日付データを先のデータフレームから抜き出し付加する。
  mdf <-as.data.frame(mtx)
  mdf <-transform(mdf,t=unique(df$t)[-1]) # 差分を取るので先頭はNAが入る。先頭要素は削除する。
  #　データフレームの列名を県名一覧で変更する。
  colnames(mdf)[1:(length(unique(w[,5])))] <- as.character(unique(w[,5]))
  tokyo_death <- mdf[,c(13,48)]  # pick up 13th column for tokyo.
  # 積み上げヒストグラムに適合するようにmelt()を使用して変換する。
  df.melt <- melt(data=mdf, id.vars="t", measure.vars=as.character(unique(w[,5])))
  # head(df.melt)
  df <- df.melt
  g <- ggplot(df, aes(x = t, y = value, fill = variable))
  g <- g + geom_bar(stat = "identity")
  # plot(g)
  png("~/Dropbox/R-script/covid/09em_death.png", width = 1400, height = 600)
  plot(g)
  dev.off()
}
