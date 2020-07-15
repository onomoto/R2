# 東京の感染者数ヒストリカルデータから日次の年齢別ヒストグラムを作る。事前に
# source("../../Dropbox/R-script/covid/tokyo_effective_repro.r") を実行し、
# 　   curl <- "https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv"
# 　   cdestfile <- "~/R/R2/covid/tokyo.csv"
# 　   download.file(curl,cdestfile)
# 最新のcsvをダウンロードしておくこと。

# インデックスは、大体最終の死亡者の10倍になるように若い人は人数に0.02
# 50代は 0.04 60代は 0.15, 70代は 0.56 それ以上は1 の係数をかけて加算。
func <- function(x1,x2,x3,x4,x5,x6,x7,x8){
  return(x1*0.02+x2*0.02+x3*0.02+x4*0.02+x5*0.04+x6*0.15+x7*0.56+x8*1)
}

# length_graph <- length(seq(as.Date("2020-03-20"),Sys.Date(),by='days'))
w <- read.csv("~/R/R2/covid/tokyo.csv")
y <- as.xts(as.numeric(substr(w[,9],1,2)),as.Date(w[,5]))
# apply.daily(as.xts(rep(1,length(y[y[,1] == 10])),as.Date(index(y[y[,1] == 10]))),sum)
length_graph <- length(seq(as.Date("2020-03-20"),last(index(y)),by='days'))

# v <- c()
# seq(as.Date(w[1,5]),Sys.Date(),by='days')
v <- as.xts(rep(1,length( seq(as.Date(w[1,5]),last(index(y)),by='days') )), seq(as.Date(w[1,5]),last(index(y)),by='days'))
for(i in seq(10,80,10)){
  if(i < 80){
    v<- merge(v,  apply.daily(as.xts(rep(1,length(y[y[,1] == i])),as.Date(index(y[y[,1] == i]))),sum))
  }else{  # for the case the sample is more than 80 yrs old.
    v<- merge(v,  apply.daily(as.xts(rep(1,length(y[y[,1] >= i])),as.Date(index(y[y[,1] >= i]))),sum))
  }
}
# v<- merge(v,  apply.daily(as.xts(rep(1,length(y[y[,1] > 80 ])),as.Date(index(y[y[,1] >80]))),sum))
v <- v[,-1]
for(i in seq(1,8,1)){
  colnames(v)[i] <- as.character(i*10)
}

# replace NA with ZERO.
for( i in seq(1,length(colnames(v)),1)) {
  for(j in seq(1,length(index(v)),1)) {
      if(is.na(v[j,i])){  v[j,i] <- 0}
  }
  # print(v[,i])
}

# for( i in length(colnames(v))){
#  replace(v[,i], which(is.na(v[,i])), 0)
# }

# apply(v[,c(6,7,8)],1,sum)
w <- merge(as.xts(apply(v[,c(1,2,3,4,5)],1,sum) ,index(v)),as.vector(apply(v[,c(6,7,8)],1,sum) ))

colnames(w)[1] <- "lessthan60"
colnames(w)[2] <- "over60"

df <- data.frame(t=last(index(w),length_graph),
                 # o=last(diff(all[,1]-all[,3],t=all[,3]),length_graph),
                 o=last(w[,1],length_graph),
                 k=last(w[,2],length_graph)
                 # or=last(all[,2],length_graph)*multi,
                 # kr=last(all[,2],length_graph)*multi
               )
# df.melt <- melt(data=df, id.vars="t", measure.vars=c("lessthan60", "over60"))


df.melt <- melt(data=df, id.vars="t", measure.vars=c(colnames(w)[1],colnames(w)[2]))
# head(df.melt)
df <- df.melt
               # g <- ggplot(x, aes(x = t, y = value, fill = variable))
               # # g <- ggplot(x, aes(x = t, y = d))
               # g <- g + geom_bar(stat = "identity")
               # # g <- g + scale_fill_nejm()
g <- ggplot(df, aes(x = t, y = value, fill = variable))
g <- g + geom_bar(stat = "identity")
# plot(g)

df <- data.frame(t=last(index(v),length_graph),
                 # o=last(diff(all[,1]-all[,3],t=all[,3]),length_graph),
                 a=last(v[,1],length_graph),
                 b=last(v[,2],length_graph),
                 c=last(v[,3],length_graph),
                 d=last(v[,4],length_graph),
                 e=last(v[,5],length_graph),
                 f=last(v[,6],length_graph),
                 g=last(v[,7],length_graph),
                 h=last(v[,8],length_graph)
                 # or=last(all[,2],length_graph)*multi,
                 # kr=last(all[,2],length_graph)*multi
               )

for(i in seq(1,8,1)){
                 colnames(df)[i+1] <- as.character(i*10)
}

# df.melt <- melt(data=df, id.vars="t", measure.vars=c("X10", "X20", "X30", "X40", "X50", "X60", "X70", "X80"))
df.melt <- melt(data=df, id.vars="t", measure.vars=c("10", "20", "30", "40", "50", "60", "70", "80"))
# head(df.melt)
df <- df.melt
# in order to overlayer graph use ggplot(NULL) to create base object.
g <- ggplot(NULL)
# g <- ggplot(df, aes(x = t, y = value, fill = variable))
g <- g + scale_fill_brewer(palette="Spectral",na.value = "black",name = "age group", direction=-1,labels = c("=<19",">=20",">=30",">=40",">=50",">=60",">=70",">=80"))
g <- g + geom_bar(data=df,aes(x = t, y = value, fill = variable),stat = "identity")

# prepare the second layer.
df <- data.frame(t=last(index(v),length_graph),
                value=last(mapply(func,v[,1],v[,2],v[,3],v[,4],v[,5],v[,6],v[,7],v[,8]),length_graph)
)
# df <- df[-length(df[,1]),]  # cut off the last entry.
# g <- ggplot(df, aes(x = t, y = value))
g <- g+geom_line(data=df, aes(x = t, y = value))

# plot(g)
png("~/Dropbox/R-script/covid/05tokyo_age.png", width = 1200, height = 800)
plot(g)
dev.off()