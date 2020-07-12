# 東京の感染者数ヒストリカルデータから日次の年齢別ヒストグラムを作る。事前に
# source("../../Dropbox/R-script/covid/tokyo_effective_repro.r") を実行し、
# 　   curl <- "https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv"
# 　   cdestfile <- "~/R/R2/covid/tokyo.csv"
# 　   download.file(curl,cdestfile)
# 最新のcsvをダウンロードしておくこと。

length_graph <- length(seq(as.Date("2020-03-20"),Sys.Date(),by='days'))
w <- read.csv("~/R/R2/covid/tokyo.csv")
y <- as.xts(as.numeric(substr(w[,9],1,2)),as.Date(w[,5]))
apply.daily(as.xts(rep(1,length(y[y[,1] == 10])),as.Date(index(y[y[,1] == 10]))),sum)

# v <- c()
# seq(as.Date(w[1,5]),Sys.Date(),by='days')
v <- as.xts(rep(1,length( seq(as.Date(w[1,5]),Sys.Date(),by='days') )), seq(as.Date(w[1,5]),Sys.Date(),by='days'))
for(i in seq(10,80,10)){
  v<- merge(v,  apply.daily(as.xts(rep(1,length(y[y[,1] == i])),as.Date(index(y[y[,1] == i]))),sum))
}
v <- v[,-1]
for(i in seq(1,8,1)){
  colnames(v)[i] <- as.character(i*10)
}


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
               # g <- ggplot(x, aes(x = t, y = value, fill = variable))
               # # g <- ggplot(x, aes(x = t, y = d))
               # g <- g + geom_bar(stat = "identity")
               # # g <- g + scale_fill_nejm()
g <- ggplot(df, aes(x = t, y = value, fill = variable))
g <- g + scale_fill_brewer(palette="Spectral",na.value = "black",name = "age group", direction=-1,labels = c("=<19",">=20",">=30",">=40",">=50",">=60",">=70",">=80"))
g <- g + geom_bar(stat = "identity")
# plot(g)
png("~/Dropbox/R-script/covid/05tokyo_age.png", width = 1200, height = 800)
plot(g)
dev.off()
