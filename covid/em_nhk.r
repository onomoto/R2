l <- 4.76
d <- l+2
nod <- 7

pref_en <- c("01Hokkaido","02Aomori","03Iwate","04Miyagi","05Akita","06Yamagata","07Fukushima","08Ibaraki","09Tochigi","10Gunma","11Saitama","12Chiba","13Tokyo","14Kanagawa","15Niigata","16Toyama","17Ishikawa","18Fukui","19Yamanashi","20Nagano","21Gifu","22Shizuoka","23Aichi","24Mie","25Shiga","26Kyoto","27Osaka","28Hyogo","29Nara","30Wakayama","31Tottori","32Shimane","33Okayama","34Hiroshima","35Yamaguchi","36Tokushima","37Kagawa","38Ehime","39Kochi","40Fukuoka","41Saga","42Nagasaki","43Kumamoto","44Oita","45Miyazaki","46Kagoshima","47Okinawa")
pref_jp <- c("北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県","茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県","新潟県","富山県","石川県","福井県","山梨県","長野県","岐阜県","静岡県","愛知県","三重県","滋賀県","京都府","大阪府","兵庫県","奈良県","和歌山県","鳥取県","島根県","岡山県","広島県","山口県","徳島県","香川県","愛媛県","高知県","福岡県","佐賀県","長崎県","熊本県","大分県","宮崎県","鹿児島県","沖縄県")
pref_lab <- c("Hokkaido","Aomori","Iwate","Miyagi","Akita","Yamagata","Fukushima","Ibaraki","Tochigi","Gunma","Saitama","Chiba","Tokyo","Kanagawa","Niigata","Toyama","Ishikawa","Fukui","Yamanashi","Nagano","Gifu","Shizuoka","Aichi","Mie","Shiga","Kyoto","Osaka","Hyogo","Nara","Wakayama","Tottori","Shimane","Okayama","Hiroshima","Yamaguchi","Tokushima","Kagawa","Ehime","Kochi","Fukuoka","Saga","Nagasaki","Kumamoto","Oita","Miyazaki","Kagoshima","Okinawa")

pref_pop <- data.frame(data=round(c(5381733,1308265,1279594,2333899,1023119,1123891,1914039,2916976,1974255,1973115,7266534,6222666,13515271,9126214,2304264,1066328,1154008,786740,834930,2098804,2031903,3700305,7483128,1815865,1412916,2610353,8839469,5534800,1364316,963579,573441,694352,1921525,2843990,1404729,755733,976263,1385262,728276,5101556,832832,1377187,1786170,1166338,1104069,1648177,1433566)/1000))

w <- c()
# curl <- "https://raw.githubusercontent.com/kaz-ogiwara/covid19/master/data/prefectures.csv"
# https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv

curl <- "https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv"
cdestfile <- "~/R/R2/covid/nhk.csv"
download.file(curl,cdestfile)

# w <- read.csv("~/Downloads/nhk.csv")
w <- read.csv(cdestfile)
colnames(w)[1] <- "date"
colnames(w)[2] <- "code"
colnames(w)[3] <- "regionname"
colnames(w)[4] <- "region_daily"
colnames(w)[5] <- "region_total"
colnames(w)[6] <- "region_death"
colnames(w)[7] <- "region_death_total"
# start.time<-proc.time()
# for(i in seq(1,length(pref_jp),1)){w[,3] <- gsub(pref_jp[i],pref_en[i],w[,3])}

for(i in seq(1,length(pref_jp),1)){ w[,3][w[,3] == pref_jp[i]] <- pref_en[i]}
# end.time<-proc.time()
# end.time - start.time

# w[,3][w[,3] == pref_jp[i]] <- pref_en[i]

w[,1] <- gsub("/","-",w[,1])

df <- data.frame(t=as.Date(w[,1]),
                 r=w[,3],
                 p=w$region_total)
# df$p[index(df)[df$p == ""] ] <- 0
df$p[index(df)[df$p == "-" | df$p == ""] ] <- 0
df$p[index(df)[is.na(df$p)]] <- 0
df$p <- as.numeric(as.vector(df$p))

testmat <- diff(matrix(df$p,nrow=length(unique(df$t))))
testdf <- transform(as.data.frame(testmat),t=unique(df$t)[-1])
colnames(testdf)[1:(length(unique(w[,3])))]  <- as.character(unique(w[,3]))

mdf <<- testdf
# change colname to align region name from hokkaido to okinawa by putting seq.nonumber
# otherwise ggplot sort region name alphabetically
# for(i in seq(1,47,1)){ colnames(mdf)[i] <-  (paste(sprintf("%02d",i),colnames(mdf)[i],sep=""))}
# colnames(mdf) <- paste0(sprintf("%02d",seq(1,47,1)),colnames(mdf))
## ongoing
# df.melt <- mdf  %>% tidyr::gather(key=variable,value=value,Hokkaido:Okinawa)
df.melt <- mdf  %>% tidyr::gather(variable,value,as.character(colnames(mdf)[-48]))
## ongoing
# df.melt <- melt(data=mdf, id.vars="t", measure.vars=as.character(unique(w[,5])))
head(df.melt)
df <- df.melt
g <- ggplot(df, aes(x = t, y = value, fill = variable))
g <- g + geom_bar(stat = "identity")
# add labels to print regions names properly in legend.
# g <- g + scale_fill_hue(name='regions',labels=as.character(unique(w[,3])))
g <- g + scale_fill_hue(name='regions',labels=pref_jp)
# g <- g + scale_fill_manual(name='regions',values=rainbow(47))
g <- g + guides(fill = guide_legend(reverse = F,order = 2),label = TRUE)
g <- g + theme_gray (base_family = "HiraKakuPro-W3")
plot(g)

# Sys.sleep(6)

df <- data.frame(t=as.Date(w[,1]),
                 r=w[,3],
                 p=w$region_death_total)
# df$p[index(df)[df$p == ""] ] <- 0
df$p[index(df)[df$p == "-" | df$p == ""] ] <- 0
df$p[index(df)[is.na(df$p)]] <- 0
df$p <- as.numeric(as.vector(df$p))

testmat <- diff(matrix(df$p,nrow=length(unique(df$t))))
testdf <- transform(as.data.frame(testmat),t=unique(df$t)[-1])
colnames(testdf)[1:(length(unique(w[,3])))]  <- as.character(unique(w[,3]))

dmdf <<- testdf
# change colname to align region name from hokkaido to okinawa by putting seq.nonumber
# otherwise ggplot sort region name alphabetically
# for(i in seq(1,47,1)){ colnames(mdf)[i] <-  (paste(sprintf("%02d",i),colnames(mdf)[i],sep=""))}
# colnames(mdf) <- paste0(sprintf("%02d",seq(1,47,1)),colnames(mdf))
## ongoing
# df.melt <- mdf  %>% tidyr::gather(key=variable,value=value,Hokkaido:Okinawa)
df.melt <- dmdf  %>% tidyr::gather(variable,value,as.character(colnames(dmdf)[-48]))
## ongoing
# df.melt <- melt(data=mdf, id.vars="t", measure.vars=as.character(unique(w[,5])))
head(df.melt)
df <- df.melt
g <- ggplot(df, aes(x = t, y = value, fill = variable))
g <- g + geom_bar(stat = "identity")
# add labels to print regions names properly in legend.
# g <- g + scale_fill_hue(name='regions',labels= as.character(unique(w[,3])) )
g <- g + scale_fill_hue(name='regions',labels=pref_jp)
# g <- g + scale_fill_manual(name='regions',values=rainbow(47))
g <- g + guides(fill = guide_legend(reverse = F,order = 2),label = TRUE)
g <- g + theme_dark(base_family = "HiraKakuPro-W3")
plot(g)

df <- df.melt
g <- ggplot(df, aes(x = t, y = value, fill = variable))
g <- g + geom_bar(stat = "identity",position='fill')
# add labels to print regions names properly in legend.
# g <- g + scale_fill_hue(name='regions',labels= as.character(unique(w[,3])) )
g <- g + scale_fill_hue(name='regions',labels=pref_jp)
# g <- g + scale_fill_manual(name='regions',values=rainbow(47))
g <- g + guides(fill = guide_legend(reverse = F,order = 2),label = TRUE)
g <- g + theme_dark(base_family = "HiraKakuPro-W3")
# plot(g)
plot(g)


colnames(mdf) <- c(pref_jp,"t")
colnames(dmdf) <- c(pref_jp,"t")

apply(last(dmdf[,-48],45)[1:30,],2,sum)
apply(last(mdf[,-48],45)[1:30,],2,sum)
round(sort((apply(last(dmdf[,-48],45)[1:30,],2,sum)  / apply(last(mdf[,-48],45)[1:30,],2,sum) )*100)[47:43],digits = 2)
round(100*apply(last(dmdf[,-48],30),2,sum) / apply(last(mdf[,-48],45)[1:30,],2,sum),digits = 2)

as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / apply(last(mdf[,-48],45)[1:30,],2,sum),digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)
as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)
# cbind(as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[order(as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T),1],
#       pref_jp[order(as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T)])
#
#

# cbind(pref_jp[order(as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T)],
#       as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[order(as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T),1])
#
#
#
# cbind(pref_jp[order(as.data.frame(as.vector(round(100*apply(last(mdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T)],
#       as.data.frame(as.vector(round(100*apply(last(mdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[order(as.data.frame(as.vector(round(100*apply(last(mdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T),1])

data.frame(r=pref_jp[order(as.data.frame(as.vector(round(100*apply(last(mdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T)],
d=as.data.frame(as.vector(round(100*apply(last(mdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[order(as.data.frame(as.vector(round(100*apply(last(mdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T),1])


data.frame(r=pref_jp[order(as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T)],
d=as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[order(as.data.frame(as.vector(round(100*apply(last(dmdf[,-48],30),2,sum) / pref_pop,digits = 2)),pref_jp,col.name=c(1,2),col.names=c("1","2"),optional = T)[,1],decreasing = T),1])


colnames(mdf) <- c(pref_en,"t")
colnames(dmdf) <- c(pref_en,"t")
