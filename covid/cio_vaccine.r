curl <- "https://vrs-data.cio.go.jp/vaccination/opendata/latest/summary_by_date.csv"
cdestfile <- "~/R/R2/covid/summary_by_date.csv"
download.file(curl,cdestfile)
w <- read.csv(cdestfile)

# colnames(w)[2] <- 'start_per_capita'
# colnames(w)[3] <- 'last_per_capita'
last(w[,1])
apply(w[,c(2,3)],2,sum)
apply(w[,c(2,3)],2,sum) %>% sum()
apply(w[,c(2,3)],1,sum) %>% last(.,14)

df <- w  %>% tidyr::gather(variable,value,2:3)
df$variable <- factor(df$variable,levels=c("count_second_or_full_general","count_first_or_mid_general" )) # factoring variable to set seq.
df$date <- as.Date(df$date) # convert to date class
p <- ggplot()
p <- p + geom_bar(data=df, aes(x = date, y = value,fill = variable),stat = "identity",alpha=0.95)
p <- p + theme_gray (base_family = "HiraKakuPro-W3")
# p <- p + scale_fill_brewer(name="回数",labels=c("1回目","２回目"),palette="Accent")
p <- p+ scale_fill_manual(name="回数",labels=c("2回目","1回目"),values = c("darkblue", "cyan4")) 
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(p)



curl <- "https://vrs-data.cio.go.jp/vaccination/opendata/latest/summary_by_prefecture.csv"
cdestfile <- "~/R/R2/covid/summary_by_region.csv"
download.file(curl,cdestfile)
w <- read.csv(cdestfile,fileEncoding ='shift-jis')
df <- data.frame(w,w[,4]/pref_pop,w[,5]/pref_pop)
colnames(df)[6] <-  "first"
colnames(df)[7] <-  "second"
wdf <- df %>% tidyr::gather(variable,value,6:7)
wdf$variable <- factor(wdf$variable,levels=c("second","first" )) # factoring variable to set seq.

# wdf[,6] <- wdf[,2]
wdf[,2] <- rep(pref_en,2)
df <- wdf 
df[,7] <- df[,7]/10 # convert to percentage.


# p <- p + theme_gray (base_family = "HiraKakuPro-W3")
p <- ggplot(df, aes(y = value, x = prefecture_name, fill = variable))
p <- p + theme_gray (base_family = "HiraKakuPro-W3")
p <- p + theme(axis.text.x = element_text(angle = 270, hjust = 1))
p <- p + geom_bar(stat = "identity")
# p <- p + scale_fill_brewer( name="回数",labels=c("2","1") ) #,palette="Spectral")
p <- p+ scale_fill_manual(name="回数",labels=c("2回目","1回目"),values = c("darkblue", "cyan4")) 
p <- p + scale_x_discrete(limits=unique(df$prefecture_name),label=substr(unique(df$prefecture_name),1,3))
plot(p)
remove(w)
w <- data.frame(t=(df[1:47,7]+df[48:94,7]),r=pref_jp,p=pref_pop[,1])
# w <- cbind(w,lm=predict(lm(w[,1] ~ log(pref_pop[,1]))),total=( df[1:47,4]+df[1:47,5] ))
x1 <- w$p
y1 <- w$t

# w <- cbind(w,lm=predict(lm(w[,1] ~ log(pref_pop[,1]))),total=( df[1:47,4]+df[1:47,5] ))
w <- cbind(w,lm=predict(nls(y1~a*x1^(1/4)+b,start=c(a=1,b=1),trace=TRUE)),total=( df[1:47,4]+df[1:47,5] ))
# v <- apply(mdf[,-48],2,sum)/pref_pop
# w <- cbind(w,ppc=v)

w <- cbind(w,ppc=as.numeric((apply(mdf[,-48],2,sum)/pref_pop)[,1]))

p <- ggplot(w)

p <- p + geom_point(aes(x=p,y=t))
p <- p + geom_point(aes(x=p,y=t,size=total,color=ppc))
# p <- p + geom_point(aes(x=p,y=t,size=total,color=factor(ppc)))
p <- p + geom_line(aes(x=p,y=lm))
p <- p + annotate("text",label=w$r,x=w$p, y=w$t+0.25,colour='black',family = "HiraKakuProN-W3",size=3)
p <- p + theme(text = element_text(size = 12))
p <- p + theme_gray (base_family = "HiraKakuPro-W3")
p <- p + guides(size = guide_legend(title="接種総数"))
p <- p + guides(color = guide_legend(title="人口当り\n感染者数"))
p  <- p + scale_color_gradient( low = "#00FF00",high = "#FF0000")
p <- p + xlab("人口") + ylab("人口あたり接種数")
p <- p + stat_smooth(aes(x=p,y=t),method = lm, formula = y ~ poly(x, 3, raw = TRUE),se=FALSE)
# p <- p + theme(legend.position = 'none')
# p <- p +  guides(color=FALSE)
# p + scale_color_discrete(guide=FALSE)
plot(p)

curl <- "https://vrs-data.cio.go.jp/vaccination/opendata/latest/prefecture.ndjson"
cdestfile <- "~/R/R2/covid/prefecture.ndjson"
download.file(curl,cdestfile)
js <- jsonlite::stream_in(gzfile(cdestfile) )
last(js)

df <- cbind(js,cat=paste(js[,6],js[,3],js[,4],sep='-'))
df$cat2 <- factor(df$cat,levels=c("2-F--64", "2-F-65-", "2-M--64", "2-M-65-", "2-U--64", "2-U-65-", "2-U-UNK","1-F--64", "1-F-65-", "1-M--64", "1-M-65-", "1-M-UNK", "1-U--64", "1-U-65-", "1-U-UNK"))  # factoring cat to set seq.
df$date <- as.Date(df$date)

p <- ggplot(df, aes(y = count, x = prefecture, fill = cat2))
p <- p + theme_dark (base_family = "HiraKakuPro-W3")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + theme(panel.background = element_rect(fill = "black",
                                               colour = "lightblue"),
               legend.key = element_rect(fill='black',colour='white'))

p <- p + scale_fill_manual(name="分類",values=rainbow(15),
                           label=c("2回目女性64歳以下","2回目女性65歳以上","2回目男性64歳以下","2回目男性65歳以上","2回目性別不明64歳以下","2回目性別不明65歳以上","2回目性別不明年齢不明",
                                   "1回目女性64歳以下","1回目女性65歳以上","1回目男性64歳以下","1回目男性65歳以上","1回目男性年齢不明","1回目性別不明64歳以下","1回目性別不明65歳以上","1回目性別不明年齢不明"))


p <- p + geom_bar(stat = "identity")
# p <- p + theme(legend.position = 'none')
p <- p + scale_x_discrete(label=substr(pref_jp,1,3))
plot(p)



p <- ggplot(df, aes(y = count, x = date, fill = prefecture))
p <- p + geom_bar(stat = "identity")
p <- p + theme_dark (base_family = "HiraKakuPro-W3")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + scale_fill_hue( name="都道府県",labels=pref_jp)
plot(p)

# df <- dplyr::group_by(js,date,prefecture) %>% dplyr::summarise(.,sum(count))
# colnames(df)[3] <- 'count'
# wdf$date <- as.Date(wdf$date)
# p <- ggplot(NULL)
# p <- p + geom_bar(data=wdf,aes(x = date, y = count, fill = prefecture),stat = "identity")
# p <- p + theme_dark (base_family = "HiraKakuPro-W3")
# p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# p <- p + scale_fill_hue( name="都道府県",labels=pref_jp)
# 
# plot(p)

