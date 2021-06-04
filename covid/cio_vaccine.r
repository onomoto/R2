curl <- "https://vrs-data.cio.go.jp/vaccination/opendata/latest/summary_by_date.csv"
cdestfile <- "~/R/R2/covid/summary_by_date.csv"
download.file(curl,cdestfile)
w <- read.csv(cdestfile)

colnames(w)[2] <- 'start_per_capita'
colnames(w)[3] <- 'last_per_capita'
last(w[,1])
apply(w[,c(2,3)],2,sum)
apply(w[,c(2,3)],2,sum) %>% sum()
apply(w[,c(2,3)],1,sum) %>% last(.,7)

df <- w  %>% tidyr::gather(variable,value,2:3)
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
colnames(df)[6] <- 'start_per_capita'
colnames(df)[7] <- 'last_per_capita'
wdf <- df %>% tidyr::gather(variable,value,c(6,7))
# wdf[,6] <- wdf[,2]
wdf[,2] <- rep(pref_en,2)
df <- wdf 
df[,7] <- df[,7]/10 # convert to percentage.


# p <- p + theme_gray (base_family = "HiraKakuPro-W3")
p <- ggplot(df, aes(y = value, x = prefecture_name, fill = variable))
p <- p + theme_gray (base_family = "HiraKakuPro-W3")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + geom_bar(stat = "identity")
# p <- p + scale_fill_brewer( name="回数",labels=c("2","1") ) #,palette="Spectral")
p <- p+ scale_fill_manual(name="回数",labels=c("2回目","1回目"),values = c("darkblue", "cyan4")) 
p <- p + scale_x_discrete(limits=unique(df$prefecture_name),label=substr(unique(df$prefecture_name),1,3))
plot(p)


curl <- "https://vrs-data.cio.go.jp/vaccination/opendata/latest/prefecture.ndjson"
cdestfile <- "~/R/R2/covid/prefecture.ndjson"
# delimiter <- ">"
# targetfile <- "~/R/R2/covid/pr.ndjson"
# download.file(curl,cdestfile)
# system(paste("gzcat",cdestfile,delimiter,targetfile))
# js <- ndjson::stream_in(targetfile)
js <- jsonlite::stream_in(gzfile(cdestfile) )
last(js)
