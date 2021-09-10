# 東京の感染者数ヒストリカルデータから日次の年齢別ヒストグラムを作る。事前に
# source("../../Dropbox/R-script/covid/tokyo_effective_repro.r") を実行し、
curl <- "https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv"
cdestfile <- "~/R/R2/covid/tokyo.csv"
download.file(curl,cdestfile)

# length_graph <- length(seq(as.Date("2020-03-20"),Sys.Date(),by='days'))
w <- read.csv("~/R/R2/covid/tokyo.csv")
y <- as.xts(as.numeric(substr(w[,9],1,2)),as.Date(w[,5]))
# apply.daily(as.xts(rep(1,length(y[y[,1] == 10])),as.Date(index(y[y[,1] == 10]))),sum)
start_date <- "2020-10-01"
length_graph <- length(seq(as.Date(start_date),xts::last(index(y)),by='days'))
#
tokyo_death <-   as.xts(dmdf[,colnames(dmdf) == "13Tokyo"],dmdf$t)

# 新型コロナウイルス感染症 診療の手引き 2020 19-COVID 第2.2版 @ 2020/7/10
# インデックスはhttps://www.mhlw.go.jp/content/000650160.pdf　のデータを使用
#
# risk_parameter<-c(0,0,0.001,0.005,0.011,0.049,0.146,0.287)
#
# 新型コロナウイルス感染症 診療の手引き 2020 19-COVID 第３版 @ 2020/9/3
#
# https://www.kyoto.med.or.jp/covid19/pdf/08.pdf
#  risk_parameter_v3 <- c(0,0,0,0.001,0.003,0.007,0.035,0.109,0.23)
#
# 新型コロナウイルス感染症 診療の手引き 2020 19-COVID 第4版 @ 2020/12/2
#
#  https://www.mhlw.go.jp/content/000702064.pdf
#  risk_parameter_v4 <- c(0,0,0,0,0.001,0.004,0.017,0.057,0.14)
#
# 新型コロナウイルス感染症 診療の手引き 2020 19-COVID 第4.1版 @ 2020/12/23
#  https://www.mhlw.go.jp/content/000712473.pdf
#
# risk_parameter_v41 <- c(0,0,0,0.001,0.003,0.014,0.048,0.12)
#   　　　
# 新型コロナウイルス感染症 診療の手引き 2020 19-COVID 第4.2版 @ 2021/3/1
# https://www.kyoto.med.or.jp/covid19/pdf/2020ken2_517.pdf
#
# risk_parameter_v42 <- (0, 0, 0, 0, 0.001, 0.003, 0.014, 0.048, 0.125 )
#
# 新型コロナウイルス感染症 診療の手引き 19-COVID 第5版 @ 2021/5/1
# https://www.mhlw.go.jp/content/000785119.pdf
#
# risk_parameter_v5 <- (0, 0, 0, 0, 0.001, 0.003, 0.013, 0.048, 0.132 )
#
# 新型コロナウイルス感染症 診療の手引き 19-COVID 第5.1版 @ 2021/6/29
# https://www.mhlw.go.jp/content/000801626.pdf
#
# risk_parameter_v51 <- (0, 0, 0, 0, 0.001, 0.003, 0.014, 0.05, 0.137 )
# 
# 新型コロナウイルス感染症 診療の手引き 19-COVID 第5.2版 @ 2021/7/29
# https://www.mhlw.go.jp/content/000815065.pdf
#
# risk_parameter_v52 <- (0, 0, 0, 0, 0.001, 0.003, 0.014, 0.051,0.142)
#


v <- c()
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

func <- function(x){(x[index(x)[is.na(x)]] <-0);return(x)}
colfunc <- function(x){colnames(x) <- seq(10,80,10);return(x)}
v <- v %>% as.vector() %>%  func() %>% matrix(.,ncol=8) %>% as.xts(.,index(v)) %>% colfunc()


df <- data.frame(t=xts::last(index(v),length_graph),
                 # o=xts::last(diff(all[,1]-all[,3],t=all[,3]),length_graph),
                 a=xts::last(v[,1],length_graph),
                 b=xts::last(v[,2],length_graph),
                 c=xts::last(v[,3],length_graph),
                 d=xts::last(v[,4],length_graph),
                 e=xts::last(v[,5],length_graph),
                 f=xts::last(v[,6],length_graph),
                 g=xts::last(v[,7],length_graph),
                 h=xts::last(v[,8],length_graph)
                 # or=xts::last(all[,2],length_graph)*multi,
                 # kr=xts::last(all[,2],length_graph)*multi
               )

for(i in seq(1,8,1)){
                 colnames(df)[i+1] <- as.character(i*10)
}

df.melt <- df  %>% tidyr::gather(variable,value,c("10", "20", "30", "40", "50", "60", "70", "80"))

# df.melt <- melt(data=df, id.vars="t", measure.vars=c("X10", "X20", "X30", "X40", "X50", "X60", "X70", "X80"))
# df.melt <- melt(data=df, id.vars="t", measure.vars=c("10", "20", "30", "40", "50", "60", "70", "80"))
# head(df.melt)
df <- df.melt
# in order to overlayer graph use ggplot(NULL) to create base object.
g <- ggplot(NULL)
# g <- ggplot(df, aes(x = t, y = value, fill = variable))
g <- g + scale_fill_brewer(palette="Spectral",na.value = "black",name = "age group", direction=-1,labels = c("=<19",">=20",">=30",">=40",">=50",">=60",">=70",">=80"))
g <- g + geom_bar(data=df,aes(x = t, y = value, fill = variable),stat = "identity")

#func <- function(x1,x2,x3,x4,x5,x6,x7,x8){
#  return(x1*risk_parameter[1]+x2*risk_parameter[2]+x3*risk_parameter[3]+x4*risk_parameter[4]+x5*risk_parameter[5]+x6*risk_parameter[6]+x7*risk_parameter[7]+x8*risk_parameter[8])
#}

func <- function(x1,x2,x3,x4,x5,x6,x7,x8,idx){
  if(idx < as.Date("2020-07-10")){
    risk_parameter <- c(0,0,0.001,0.005,0.011,0.049,0.146,0.287)
  }else{
    if(idx < as.Date("2020-09-02")){
      risk_parameter <- c(0,0,0.001,0.003,0.007,0.035,0.109,0.23)
    }else{
      if(idx < as.Date("2020-12-02")){
         risk_parameter <- c(0,0,0,0.001,0.004,0.017,0.057,0.14)
      }else{
          if(idx < as.Date("2021-03-01")){
            risk_parameter <- c(0,0,0,0.001,0.003,0.014,0.048,0.12)
          }else{
            if(idx < as.Date("2021-05-01")){
              risk_parameter <- c(0,0, 0, 0.001, 0.003, 0.014, 0.048, 0.125 )
            }else{
              if(idx < as.Date("2021-06-29")){
                risk_parameter <- c(0,0, 0, 0.001, 0.003, 0.013, 0.048, 0.132 )
              }else{
                if(idx < as.Date("2021-07-29")){
                risk_parameter <- c(0,0, 0, 0.001, 0.003, 0.014, 0.05, 0.137 )
                }else{
                  risk_parameter <- c(0,0, 0, 0.001, 0.003, 0.014, 0.051, 0.142 )
                }
             }
          }
        }
      }
    }
  }

  return(x1*risk_parameter[1]+x2*risk_parameter[2]+x3*risk_parameter[3]+x4*risk_parameter[4]+x5*risk_parameter[5]+x6*risk_parameter[6]+x7*risk_parameter[7]+x8*risk_parameter[8])
}
# prepare the second layer.
df <- data.frame(t=xts::last(index(v),length_graph),
                value=xts::last(mapply(func,v[,1],v[,2],v[,3],v[,4],v[,5],v[,6],v[,7],v[,8],index(v)),length_graph)
)
# df <- df[-length(df[,1]),]  # cut off the$1xts::last entry.
# g <- ggplot(df, aes(x = t, y = value))
g <- g+geom_line(data=df, aes(x = t, y = value*10))

df <- data.frame(
  t=xts::last(index(tokyo_death),length_graph),
  value=xts::last(tokyo_death[,1],length_graph)
)
g <- g+geom_bar(data=df, aes(x = t, y = value*10,color='black'),stat = "identity",alpha=0.1)
g <- g + scale_color_brewer(name = "death",labels = "# of death")
g <- g + geom_hline(yintercept = seq(100,300,100),size=0.5,linetype=2,colour="black",alpha=1)
g <- g + annotate("text",label=as.character(seq(10,30,10)),x=as.Date(start_date)-10, y= seq(100,300,100)+18,colour='black')
# plot(g)
png("~/Dropbox/R-script/covid/05tokyo_age.png", width = 1200, height = 800)
plot(g)
dev.off()

g <- ggplot(NULL)
# # g <- ggplot(df, aes(x = t, y = value, fill = variable))
# g <- g + scale_fill_brewer(palette="Spectral",na.value = "black",name = "age group", direction=-1,labels = c("=<19",">=20",">=30",">=40",">=50",">=60",">=70",">=80"))
# g <- g + geom_bar(data=df,aes(x = t, y = value, fill = variable),stat = "identity")



df <- data.frame(
  t=xts::last(index(tokyo_death),length_graph),
  value=xts::last(tokyo_death[,1],length_graph)
)
g <- g+geom_bar(data=df, aes(x = t, y = value),stat = "identity",alpha=0.5,colour="red",fill="red")
# prepare the second layer.

df <- data.frame(t=xts::last(index(v),length_graph),
                value=xts::last(mapply(func,v[,1],v[,2],v[,3],v[,4],v[,5],v[,6],v[,7],v[,8],index(v)),length_graph)
)
tokyo_severity <- as.xts(mapply(func,v[,1],v[,2],v[,3],v[,4],v[,5],v[,6],v[,7],v[,8],index(v)),index(v))
# df <- df[-length(df[,1]),]  # cut off the last entry.
# g <- ggplot(df, aes(x = t, y = value))
g <- g+geom_line(data=df, aes(x = t, y = value))
# plot(g)
png("~/Dropbox/R-script/covid/08tokyo_severity_death.png", width = 1200, height = 800)
plot(g)
dev.off()

gap <- 20   # shift between positive and death. as new death only comes out after certain number of days of positive.
start <- 500 # day of start should be less than length(mdf$t) - gap 500 means "2021-05-30"
end <- 0 # day to end. 0 means the end of record.
dmdf$t[start]
if(end == 0){
  end <- length(mdf[,13])
}

d <- dmdf[,13][(gap+start):(end)] %>% cumsum()
p <- mdf[,13][start:(end-gap)] %>% cumsum()
df <- data.frame(death=d,positive=p)
# p <- ggplot(df, aes(x = positive, y = death))
# p <- p + geom_point(stat="identity", position="identity")
df <- cbind(df,twopercent=df$positive * 0.02)
df <- cbind(df,onepointfive=df$positive * 0.015)
df <- cbind(df,onepercent=df$positive * 0.01)
df <- cbind(df,halfpercent=df$positive * 0.005)
df <- cbind(df,qpercent=df$positive * 0.0025)
p <- ggplot(df, aes(x = positive, y = death))
p <- p + geom_point(stat="identity", position="identity")
p <- p + geom_path(aes(y=twopercent))
p <- p + geom_path(aes(y=onepointfive))
p <- p + geom_path(aes(y=onepercent))
p <- p + geom_path(aes(y=halfpercent))
p <- p + geom_path(aes(y=qpercent))


plot(p)
