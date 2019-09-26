
-----
# dataframe を準備
* 異なる種類のデータを統合するときはこのdata frame作成時にやる。
* カラム名がggplot()内部での変数名となるので、注意！
```r
df <- data.frame(
  i=as.vector(cov),
  c=as.vector(mapply(func,delta,watermark[1],watermark[2],watermark[3],watermark[4],watermark[5],watermark[6],watermark[7])),
  t=as.Date(index(cov)))
```

# ggplot作成の第一歩
## データフレームおよび基礎データの指定
* データとして使用するデータフレームを指定する。
* aes()でいろいろな見た目パラメータを指定する。ここではfillで塗りつぶし色を決定する基準を指定している。
* 離散量で指定したいときはfactor()を使用する。
* 連続量を使うときはnumericなどの型のデータをそのまま指定すれば良い。
* y軸と違いx軸は大概の場合、常に共通なので、aes(x=<column name of data frame>) を使ってここで指定すると良い。
* 以降はオブジェクト p に対して更新を行う。
~~~r
p <- ggplot(df, aes(x=i,fill=factor(c)))
~~~

## グラフの種類およびyデータの指定
* ぞれぞれ棒グラフの作成、分散図の作成、線グラフの作成する。
* aes(y=<column name of data fram>,,,)の形でデータフレームのカラム名のうち適当なものをy軸のデータとして指定する。
* 色を付けたい場合はaes(fill=<column name of data frame>)などとする。
* geom_segment()は任意の2つの座標の間に線分を引く
~~~r
p <- p + geom_bar(aes(y=r),stat = "identity",fill='pink',colour="black") # need identity to draw value itself.
p <- p + geom_point(aes(y=i),stat="identity", position="identity",colour="green",size=0.8)
p <- p + geom_path(aes(y=i),stat="identity", position="identity",colour="black",linetype="dotted")
p <- p + geom_segment(x=as.Date("1985-01-01"),y=log(168),xend=as.Date("2019-09-01"),yend=log(3000),color='white',size=0.02,linetype=2)
~~~

### 棒グラフ&ヒストグラム
#### 棒グラフに色をつけたい場合。

この例ではfillとcolor(colour)双方をつかっている。詳細は`scale_color_brewer()`と`scale_fill_brewer()`で追加の設定を行う。
~~~r
p <- p + geom_bar(aes(y=r,fill=clidelta),stat = "identity",colour="black") # need identity to draw value itself.
p <- p + geom_point(mapping=aes(y=i,colour=clidelta),stat="identity", position="identity",size=0.8)
~~~
色指定したいデータについて使用するパレットとそれぞれの色値に対する説明を入力する。fillの場合はこの`scale_fill_brewer()`を使う。
`scale_fill_discrete()`とか`scale_colour_discrete()`あるがよくわからないことがまだ多いので、注意して使うこと。
~~~r
legendlable <- c(paste("more than ",as.character(round(watermark,digits=2)),sep=""),"Less than above","NA")
p <- p +scale_fill_brewer(palette="Spectral",na.value = "grey50",name = "CLI Delta", labels = legendlable)
~~~

#### ヒストグラムの色々

* 目的に応じて、"identity","stack","fill" それぞれのパラメータを指定する。
* ヒストグラムのx軸に連続量を指定する場合、離散量を指定する場合で結果が全く異なるので注意すること。

~~~r
p <- p + geom_histogram(aes(fill=sign),position = "identity", alpha = 0.3,bins=120)
p <- p + geom_histogram(bins=50,position = "fill", alpha = 0.9)
p <- p + geom_histogram(bins=80,position = "stack", alpha = 0.9)
~~~

### 点グラフ

点グラフの色指定はcolorのみ。fillは指定できないので注意すること。

### X軸の間隔指定

Date型のときに使用する。
~~~r
p <- p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
~~~
時間量に応じてグラデーションで色を変えたいときに使用する。
~~~r
p <- p + scale_fill_date(low = "green3" , high = "darkgreen")
~~~

## タイトルおよび凡例

全体タイトルとレジェンドのタイトルを指定する。共通なタイトルを指定するとレジェンドを一つに統合できる。
~~~r
p <- p + labs(title = "SPX + Theory + Residual + CLI Delta",fill="CLI Delta",colour = "CLI Delta")
p <- p + scale_color_brewer(palette="Spectral",na.value = "black",name = "CLI Delta", labels = c("High","mid High","mid Low","Low","NA"))
p <- p + scale_fill_brewer(palette="Spectral",na.value = "black",name = "CLI Delta", labels = c("High","mid High","mid Low","Low","NA"))
~~~

## 任意の線を引く
水平線を引く。
~~~r
p <- p + geom_hline(yintercept = 250,size=0.5,linetype=1,colour="white",alpha=1)
~~~
垂直線を引く。
~~~r
p <- p + geom_vline(xintercept=seq(as.Date(paste(substr(index(head(spx_mean,1)),1,7),"-01",sep="")),as.Date("2019-01-01"),by='years'), colour="white",size=0.4,alpha=0.5)
~~~
任意の直線を引く
~~~r
p <- p + geom_segment(x=as.Date("1985-01-01"),y=log(168),xend=as.Date("2019-09-01"),yend=log(3000),color='white',size=0.02,linetype=2)
~~~
回帰線
~~~r
p <- p + stat_smooth(aes(x=t,y=i),method="loess",color='white',size=0.3)
~~~

## 任意の色を使う

* データフレーム`df`を作成する際にxの値に応じて"g"または"r"を割り振って、カラム`sign`に入れる
* `color=`にカラム`sign`を指定する。
* `scale_colour_manual(values = c(g  = "green",r  = "red"))`で"g"に緑を"r"に赤を割り振る。

~~~r
func <- function(x){if(x > 0){return("g")}else{return("r")}}
df <- data.frame(week=as.vector(w),t=index(w),sign=as.vector(apply(w,1,func)))
p <- ggplot(df)
p <- p + geom_bar(aes(y=week,x=t,color=sign),stat="identity")
p <- p + scale_colour_manual(values = c(g  = "green",r  = "red"))
~~~

# テーマ
x-y軸のタイトル消去
~~~r
p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
~~~
台紙の色指定
~~~r
p <- p + theme(rect = element_rect(fill = "grey88",
                                  colour = "black",
                                  size = 0,
                                  linetype = 1))
~~~
パネルの色指定およびグリッドの消去
~~~r
p <- p + theme(panel.background = element_rect(fill = "grey88",
                                              colour = "lightblue"),
             panel.grid = element_blank())
~~~
凡例の抑止
~~~r
p <- p + theme(legend.position = 'none')  # erase legend
~~~


# その他
コメント入力
~~~r
p <- p + annotate("text",label=as.character(s),x=as.Date("2000-01-01"), y=log(s*1.03),colour='white')
~~~

未検証
~~~r
p <- p + theme(axis.text = element_text(colour = "red", size = rel(1.5)))
~~~

# 過去に使用した関数

* p <- p + geom_bar
* p <- p + geom_path
* p <- p + labs
* p <- p + scale_fill_date
* p <- p + scale_x_date
* p <- p + geom_bar
* p <- p + geom_histogram
* p <- p + geom_hline
* p <- p + geom_path
* p <- p + geom_point
* p <- p + geom_vline
* p <- p + labs
* p <- p + scale_color_brewer
* p <- p + scale_fill_brewer
* p <- p + scale_x_date
* p <- p + theme
* p <- p + scale_colour_manual
