
*how to write*

* use write zoo, don't forget sep=',', otherwise outputs doesn't include 'comma'.

`write.zoo(eps_year_xts,file="~/e.csv",sep=',')`

*how to read*

`eps_year_xts <- as.xts(read.zoo(read.csv("~/e.csv")))`
