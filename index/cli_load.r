# read data from csv.
#
cli_xts <- merge(as.xts(read.zoo(read.csv("~/Downloads/oecd.csv"))),as.xts(read.zoo(read.csv("~/Downloads/usa.c
sv"))),suffixes = c("oecd","usa"))
