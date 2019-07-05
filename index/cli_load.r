# read data from csv.
#
cli_xts <- merge(as.xts(read.zoo(read.csv("~/Downloads/oecd.csv"))),
as.xts(read.zoo(read.csv("~/Downloads/usa.csv"))),
as.xts(read.zoo(read.csv("~/Downloads/chn.csv"))),
as.xts(read.zoo(read.csv("~/Downloads/ea19.csv"))),
suffixes = c("oecd","usa","china","ea19"))
