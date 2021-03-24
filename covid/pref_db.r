# go to https://www.e-stat.go.jp/dbview?sid=0003312316
# choose csv with cross summary w/ header

# w <- read.csv("~/Downloads/c01.csv",fileEncoding = "shift_jis")
pref_db <- cbind(as.data.frame(pref_lab),pref_en,read.csv("~/Downloads/c01.csv",fileEncoding = "shift_jis")[3:49,c(8,10,11,12,13)])
# pref_db <- pref_db[,-4]
colnames(pref_db)[c(3,4,5,6,7)] <- c("pref_jp","x2010","x2015","x2016","x2017")
# otherwise use column name
attributes(pref_db)$row.names  <- as.integer(seq(1,47,1))
# update $row.names as [[3]] this works as equal to use $row.names.
# attributes(pref_db)[[3]] <- as.integer(seq(1,47,1))
