mnt <- index(cli_xts$oecd["2000::2018"][cli_xts$oecd["2000::2018"] < 100 & cli_xts$oecd["2000::2018"]/as.vector(cli_xts$oecd["1999-07-01::2018-06-01"]) < 1])
plot.zoo(merge(SP5["2000::"][,4]/SP5["2000::"][,1]-1,SP5[mnt][,4]/SP5[mnt][,1]-1),type='h',col = c("red", "blue"), plot.type = "single")

