#
# experimental code to compare CLI lagged period, CLI delta and their combination.
#
k2k <- "2000-04-01::2019-06-30"
k2k_cli <- "2000-01-01::2019-03-31"
k2k_eps <- "2000-07-01::2019-09-30"
# [1] "2000-01-01::2018-12-31"
# calculate cli 6 month delta
# change to 5 month as it fits better
lag_month <- 1
#
# 1)CLI lagged period instead of CLI delta
#
print(summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(cli_xts$oecd[k2k_cli],mean))))
#
# 1)CLI laggned period and 2)CLI delta
#
print(summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=lag_month)[k2k],mean)+apply.quarterly(cli_xts$oecd[k2k_cli],mean))))
#
# the standard strategy 1)CLI cli_delta
#
print(summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=lag_month)[k2k],mean))))
#
# 1)current EPS 2)next EPS and 3)CLI delta.
#
print(summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+eps_year_xts[k2k_eps]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=lag_month)[k2k],mean))))
#
# 1)current EPS 2)next EPS 3)CLI delta and 4) lannged CLI
#
print(summary(lm(apply.quarterly(SP5[,4][k2k],mean) ~ eps_year_xts[k2k]+eps_year_xts[k2k_eps]+apply.quarterly(PA[k2k],mean)+apply.quarterly(CS[k2k],mean)+apply.quarterly(UC[k2k],mean)+apply.quarterly(diff(cli_xts$oecd,lag=lag_month)[k2k],mean)+apply.quarterly(cli_xts$oecd[k2k_cli],mean))))