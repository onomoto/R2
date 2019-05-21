
# delta is parameter relation against month close vs. open, when delta is 0.9.
# close price is 10% down from open
# when m is "c", its open vs. close. in the case of "h", it is high versus low
#
func <- function(delta=0.9,m="c",l=5){
  lag_month <- l
  ind_bp <- index(na.omit(diff(cli_xts$oecd,lag=lag_month))["1962::"])[na.omit(diff(cli_xts$oecd,lag=lag_month))["1962::"] > 0 & na.omit(diff(cli_xts$oecd,lag=1))["1962::"] > 0]

  ind_bm <- index(na.omit(diff(cli_xts$oecd,lag=lag_month))["1962::"])[na.omit(diff(cli_xts$oecd,lag=lag_month))["1962::"] < 0 & na.omit(diff(cli_xts$oecd,lag=1))["1962::"] < 0]

  ind_mp <- index(na.omit(diff(cli_xts$oecd,lag=lag_month))["1962::"])[na.omit(diff(cli_xts$oecd,lag=lag_month))["1962::"] < 0 & na.omit(diff(cli_xts$oecd,lag=1))["1962::"] > 0]

  ind_pm <- index(na.omit(diff(cli_xts$oecd,lag=lag_month))["1962::"])[na.omit(diff(cli_xts$oecd,lag=lag_month))["1962::"] > 0 & na.omit(diff(cli_xts$oecd,lag=1))["1962::"] < 0]

# sp_correction_ind <- index(SP5["1962::"][SP5["1962::"][,4] / SP5["1962::"][,1] < delta])
# sp_correction_ind <- index(SP5["1962::"][SP5["1962::"][,3] / SP5["1962::"][,2] < delta])

  # if(c == "h"){ print("T")}
  # if(c == "o"){ print("S")}else{print("F")}
  sp_correction_ind <- c()
  a <- m
  # switch(a,               # switch(文字列,
  #   "h" = append(sp_correction_ind,index(SP5["1962::"][SP5["1962::"][,3] / SP5["1962::"][,2] < delta])),
  #   "c" = append(sp_correction_ind,index(SP5["1962::"][SP5["1962::"][,4] / SP5["1962::"][,1] < delta])),
  #   print("?")            #  一致するものが
  # )
  if(a == "h"){
    sp_correction_ind <- as.Date(index(to.monthly(SP5)["1962::"][to.monthly(SP5)["1962::"][,3] / to.monthly(SP5)["1962::"][,2] < delta]))
  }
  else if(a == "c"){
    sp_correction_ind <- as.Date(index(to.monthly(SP5)["1962::"][to.monthly(SP5)["1962::"][,4] / to.monthly(SP5)["1962::"][,1] < delta]))
  }
  else{
    print("?")
  }

# cat("sp_corr ");print(sp_correction_ind)
  cat("bp ");print(length(ind_bp))
  cat("bm ");print(length(ind_bm))
  cat("mp ");print(length(ind_mp))
  cat("pm ");print(length(ind_pm))
  # cat("pm+bm+mp ");print(length(append(append(ind_pm,ind_bm),ind_mp)))
  cat("pm+bm+mp ");print(length(c(ind_pm,ind_bm,ind_mp)))


  cat("correction sp5 vs. bm");print(sp_correction_ind[is.element(sp_correction_ind,ind_bm)])

  cat("correction sp5 vs. bp");print(sp_correction_ind[is.element(sp_correction_ind,ind_bp)])
  cat("correction sp5 vs. pm");print(sp_correction_ind[is.element(sp_correction_ind,ind_pm)])
  cat("correction sp5 vs. mp");print(sp_correction_ind[is.element(sp_correction_ind,ind_mp)])


# t.test(as.vector(VIX[,4][ind_bp]),as.vector(VIX[,4][ind_bm]))
# print("######################################")
# t.test(as.vector(VIX[,4][ind_bp]),as.vector(VIX[,4][append(append(ind_pm,ind_bm),ind_mp)]))
# # t.test(as.vector(VIX[,4][ind_bp]),as.vector(VIX[,4][ind_bm]))

}
func(0.9,"c",5)
