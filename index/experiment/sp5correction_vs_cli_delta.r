func <- function(){

ind_bp <- index(na.omit(diff(cli_xts$oecd,lag=5))["1962::"])[na.omit(diff(cli_xts$oecd,lag=5))["1962::"] > 0 & na.omit(diff(cli_xts$oecd,lag=1))["1962::"] > 0]

ind_bm <- index(na.omit(diff(cli_xts$oecd,lag=5))["1962::"])[na.omit(diff(cli_xts$oecd,lag=5))["1962::"] < 0 & na.omit(diff(cli_xts$oecd,lag=1))["1962::"] < 0]

ind_mp <- index(na.omit(diff(cli_xts$oecd,lag=5))["1962::"])[na.omit(diff(cli_xts$oecd,lag=5))["1962::"] < 0 & na.omit(diff(cli_xts$oecd,lag=1))["1962::"] > 0]

ind_pm <- index(na.omit(diff(cli_xts$oecd,lag=5))["1962::"])[na.omit(diff(cli_xts$oecd,lag=5))["1962::"] > 0 & na.omit(diff(cli_xts$oecd,lag=1))["1962::"] < 0]

sp_correction_ind <- index(SP5["1962::"][SP5["1962::"][,3] / SP5["1962::"][,2] < 0.9])

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

# correction sp5 vs. bm
# [1] "1962-05-01" "1962-06-01" "1966-08-01" "1966-10-01" "1969-07-01" "1970-01-01" "1970-04-01" "1970-05-01" "1970-07-01" "1970-08-01"
# [11] "1973-11-01" "1974-08-01" "1974-09-01" "1974-10-01" "1974-11-01" "1975-01-01" "1979-10-01" "1980-01-01" "1980-03-01" "1981-09-01"
# [21] "1982-08-01" "1984-08-01" "1987-11-01" "1987-12-01" "1990-08-01" "1991-01-01" "1991-12-01" "1998-08-01" "1998-09-01" "1998-10-01"
# [31] "2000-10-01" "2001-02-01" "2001-03-01" "2001-04-01" "2001-09-01" "2002-07-01" "2002-08-01" "2002-09-01" "2002-10-01" "2003-01-01"
# [41] "2003-03-01" "2008-01-01" "2008-09-01" "2008-10-01" "2008-11-01" "2008-12-01" "2009-01-01" "2009-02-01" "2011-08-01" "2011-10-01"
# [51] "2015-08-01" "2016-01-01" "2018-02-01" "2018-10-01" "2018-12-01"
# correction sp5 vs. bp
# [1] "1975-04-01" "1975-10-01" "1976-01-01" "1978-04-01" "1978-10-01" "1980-11-01" "1980-12-01" "1982-10-01" "1987-01-01" "1990-01-01"
# [11] "1996-07-01" "1999-10-01" "2000-03-01" "2009-07-01" "2010-05-01"
# correction sp5 vs. pm
# [1] "1987-10-01" "1997-10-01" "2000-04-01" "2002-06-01"
# correction sp5 vs. mp
# [1] "1962-11-01" "1986-09-01" "2009-03-01" "2009-04-01"

# print(mean(VIX[,4][ind_bp]))
# print(mean(VIX[,4][ind_bm]))
# print(mean(VIX[,4][ind_mp]))
# print(mean(VIX[,4][ind_pm]))
# print(mean(VIX[,4][append(append(ind_pm,ind_bm),ind_mp)]))
#
#
# t.test(as.vector(VIX[,4][ind_bp]),as.vector(VIX[,4][ind_bm]))
# print("######################################")
# t.test(as.vector(VIX[,4][ind_bp]),as.vector(VIX[,4][append(append(ind_pm,ind_bm),ind_mp)]))
# # t.test(as.vector(VIX[,4][ind_bp]),as.vector(VIX[,4][ind_bm]))

}
func()
