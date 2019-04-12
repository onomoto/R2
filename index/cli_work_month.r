
# select month from CY2000 where cli 6 mon. delta and 1 mon. delta are less than 0.
index(cli_xts$oecd["2000-01-01::"][na.omit(diff(cli_xts$oecd["1999-07-01::"],lag=6)) < 0 & na.omit(diff(cli_xts$oecd["1999-12-01::"])) < 0])
#
# select month from CY2000 where cli 6 mon. delta is less than 0.
#
index(cli_xts$oecd["2000-01-01::"][na.omit(diff(cli_xts$oecd["1999-07-01::"],lag=6)) < 0])
#
#  the list of months from 2000 till the updated cli.
#
seq(as.Date("2000-01-01"),as.Date(index(last(cli_xts))),by='months')
#
# selct month from CY2000 where cli 6 mon. delta AND/OR 1 mon. delta is more than ZERO.
#
as.Date(setdiff(seq(as.Date("2000-01-01"),as.Date(index(last(cli_xts))),by='months'),
index(cli_xts$oecd["2000-01-01::"][na.omit(diff(cli_xts$oecd["1999-07-01::"],lag=6)) < 0 & na.omit(diff(cli_xts$oecd["1999-12-01::"])) < 0])))
#
# select month from CY2000 where cli 6 mon. delta is equal to or more than 0.
#
index(cli_xts$oecd["2000-01-01::"][na.omit(diff(cli_xts$oecd["1999-07-01::"],lag=6)) >= 0])
#
#   OR
#
as.Date(setdiff(seq(as.Date("2000-01-01"),as.Date(index(last(cli_xts))),by='months'),index(cli_xts$oecd["2000-01-01::"][na.omit(diff(cli_xts$oecd["1999-07-01::"],lag=6)) < 0])))
#
#  check any month whose low is less than 90% of high belongs to those which cli 5 mon. delta and 1 mon. delta are less than ZERO. from 2007-01-01 to 2019-02-01
#
is.element(as.Date(index(to.monthly(GSPC)[,4][to.monthly(GSPC)[,3]/to.monthly(GSPC)[,2] < 0.9]))[12:18],index(cli_xts$oecd["2000-01-01::"][na.omit(diff(cli_xts$oecd["1999-07-01::"],lag=5)) < 0 & na.omit(diff(cli_xts$oecd["1999-12-01::"])) < 0]))
#
#  check any month whose low is less than 90% of high belongs to those which cli 5 mon. delta and 1 mon. delta are less than ZERO. from 2000-01-01 to 2019-02-01
#
index(SP5["2000-01-01::2019-02-01"][SP5[,3]["2000-01-01::2019-02-01"]/SP5[,2]["2000-01-01::2019-02-01"] < 0.9])
#  [1] "2000-03-01" "2000-04-01" "2000-10-01" "2001-02-01" "2001-03-01" "2001-04-01" "2001-09-01"
#  [8] "2002-06-01" "2002-07-01" "2002-08-01" "2002-09-01" "2002-10-01" "2003-01-01" "2003-03-01"
# [15] "2008-01-01" "2008-09-01" "2008-10-01" "2008-11-01" "2008-12-01" "2009-01-01" "2009-02-01"
# [22] "2009-03-01" "2009-04-01" "2009-07-01" "2010-05-01" "2011-08-01" "2011-10-01" "2015-08-01"
# [29] "2016-01-01" "2018-02-01" "2018-10-01" "2018-12-01"
is.element(index(SP5["2000-01-01::2019-02-01"][SP5[,3]["2000-01-01::2019-02-01"]/SP5[,2]["2000-01-01::2019-02-01"] < 0.9]),index(cli_xts$oecd["2000-01-01::"][na.omit(diff(cli_xts$oecd["1999-07-01::"],lag=5)) < 0 & na.omit(diff(cli_xts$oecd["1999-12-01::"])) < 0]))
#  [1] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [17]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
length( index(SP5["2000-01-01::2019-02-01"][SP5[,3]["2000-01-01::2019-02-01"]/SP5[,2]["2000-01-01::2019-02-01"] < 0.9]))
# [1] 32
#
# month of correction taking place but both or either one of 5 mon. and 1 mon. delta is more than ZERO.
#
index(SP5["1990-01-01::2019-02-01"][SP5[,3]["1990-01-01::2019-02-01"]/SP5[,2]["1990-01-01::2019-02-01"] < 0.9])[!is.element(index(SP5["1990-01-01::2019-02-01"][SP5[,3]["1990-01-01::2019-02-01"]/SP5[,2]["1990-01-01::2019-02-01"] < 0.9]),index(cli_xts$oecd["1990-01-01::"][na.omit(diff(cli_xts$oecd["1989-07-01::"],lag=5)) < 0 & na.omit(diff(cli_xts$oecd["1989-12-01::"])) < 0]))]
# [1] "1990-01-01" "1996-07-01" "1997-10-01" "1999-10-01" "2000-03-01" "2000-04-01" "2002-06-01"
# [8] "2009-03-01" "2009-04-01" "2009-07-01" "2010-05-01"
#
# month of correction both 5 mon. and 1 mon. delta is less than ZERO.
#
index(SP5["1990-01-01::2019-02-01"][SP5[,3]["1990-01-01::2019-02-01"]/SP5[,2]["1990-01-01::2019-02-01"] < 0.9])[is.element(index(SP5["1990-01-01::2019-02-01"][SP5[,3]["1990-01-01::2019-02-01"]/SP5[,2]["1990-01-01::2019-02-01"] < 0.9]),index(cli_xts$oecd["1990-01-01::"][na.omit(diff(cli_xts$oecd["1989-07-01::"],lag=5)) < 0 & na.omit(diff(cli_xts$oecd["1989-12-01::"])) < 0]))]
#  [1] "1990-08-01" "1991-01-01" "1991-12-01" "1998-08-01" "1998-09-01" "1998-10-01" "2000-10-01"
#  [8] "2001-02-01" "2001-03-01" "2001-04-01" "2001-09-01" "2002-07-01" "2002-08-01" "2002-09-01"
# [15] "2002-10-01" "2003-01-01" "2003-03-01" "2008-01-01" "2008-09-01" "2008-10-01" "2008-11-01"
# [22] "2008-12-01" "2009-01-01" "2009-02-01" "2011-08-01" "2011-10-01" "2015-08-01" "2016-01-01"
# [29] "2018-02-01" "2018-10-01" "2018-12-01"
