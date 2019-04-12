
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
