# download csv from oecd(https://data.oecd.org/leadind/composite-leading-indicator-cli.htm) to "~/Downloads"
# use file name "CLI3.csv".
# this file contains multiple regions data. you have to specify its name.
# extract USA only entries
# execute commands below at "~/Download".
#
sed -n '/USA/p' CLI3.csv |awk -F, '{print $6"-01,"$7}'  |sed 's/\"//g' |awk 'BEGIN{print "DATE,DATA"}{print $0}' > usa.csv
# extract OECD entries and exclude OECDE
# sed -n '/OECD[^E]/p' CLI3.csv |awk -F, '{print $6"-01,"$7}'  |sed 's/\"//g' |awk 'BEGIN{print "DATE,DATA"}{print $0}' > oecd.csv
# 
sed -n '/CHN/p' CLI3.csv |awk -F, '{print $6"-01,"$7}'  |sed 's/\"//g' |awk 'BEGIN{print "DATE,DATA"}{print $0}' > chn.csv
# 
sed -n '/G-20/p' CLI3.csv |awk -F, '{print $6"-01,"$7}'  |sed 's/\"//g' |awk 'BEGIN{print "DATE,DATA"}{print $0}' > g20.csv
sed -n '/G-7/p' CLI3.csv |awk -F, '{print $6"-01,"$7}'  |sed 's/\"//g' |awk 'BEGIN{print "DATE,DATA"}{print $0}' > g7.csv
#
sed -n '/JPN/p' CLI3.csv |awk -F, '{print $6"-01,"$7}'  |sed 's/\"//g' |awk 'BEGIN{print "DATE,DATA"}{print $0}' > jpn.csv
