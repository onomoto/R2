#
# original is https://rstudio-pubs-static.s3.amazonaws.com/968660_435758e7a65e4ec497fd027c07bdc49a.html
#
# First, we need the options data, which is available from the CBOE. click on Options
# tab -> set Options Range == All -> set Expiration == All -> click View Chain.
# To download the entire options chain, scroll all the way down, and click on Download CSV.
#
# load detach
#
library(data.table)
library(lubridate)
library(timeDate)
library(stringr)
library(formattable)
library(dplyr)
library(plotly)
library(tidyr)
library(purrr)

#
# load FILE and check spot price
#
# install.packages('reticulate')


FILE <- "~/Downloads/spx_quotedata.csv"

option_chain <- fread (FILE)

# Get SPX Spot
spotLine <- fread(FILE, skip=1, nrows = 1)
spotLineData <- strsplit(as.character(colnames(spotLine[,2])), ":")
spotPrice <- as.numeric(str_trim(spotLineData[[1]][2]))

#specify range of strike prices to view gamma for
fromStrike = 0.8 * spotPrice
toStrike = 1.2 * spotPrice

# Get Today's Date
dateLine <-  fread(FILE, skip=2, nrows = 1)
todayDateData <- strsplit(as.character(colnames(dateLine[,1])), " ")
todayYear <- todayDateData[[1]][4]
todaymonth <- todayDateData[[1]][2]
todayDay <- todayDateData[[1]][3]
todayDate <- as.Date(lubridate::ymd(paste0(todayYear, '-', todaymonth, '-', todayDay)))

# Make col names less ambiguous
colnames(option_chain) <- c('ExpirationDate','Calls','CallLastSale','CallNet','CallBid','CallAsk','CallVol',
                            'CallIV','CallDelta','CallGamma','CallOpenInt','StrikePrice','Puts','PutLastSale',
                            'PutNet','PutBid','PutAsk','PutVol','PutIV','PutDelta','PutGamma','PutOpenInt')

# change date format of expiration date
option_chain$ExpirationDate <- as.Date(lubridate::mdy(substring(option_chain$ExpirationDate,5)))

#determines if the date passed is the 3rd Friday of month
isThirdFriday <- function(expDate){
  #passing date directly to timeNthNdayInMonth in the last step gives unreliable results.
  #day of month appears to need to be the 1st if giving a single date and not a range of dates
  #truncate expiration date day and add 01 in its place

  ym <- str_sub(expDate, end=-4)
  ym01 <- ymd(paste0(ym,'-01'))
  ThirdFriday <- as.Date(timeNthNdayInMonth(ym01, nday = 5, nth = 3))

  ifelse(ThirdFriday == expDate, TRUE, FALSE)
}

# spot gamma exposure calls, puts & total
option_chain$'CallGEX' <- option_chain$'CallGamma' * option_chain$'CallOpenInt'  * 100 * spotPrice^2 * 0.01
option_chain$'PutGEX' <- option_chain$'PutGamma' * option_chain$'PutOpenInt' * 100 * spotPrice^2* 0.01 * -1
option_chain$'TotalGamma' <- (option_chain$CallGEX + option_chain$PutGEX) / 10^9

# create data frame for plotting charts 1 & 2 showing call, put and total gamma exposure
# over a range of strike prices +/- 20% of spot.
dfAgg <- option_chain %>%
  filter(StrikePrice > fromStrike, StrikePrice < toStrike) %>%
  group_by(StrikePrice) %>%
  summarise(CallGEX = sum(CallGEX),
            PutGEX = sum(PutGEX),
            TotalGamma = sum(TotalGamma)
  )

#defining variables for plotting
strikes <- dfAgg$StrikePrice
TotalGamma <- round(sum(dfAgg$TotalGamma),3)

chart1 <- plot_ly(data = dfAgg, x = ~StrikePrice, y = ~TotalGamma, type = 'bar', name = 'Total Gamma')
chart1 <- chart1 %>% add_lines(x=spotPrice,
                               line=list(color= rgb(255, 0, 0, maxColorValue = 255), dash = 'dot',width=0.5),
                               name= paste('Spot Price: ', round(spotPrice,0))
)

# chart1 <- chart1 %>% add_lines(x=5000,
#                                line=list(color= rgb(0, 128, 0, maxColorValue = 255), dash = 'longdash',width=1),
#                                name='', showlegend = FALSE
# )
# chart1 <- chart1 %>% add_lines(x=5100,
#                                line=list(color= rgb(0, 128, 0, maxColorValue = 255), dash = 'longdash',width=1),
#                                name='',showlegend = FALSE
# )
# chart1 <- chart1 %>% add_lines(x=5200,
#                                line=list(color= rgb(0, 128, 0, maxColorValue = 255), dash = 'longdash',width=1),
#                                name='',showlegend = FALSE
# )
# for(i in seq(4200,6200,100)){
for(i in seq(floor((0.008*spotPrice))*100  ,floor((0.012*spotPrice))*100 ,100)){
  chart1 <- chart1 %>% add_lines(x=i,
  line=list(color= rgb(0, 64, 0, maxColorValue = 255), dash = 'dot',width=0.5),
  name='',showlegend = FALSE
  )
}

chart1 <- chart1 %>%layout(
  title = paste("Total Gamma: $", TotalGamma,  " Bn per 1% SPX Move ",  todayDate),
  xaxis=list(title='Strike Price'),
  yaxis=list(title='Spot Gamma Exposure ($ billions/1% move)')
)
# chart1
#
# !!!PLEASE PUT BREAK HERE CAN'T EXECUTE THE SCRIPT IN A SINGLE FILE!!!!
#
readline(prompt="Press [enter] to continue") # wait for input to pause.
#
library(reticulate)
#
# !!!!! execute following reticulate at least once on the execution environment
# !!!!! they are do to install required python packages
# !!!!! w/o instlation execution will be blocked
# 
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys")

#the name and type of file (can be .png, .jpeg, etc.)
save_image(chart1,"/Users/honomoto/Dropbox/R-script/covid/w.png",width=1200,height=800)
quit("no")

# png("~/Dropbox/R-script/covid/w.png", width = 1200, height = 1000)
# chart1
# dev.off()


# detach("package:lubridate", unload=TRUE)
# detach("package:timeDate", unload=TRUE)
# detach("package:stringr", unload=TRUE)
# detach("package:formattable", unload=TRUE)
#
# detach("package:plotly", unload=TRUE)
# detach("package:data.table", unload=TRUE)
# detach("package:tidyr", unload=TRUE)
#
# detach("package:dplyr", unload=TRUE)
# detach("package:purrr", unload=TRUE)
