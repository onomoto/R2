# 0. set locale to "en_US"
# 1. check the given date by parameter "date"
# 2. check the last updated date of symbole which is also given as paramter
# 3. from tuesday to Saturday. today should be the last update + 1,otherwise dowload data
# 4. Sunday, last update +2, Monday, +3
# 5. put back to locale to "JP"
#
my_update_check <- function(symbol,date){
    locale <- substr(Sys.getlocale(),1,5)
    Sys.setlocale("LC_ALL",'en_US')
    DOWNLOAD <- "D"
    SKIP <- "S"
    today <- date
    sym <- symbol
    # today <- as.Date("2019-05-10")
    wd <- weekdays(today)
    # wd <- weekdays(as.Date("2019-05-10"))
    last_update <- last(index(sym))
    if(wd == "Monday"){
        if(as.Date(last_update)+3 != today){
  #         getSymbols(as.character(sym),src="yahooj")
            print("monday  lazy friday")
            cat("the last update was")
            print(as.Date(last_update))
            CODE <- DOWNLOAD
        }else{
            print("monday and working friday!")
            print(as.Date(last_update))
            CODE <- SKIP
        }
    }else if(wd == "Sunday"){
        if(as.Date(last_update)+2 != today){
   #        getSymbols(as.character(sym),src="yahooj")
            print("sunday  lazy friday")
            cat("the last update was")
            print(as.Date(last_update))
            CODE <- DOWNLOAD
        }else{
            print("sunday and working friday!")
            print(as.Date(last_update))
            CODE <- SKIP
        }
    }else{
        if(as.Date(last_update)+1 != today){
    #       getSymbols(as.character(sym),src="yahooj")
            print("not monday, lazy yesterday")
            cat("the last update was")
            print(as.Date(last_update))
            CODE <- DOWNLOAD
        }else{
            print("not monday, worked yesterday")
            print(as.Date(last_update))
            CODE <- SKIP
        }
    }
    Sys.setlocale("LC_ALL",locale)
    return(CODE)
}
