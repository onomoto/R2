anydayreturn <- function(d=GSPC,k="2021::" ,l=2){
    data=d
    return ((data[,4]/lag(data[,4],2))[k])
}