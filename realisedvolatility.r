func <- function(xts=GSPC[,4],n=21){
    return (((xts/lag(xts)) %>% last(.,n) %>% log())**2 %>% sum() %>% multiply_by(.,252/n) %>% sqrt() %>% multiply_by(.,100))
}

