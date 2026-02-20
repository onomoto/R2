# plese refer the latter half of
# lag is set to  5 months.
#
#   s = start date of the spiral like "1992-01-01::" DON'T FORGET DOUBLE COLON!!
#       Don't set before "1964-01-01"
#   m = length of delta, by month. like 1 or 2 or any other integer.
#   xts = xts object like cli_xts$oecd,
#   use like  > func("2001-01-01::",5,cli_xts_$oecd)
#
func <- function(s="2011-01-01::",m=5,xts=cli_g20,region="G20"){

  head_of_record <- s
  print(head_of_record)
  lag_month <- m

# construct data fram for graph
  reading <- as.numeric(xts[head_of_record])
  delta <- as.numeric(na.omit(diff(xts,lag_month)[head_of_record]))
  year <- as.character(year(index(na.omit(diff(xts,lag_month)[head_of_record]))))
  df <- data.frame(y=reading,x=delta,c=year)
# draw and input graph info
  p <- ggplot(df, aes(x=x,y=y))
  p <- p + geom_path(alpha=0.9,linetype = 'dotted')
  p <- p + geom_path(alpha=0.5,aes(color=year))
  p <- p + geom_point(alpha=0.9,aes(color=year),size=2)
# construct and input title info.
  # title <- paste("Composite Leading Indicator ",paste(lag_month," month delta vs. reading",sep=""),sep="")
  title <- paste0(region," Composite Leading Indicator ",lag_month," month delta vs. reading")
  p <- p + ggtitle(title)
  p <- p + theme(plot.title = element_text(hjust = 0.5))  # position main title at the center
  p <- p + xlab("Delta") + ylab("Composite Leading Indicator")

  plot(p)

}
func(s="2021-01-01::",5,xts=cli_g20,region="G20")
