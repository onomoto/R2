
df <- data.frame(i=as.vector(tmp.predict[,4]),g=as.vector(tmp.predict[,6]),e=as.vector(tmp.predict[,7]),r=as.vector(tmp.predict[,4]-tmp.predict[,7]),t=as.Date(index(tmp.predict)))
colnames(df)[1] <- 'i'
colnames(df)[2] <- 'g'
colnames(df)[3] <- 'e'
colnames(df)[4] <- 'r'

p <- ggplot(df,aes(x=t))
#
# need to investigate mapping= designator. somehow it's necessary to overlay y-axis coordinated graph.
#
p <- p + geom_bar(aes(y=r),stat = "identity",fill='pink',colour="black") # need identity to draw value itself.
p <- p + geom_point(aes(y=i),stat="identity", position="identity",colour="green",size=0.8)
p <- p + geom_path(aes(y=i),stat="identity", position="identity",colour="black")
p <- p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
# same as above about mapping=
p <- p + geom_path(aes(y=g),colour='red')
p <- p + geom_path(aes(y=e),colour='blue')

plot(p)
# remove unnecessary function.
