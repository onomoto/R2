# > palette.pals()
# [1] "R3"              "R4"              "ggplot2"         "Okabe-Ito"       "Accent"          "Dark 2"         
# [7] "Paired"          "Pastel 1"        "Pastel 2"        "Set 1"           "Set 2"           "Set 3"          
# [13] "Tableau 10"      "Classic Tableau" "Polychrome 36"   "Alphabet" 
palette("Alpabet")
df <- data.frame(case_per_capita=as.vector(apply(mdf[,-48],2,sum) / pref_db$x2017),pop_density=pref_db$x2017/pref_db$size,sign=pref_db$x2017,r=pref_db[,2])
# p <- ggplot(df, aes(x=pop_density,y=case_per_capita,size=sign,color=r))
p <- ggplot(df, aes(x=pop_density,y=case_per_capita,size=sign,color=r))
p <- p + xlab("人口密度") + ylab("人口あたり件数")
p <- p + geom_point(alpha=1)
p <- p+annotate("text",label=pref_db[attributes(df[df$case_per_capita > 3 ,])$row.names,3],x=df[attributes(df[df$case_per_capita > 3 ,])$row.names,2], y=df[attributes(df[df$case_per_capita > 3 ,])$row.names,1]+0.1,colour='red',family = "HiraKakuProN-W3")
# p <- p +scale_color_brewer(palette="Spectral")
# p <- p +scale_color_brewer(palette=rainbow(47))
# p <- p +scale_color_brewer()
p <- p + theme_gray (base_family = "HiraKakuPro-W3")
p <- p + scale_color_hue(name="都道府県",labels=pref_db[,3])
# p <- p + guides(fill = guide_legend(reverse = F,order = 2),label = TRUE)
p <- p + guides(size = guide_legend(title="人口"))
# don't forget to set "color=". otherwise fails to show up.
p <- p + geom_smooth(method = "lm",se=F,color="red",size=1)  
plot(p)

df <- data.frame(death_per_capita=as.vector(apply(dmdf[,-48],2,sum) / pref_db$x2017),pop_density=pref_db$x2017/pref_db$size,sign=pref_db$x2017,r=pref_db[,2])
# p <- ggplot(df, aes(x=pop_density,y=case_per_capita,size=sign,color=r))
p <- ggplot(df, aes(x=pop_density,y=death_per_capita,size=sign,color=r))
p <- p + xlab("人口密度") + ylab("人口あたり死者数")
p <- p + geom_point(alpha=1)
p <- p+annotate("text",label=pref_db[attributes(df[df$death_per_capita > 0.07 ,])$row.names,3],x=df[attributes(df[df$death_per_capita > 0.07 ,])$row.names,2], y=df[attributes(df[df$death_per_capita > 0.07 ,])$row.names,1]+0.002,colour='red',family = "YuGo-Medium")

# p <- p +scale_color_brewer(palette="Spectral")
# p <- p +scale_color_brewer(palette=rainbow(47))
# p <- p +scale_color_brewer()
p <- p + theme_gray (base_family = "YuGo-Medium")
p <- p + scale_color_hue(name="都道府県",labels=pref_db[,3])
# p <- p + guides(fill = guide_legend(reverse = F,order = 2),label = TRUE)
p <- p + guides(size = guide_legend(title="人口"))
# don't forget to set "color=". otherwise fails to show up.
p <- p + geom_smooth(method = "lm",se=F,color="red",size=1,formula = "y ~ log(x)")  
plot(p)

