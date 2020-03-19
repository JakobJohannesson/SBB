library(borsdata)
library(stringr)
library(dplyr)

# Api key

aktier<-fetch_instruments(key=key)
str(aktier)
aktier %>% filter(ticker=="SBB B")

year<-fetch_year(438,key = key)
r12<-fetch_r12(438,key = key)
kvartal<-fetch_quarter(438,key = key)

for(i in 1:33){
  year[,i]<-rev(year[,i])
  r12[,i]<-rev(r12[,i])
  kvartal[,i]<-rev(kvartal[,i])
}

cagr<-(kvartal$revenues[10]/kvartal$revenues[1])^1/length(kvartal$year)


# Visualiserar
library(ggplot2)

skr<-data.frame(year=kvartal$year,revenue=kvartal$revenues,
                ebit=kvartal$profit_Before_Tax,tid=seq(2017.5,2019.75,0.25))



plot_labels <- data.frame(label = round(cagr,digits = 3)*100,
                          x = seq(2017.5,2019.75,0.25),
                          y = kvartal$revenues[1:10]*1.3)


ggplot(data =  skr, aes(x=tid, y=revenue))+ 
  geom_bar(stat = "identity", 
           fill = "dark green",   
           colour = "black")+ theme_bw()+ labs(
             x = "År",
             y = "Omsättning\n MKR",
             title = "Novotek omsättning och ebitmarginal mellan 2009 och 2019",
             caption = "Källa: Börsdata"
           )+ 
  theme(
    axis.title.y =
      element_text(
        angle = 0,
        hjust = 1,
        vjust = 0.5
      ),
    plot.title = element_text(hjust = 0.5)
  )+ theme(
    panel.grid.major.x =
      element_blank(),
    panel.grid.minor.x =
      element_blank(),
    panel.grid.major.y =
      element_line(color = "grey")
  )+
  scale_x_continuous(breaks=seq(2009,2019,1))+
  scale_y_continuous(breaks=seq(0,350,50), limits = c(0,350))+ 
  geom_text(data = plot_labels,
            aes(x = x[6], y = 300, label = label), size=5, parse = TRUE)+
  annotate("segment", x = plot_labels$x[2], xend = plot_labels$x[10],
           y = year$revenues[2]*1.1,
           yend = year$revenues[10]*1.25,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50")+
  geom_text(aes(label=round(skr$ebit/skr$revenue, digits=3)*100), vjust=1.5, color="white", size=3.5)

