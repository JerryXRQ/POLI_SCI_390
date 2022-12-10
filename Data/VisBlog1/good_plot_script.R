country <- c(rep("China" , 2) , rep("France" , 2) , rep("Japan" , 2) , rep("India" , 2) )
condition <- rep(c("previous" , "current") , 4)
value <- abs(rnorm(8,0))
mock <- data.frame(rate,condition,value)

mock %>% ggplot(aes(fill=condition, y=value, x=country)) + 
  ylim(0,2)+
  labs(x = "Country", y = "Growth of e-commerce as a share of total retail (%)",title="Growth of E-Commerce") +
  scale_fill_discrete(name = "Year", labels = c("2020 average", "2015-2019 average")) +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 8)),
        plot.title = element_text(size = 16), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 8),size = 14))+
  geom_bar(position="dodge", stat="identity")

