country <- c(rep("China" , 2) , rep("France" , 2) , rep("Japan" , 2) , rep("India" , 2) )
condition <- rep(c("previous" , "current") , 4)
value <- abs(rnorm(8,0))
mock <- data.frame(rate,condition,value)

mock %>% ggplot(aes(fill=condition, y=value, x=country)) + 
  ylim(0,2)+
  labs(x = "Country", y = "Growth of e-commerce as a share of total retail (%)") +
  scale_fill_discrete(name = "Year", labels = c("2020 average", "2015-2019 average")) +
  geom_bar(position="dodge", stat="identity")

