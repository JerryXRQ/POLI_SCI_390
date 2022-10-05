library(tidyverse)
install.packages("tidyverse", dependencies=T)

data <- read.csv("VisBlog1.csv")

data %>%
  group_by(year)%>%
  summarize(sum_cases=sum(cases),na.rm=T)%>%
  ggplot(aes(x=year,y=sum_cases))+
  scale_y_continuous(limits=c(0,20000000)) +
  geom_point()
