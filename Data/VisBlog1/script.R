library(tidyverse)
install.packages("tidyverse", dependencies=T)

data <- na.omit(read.csv("VisBlog1.csv"))


data %>%
  group_by(month,year)%>%
  summarize(sum_cases=sum(deaths),na.rm=T)%>%
  ggplot(aes(y=month,x=sum_cases, color=as_factor(year)))+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  labs(x="Number of Confirmed Cases Recorded", y = "Month", 
       color="Year",
       title="Europe COVID Cases by Month",
       subtitle ="Plot of recorded COVID cases in the Dataset from European Centre for Disease Prevention and Control")+
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 8)),
        plot.title = element_text(size = 16), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 8),size = 14))+
  geom_point(size=3)

