packages <- c("tidyverse", "reshape2", "fauxnaif", "gganimate", "ggthemes",
              "stringr", "gridExtra", "gifski", "png", "ggrepel")
lapply(packages, require, character.only = TRUE)

emission <- read.csv("annual-co-emissions-by-region.csv")
target <- c('China','Europe (excl. EU-28)','European Union (28)','India','United States' )
emission %>% 
  group_by(Entity,Year)%>%
  filter(all(Entity %in% target))%>%
  summarize(sum_ems=sum(Annual.CO2.emissions..zero.filled.),na.rm=T)%>%
  ggplot(aes(y=sum_ems/10^9,x=Year, color=Entity))+
  labs(x="Region", y = "Annual of CO2 Emissions (Billion Tonnes)", 
       color="Year",
       title="Annual CO2 Emissions from Fossil Fuels from 1750 to 2020, by World Region")+
  geom_line()

emission %>% 
  group_by(Entity)%>%
  filter(all(Entity %in% target))%>%
  summarize(sum_ems=sum(Annual.CO2.emissions..zero.filled.),na.rm=T)%>%
  ggplot(aes(y=sum_ems/10^9,x=Entity,fill=Entity))+
  labs(x="Region", y = "Sum of CO2 Emissions (Billion Tonnes)", 
       color="Year",
       title="Sum of CO2 Emissions from Fossil Fuels from 1750 to 2020, by World Region")+
  geom_bar(stat='identity')
