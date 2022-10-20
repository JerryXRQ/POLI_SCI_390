packages <- c("tidyverse", "reshape2", "fauxnaif", "gganimate", "ggthemes",
              "stringr", "gridExtra", "gifski", "png", "ggrepel")
lapply(packages, require, character.only = TRUE)

emission <- read.csv("annual-co-emissions-by-region.csv")
target <- c('China','Europe (excl. EU-28)','European Union (28)','India','United States' )
emission %>% 
  group_by(Entity,Year)%>%
  filter(all(Entity %in% target))%>%
  ggplot(aes(y=Annual.CO2.emissions..zero.filled./10^9,x=Year, color=Entity))+
  labs(x="Year", y = "Annual of CO2 Emissions (Billion Tonnes)", 
       color="Year",
       title="Annual CO2 Emissions from Fossil Fuels from 1750 to 2020, by World Region")+
  geom_line(size=1)

emission %>% 
  group_by(Entity)%>%
  filter(all(Entity %in% target))%>%
  summarize(sum_ems=sum(Annual.CO2.emissions..zero.filled.),na.rm=T)%>%
  ggplot(aes(y=sum_ems/10^9,x=Entity,fill=Entity))+
  labs(x="Region", y = "Sum of CO2 Emissions (Billion Tonnes)", 
       color="Year",
       title="Sum of CO2 Emissions from Fossil Fuels from 1750 to 2020, by World Region")+
  geom_bar(stat='identity')

per_capita_target <- c('China','European Union','India','United States' )

per_capita_emission <- read.csv("emission_per_capita.csv")
drop <- c('Indicator.Name','Country.Code','Indicator.Code')
country_data <- per_capita_emission[,!(names(per_capita_emission) %in% drop)]
country_data_melt <- melt(country_data, id='Country.Name')
country_data_melt <- select(country_data_melt, c("Country.Name","variable","value"))
country_data_melt$variable = substr(country_data_melt$variable,2,5)
country_data_melt$variable = strtoi(country_data_melt$variable)
country_data_melt %>% 
  group_by(Country.Name,variable)%>%filter(all(Country.Name %in% per_capita_target))->res

country_data_melt %>% 
  group_by(Country.Name,variable)%>%
  filter(all(Country.Name %in% per_capita_target))%>%
  drop_na()%>%
  ggplot(aes(y=value,x=variable, color=Country.Name))+
  labs(x="Year", y = "Annual CO2 Emissions per Capita (Metric Tonnes)", 
       color="Region",
       title="Annual CO2 Emissions per Capita from 1990 to 2018, by World Region")+
  geom_line(size=1)

oil_data <- read.csv("world-crude-oil-price-vs-oil-consumption.csv")
world_oil <- subset(oil_data,oil_data$Entity=='World')
coeff <- 2*10^4
world_oil %>%
  drop_na()%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=Oil.Consumption...Barrels,color='Oil Consumption'))+
  geom_line(aes(y=Oil...Crude.prices.since.1861..2021...*coeff,color='Crude Oil Prices'))+
  scale_y_continuous(
    name = "Oil Consumption (Barrels)",
    sec.axis = sec_axis(~ . / coeff,name="Oil Price ($)")
  ) + 
  labs(x="Year", y = "Annual CO2 Emissions per Capita (Metric Tonnes)", 
       color="Region",
       title="Annual CO2 Emissions per Capita from 1990 to 2018, by World Region")

