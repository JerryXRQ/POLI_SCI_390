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
per_capita_GDP <- read.csv("GDP_per_capita.csv")

drop <- c('Indicator.Name','Country.Code','Indicator.Code')
ems_country_data <- per_capita_emission[,!(names(per_capita_emission) %in% drop)]
ems_country_data_melt <- melt(ems_country_data, id='Country.Name')
ems_country_data_melt <- select(ems_country_data_melt, c("Country.Name","variable","value"))
names(ems_country_data_melt)[names(ems_country_data_melt) == "variable"] <- "Year"
names(ems_country_data_melt)[names(ems_country_data_melt) == "value"] <- "Emission_per_Capita"
ems_country_data_melt$Year = substr(ems_country_data_melt$Year,2,5)
ems_country_data_melt$Year = strtoi(ems_country_data_melt$Year)

gdp_country_data <- per_capita_GDP[,!(names(per_capita_GDP) %in% drop)]
gdp_country_data_melt <- melt(gdp_country_data, id='Country.Name')
gdp_country_data_melt <- select(gdp_country_data_melt, c("Country.Name","variable","value"))
names(gdp_country_data_melt)[names(gdp_country_data_melt) == "variable"] <- "Year"
names(gdp_country_data_melt)[names(gdp_country_data_melt) == "value"] <- "GDP_per_Capita"
gdp_country_data_melt$Year = substr(gdp_country_data_melt$Year,2,5)
gdp_country_data_melt$Year = strtoi(gdp_country_data_melt$Year)


per_capita_all <- merge(x=gdp_country_data_melt,y=ems_country_data_melt,by=c("Year","Country.Name"))

per_capita_all %>% 
  group_by(Country.Name,Year)%>%filter(all(Country.Name %in% per_capita_target))->res

per_capita_all %>% 
  group_by(Country.Name,Year)%>%
  filter(all(Country.Name %in% per_capita_target))%>%
  drop_na()%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=Emission_per_Capita,color=Country.Name),size=1)+
  labs(x="Year", y = "Annual CO2 Emissions per Capita (Metric Tonnes)", 
       color="Region",
       title="Annual CO2 Emissions per Capita from 1990 to 2018, by World Region")

per_capita_all %>% 
  group_by(Country.Name,Year)%>%
  filter(all(Country.Name %in% per_capita_target))%>%
  drop_na()%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=GDP_per_Capita,color=Country.Name),size=1)+
  labs(x="Year", y = "Annual GDP per Capita (Dollars)", 
       color="Region",
       title="Annual GDP per Capita from 1990 to 2018, by World Region")

per_capita_all %>% 
  group_by(Country.Name,Year)%>%
  filter(all(Country.Name=="China"))%>%
  drop_na()%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=GDP_per_Capita,color="GDP per capita"),size=1)+
  geom_line(aes(y=Emission_per_Capita/50,color="Emission per capita"),size=1)+
  labs(x="Year", y = "Annual GDP per Capita (Dollars)", 
       color="Region",
       title="Annual GDP per Capita from 1990 to 2018, by World Region")

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

