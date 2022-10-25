packages <- c("tidyverse", "reshape2", "fauxnaif", "gganimate", "ggthemes",
              "stringr", "gridExtra", "gifski", "png", "ggrepel")
lapply(packages, require, character.only = TRUE)

emission <- read.csv("annual-co-emissions-by-region.csv")
target <- c('Indonesia','India','China','European Union (28)','United States' )
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

#GDP per capita
per_capita_all %>% 
  group_by(Country.Name,Year)%>%
  filter(all(Country.Name %in% per_capita_target))%>%
  drop_na()%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=GDP_per_Capita,color=Country.Name),size=1)+
  labs(x="Year", y = "Annual GDP per Capita (Dollars)", 
       color="Region",
       title="Annual GDP per Capita from 1990 to 2018, by World Region")

#Relationship between GDP and CO2 Emission
per_capita_all %>% 
  group_by(Country.Name,Year)%>%
  filter(all(Country.Name=="China"))%>%
  drop_na()%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=GDP_per_Capita,color="GDP per capita"),size=1)+
  geom_line(aes(y=Emission_per_Capita*1000,color="Emission per capita"),size=1)+scale_y_continuous(
    name = "GDP per capita (Dollars)",
    sec.axis = sec_axis(~ . / 1000,name="Emission per capita (Metric Tonnes)")
  ) + 
  labs(x="Year", y = "Annual GDP per Capita (Dollars)", 
       color="Value Type",
       title="GDP and CO2 Emission per capita of China")


#Relationship between GDP and CO2 Emission
sel_country<-c('China','Brazil','Iran','Vietnam',
               'Indonesia','Philippines','India','South Africa',
               'United States','United Kingdom','European Union',
               'Germany','France','Canada','Finland','Japan','Argentina')
per_capita_all %>%
  group_by(Country.Name,Year)%>%
  filter(all(Country.Name %in% sel_country))%>%
  drop_na()%>%
  ggplot(aes(y = GDP_per_Capita, x = Emission_per_Capita)) +
  labs(x="Emissions per capita", y = "Annual GDP per capita (Dollars)") +
  geom_point(size=2,color='red',alpha=0.5)-> p

p + transition_time(Year) +
  geom_text(aes(label = Country.Name),check_overlap = T, nudge_y = -1200)+
  labs(title = "Emissions and Annual GDP per capita of {frame_time}") +
  anim_save("Emissions-GDP.gif", dpi=1200)

 #Oil Price and Consumption
oil_data <- read.csv("world-crude-oil-price-vs-oil-consumption.csv")
world_oil <- subset(oil_data,oil_data$Entity=='World')
coeff <- 2*10^-2
world_oil %>%
  drop_na()%>%
  ggplot(aes(x=Year))+
  geom_vline(xintercept=1979, linetype = "longdash",)+
  annotate(geom="text", x=1983, y=0.5, label="Energy Crisis", size=3) +
  geom_vline(xintercept=2008, linetype = "longdash",)+
  annotate(geom="text", x=2003, y=0.5, label="Great Depression", size=3) +
  geom_vline(xintercept=2019, linetype = "longdash",)+
  annotate(geom="text", x=2014, y=0.5, label="COVID Pandemic", size=3) +
  geom_line(aes(y=Oil.Consumption...Barrels/10^6,color='Oil Consumption'))+
  geom_line(aes(y=Oil...Crude.prices.since.1861..2021...*coeff,color='Crude Oil Prices'))+
  scale_y_continuous(
    name = "Oil Consumption (Million Cubic Meter)",
    sec.axis = sec_axis(~ . / coeff,name="Oil Price ($)")
  ) + 
  labs(x="Year", 
       color="Value Type",
       title="World Crude Price and Consumption")

#Electricity generation
electricity_data <- read.csv("power_generation.csv")
ele_data_melt <- melt(electricity_data, id='Period')
names(ele_data_melt)[names(ele_data_melt) == "variable"] <- "Energy Source"
names(ele_data_melt)[names(ele_data_melt) == "value"] <- "Quantity"
ele_data_melt$Quantity <- as.integer(gsub(",", "", ele_data_melt$Quantity))

ele_data_melt %>%
  group_by(Period)%>%
  drop_na()%>%
  ggplot(aes(fill=`Energy Source`, y=Quantity/1000000.0, x=Period)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(
    breaks=c(2010,2012,2014,2016,2018,2020)
  ) + 
  labs(x="Year", 
       y="Billion Megawatthours",
       title="US Electricty Generation from 2010 to 2020 by Sources")

#Global Electricity generation
global_ele_data <- read.csv("electricity-prod-source.csv")
filter(global_ele_data,Entity == "World")->world_ele
world_ele<- select(world_ele,-c('Entity','Code'))
world_ele_melt <- melt(world_ele, id='Year')
names(world_ele_melt)[names(world_ele_melt) == "variable"] <- "Energy Source"
names(world_ele_melt)[names(world_ele_melt) == "value"] <- "Quantity"

global_ele_share_data <- read.csv("share-elec-by-source.csv")
filter(global_ele_share_data,Entity == "World")->world_ele_share
world_ele_share<- select(world_ele_share,-c('Entity','Code'))
world_ele_share_melt <- melt(world_ele_share, id='Year')
names(world_ele_share_melt)[names(world_ele_share_melt) == "variable"] <- "Energy Source"
names(world_ele_share_melt)[names(world_ele_share_melt) == "value"] <- "Percentage"

world_ele_melt %>%
  filter(Year>=1990)%>%
  group_by(Year)%>%
  drop_na()%>%
  ggplot(aes(fill=`Energy Source`, y=Quantity, x=Year)) + 
  geom_area(position="stack", stat="identity")+
  labs(x="Year", 
       y="Amount of Electrity (TWh)",
       title="World Electricty Generation from 1990 to 2021 by Sources")->ele_amount_plot

world_ele_share_melt %>%
  filter(Year>=1990)%>%
  group_by(Year)%>%
  drop_na()%>%
  ggplot(aes(color=`Energy Source`, y=Percentage, x=Year)) + 
  geom_line(stat="identity")+
  labs(x="Year", 
       y="Percentage of Electricity")->ele_share_plot

grid.arrange(ele_amount_plot, ele_share_plot, ncol=1)->res
ggsave("electricity.jpg",res, width = 20, height = 20,dpi=500, units = "cm")

regions<-c('Oceania','Africa', 'Asia', 'South America', 'North America','Europe')
pop_data <- read.csv("world-population-by-region-with-projections.csv")
filter(pop_data,Entity %in% regions)->pop_sel
pop_sel <- select(pop_sel,-c('Code'))
names(world_ele_share_melt)[names(world_ele_share_melt) == "Entity"] <- "Region"

pop_sel %>%
  filter(Year>=1900)%>%
  group_by(Year)%>%
  drop_na()%>%
  ggplot(aes(fill=Entity, y=Population/10^9, x=Year)) + 
  geom_area(position="stack", stat="identity")+
  labs(x="Year", 
       y="Population (Billion)",
       title="World Population Projection from 1900 to 2100 by United Nations")
