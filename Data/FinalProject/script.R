#install.packages("zoo") 
packages <- c("tidyverse", "reshape2", "fauxnaif", "gganimate", "ggthemes",
              "stringr", "gridExtra", "gifski", "png", "ggrepel", "scales",
              "lubridate", "paletteer", "GGally", "systemfonts", "extrafont",
              "colorspace", "sf", "rnaturalearth", "ggmap",
              "rnaturalearthdata", "paletteer", "stringr", "haven", "sp","zoo")
lapply(packages, require, character.only = TRUE)

################### Plot 1 ###################
export <- read.csv("world-trade-exports-constant-prices.csv")
cost <- read.csv("real-transport-and-communication-costs.csv")


export %>% 
  group_by(Year)%>%
  ggplot()+
  labs(x="Year", y = "World Trade Relative to 1913 (With 1913 as 100%)",
       title="Global Export Adjusted to Inflation Relative to 1913 Level")+
  theme_stata()+scale_y_continuous(labels = function(x) paste0(x, "%"))+ 
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 8)),
        plot.title = element_text(size = 16), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 8),size = 12))+
  geom_line(size=1,aes(y=World.Trade..relative.to.1913...Federico.and.Tena.Junguito..2016..,x=Year))+
  annotate(geom="text", x=1928, y=500, label="End of World War II", size=3) +
  geom_vline(xintercept=1945, linetype = "longdash")->Export_Plot

colors <- c("International Calling Cost" = "blue", "Passenger Air Transport Cost" = "red", "Sea Freight Cost" = "orange")

cost %>%
  group_by(Year)%>%
  ggplot(aes(x=Year))+
  scale_color_manual(values = colors) +
  geom_line(size=0.8,aes(y=International.calling.costs..relative.to.1930...OECD.Economic.Outlook..2007..,color="International Calling Cost"))+
  geom_line(size=0.8,aes(y=Passenger.air.transport.cost..relative.to.1930...OECD.Economic.Outlook..2007..,color="Passenger Air Transport Cost"))+
  geom_line(size=0.8,aes(y=Sea.freight.cost..relative.to.1930...OECD.Economic.Outlook..2007..,color="Sea Freight Cost"))+
  labs(x = "Year", y= "Cost to 1930", color = "Type of Costs",
       title="Costs Related to Globalization Relative to 1930 Level") + 
  theme_stata()+
  theme(axis.title.x = element_text(size = 14,),
        plot.title = element_text(size = 16), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 8),size = 12))->Cost_Plot
grid.arrange(Export_Plot, Cost_Plot, nrow=2)

################### Plot 2 ###################
export_as_gdp <- read.csv("merchandise-exports-gdp-cepii.csv")

selected_countries <- ne_countries(returnclass = "sf")

inner_join(selected_countries, export_as_gdp, 
           by = c("adm0_a3" = "Code")) -> export_mapping

map_colors <- c("0-20%" = "dodgerblue3", "20-40%" = "chartreuse2", "40-60%" = "darkorange1","60-80%"="firebrick2","80-100%"="darkorchid1")

export_mapping %>% 
  filter(Year==2014)%>%
  group_by(geounit) %>%
  mutate(ranges=cut(Value.of.global.merchandise.exports.as.a.share.of.GDP..Fouquin.and.Hugot..CEPII.2016..National.data.,seq(0, 100, 20),
                    labels=c("0-20%","20-40%","40-60%","60-80%","80-100%"))) %>%
  ggplot(aes(fill = ranges)) + 
  scale_fill_manual(values = map_colors) +
  labs(fill = "Percentage of Export in GDP",title="Value of Exported Goods as Share of GDP in 2014") +
  geom_sf(lwd = 0) + theme_stata()+
  theme(axis.title.x = element_text(size = 14,),
        plot.title = element_text(size = 18), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 8),size = 12))

################### Plot 3 ###################
income_trade <- read.csv("growth-of-income-and-trade.csv")

income_trade %>% 
  group_by(Entity) %>%
  filter(Year>=2000)%>%
  filter(Year<=2014)%>%
  filter(Entity!="Liberia")%>%
  filter(Entity!="Qatar")%>%
  ggplot(aes(x=Value.of.global.merchandise.exports.as.a.share.of.GDP..Fouquin.and.Hugot..CEPII.2016..National.data.,y=GDP.per.capita))+
  geom_point(size=2,color='red',alpha=0.5) + transition_time(Year) + theme_stata()+
  geom_text(aes(label = Entity),check_overlap = T, nudge_y = -800)+
  labs(x = "Value of Merchandise Export as Share of GDP", title="Percentage of Export in GDP versus GDP per capita of {frame_time}", 
       y= "GDP per capita") +
  theme(axis.title.x = element_text(size = 20,),
        plot.title = element_text(size = 24), axis.text.y = element_text(size = 20, angle=0),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(margin = margin(r = 8),size = 20))-> animation
animate(animation, height = 800, width= 1000)
anim_save("GDP-Export.gif")

################### Plot 4 ###################

gini_all <- read.csv("gini-world.csv")

drop <- c('Indicator.Name','Country.Code','Indicator.Code')
gini_all <- gini_all[,!(names(gini_us) %in% drop)]
gini_melt <- melt(gini_all, id='Country.Name')
gini_melt <- select(gini_melt, c("Country.Name","variable","value"))
names(gini_melt)[names(gini_melt) == "variable"] <- "Year"
names(gini_melt)[names(gini_melt) == "value"] <- "Gini"
gini_melt$Year = substr(gini_melt$Year,2,5)
gini_melt$Year = strtoi(gini_melt$Year)
gini_melt$Gini = as.double(gini_melt$Gini)

import_all <- read.csv("import-world.csv")

import_all <- import_all[,!(names(import_all) %in% drop)]
import_melt <- melt(import_all, id='Country.Name')
import_melt <- select(import_melt, c("Country.Name","variable","value"))
names(import_melt)[names(import_melt) == "variable"] <- "Year"
names(import_melt)[names(import_melt) == "value"] <- "Import"
import_melt$Year = substr(import_melt$Year,2,5)
import_melt$Year = strtoi(import_melt$Year)

inner_join(import_melt, gini_melt, 
           by = c("Year" = "Year", "Country.Name" = "Country.Name")) -> complete_inequality
country_set <- c('Germany', 'United States',"France",'United Kingdom','Japan','Australia')

complete_inequality %>%
  group_by(Country.Name)%>%
  filter(all(Country.Name %in% country_set))%>%
  drop_na()%>%
  filter(Year>=1995)%>%
  ggplot(aes(x=Year))+
  facet_wrap(~ Country.Name, scales="free")+
  geom_line(aes(y = Import,color='Import as Percentage of GDP'),size=0.8)+
  geom_line(aes(y = Gini,color='Gini Index'),size=0.8)+
  theme_stata()+
  labs(x = "Year", title="Gini Index and Import of 6 Developed Countries after 1995", 
       color = "Data Type")+
  theme(axis.title.y=element_blank(), axis.title.x = element_text(size = 12))

################### Plot 5 ###################
trade_war_data <- read.csv("trade_war_data.csv")
trade_war_data %>% mutate(time=as.Date(Date, "%d-%B-%Y"))->trade_war

tariff_colors <- c("US Tariff on ROW exports" = "blue", "US Tariff on Chinese exports" = "blue", 
                   "Chinese Tariff on ROW exports" = "red","Chinese Tariff on US exports"="red")
line_type <- c("US Tariff on ROW exports" = "dashed", "US Tariff on Chinese exports" = "solid", 
               "Chinese Tariff on ROW exports" = "dashed","Chinese Tariff on US exports"="solid")
trade_war %>%
  ggplot(aes(x=time))+
  scale_color_manual(values = tariff_colors) +
  scale_linetype_manual(values = line_type) +
  geom_line(aes(y=US.tariffs.on.ROW.exports,color="US Tariff on ROW exports",linetype="US Tariff on ROW exports"))+
  geom_line(aes(y=Chinese.tariffs.on.ROW.exports,color="Chinese Tariff on ROW exports",linetype="Chinese Tariff on ROW exports"))+
  geom_line(aes(y=US.tariffs.on.Chinese.exports,color="US Tariff on Chinese exports",linetype="US Tariff on Chinese exports"))+
  geom_line(aes(y=Chinese.tariffs.on.US.exports,color="Chinese Tariff on US exports",linetype="Chinese Tariff on US exports"))+
  geom_vline(xintercept=as.Date("6-Jul-2018", "%d-%B-%Y"), linetype = "longdash",alpha=0.5)+
  geom_vline(xintercept=as.Date("14-Feb-2020", "%d-%B-%Y"), linetype = "longdash",alpha=0.5)+
  theme_stata()+
  annotate(geom="text", x=as.Date("1-Oct-2018", "%d-%B-%Y"), y=24, label="Trade War", size=3.5) +
  annotate(geom="text", x=as.Date("30-Jul-2020", "%d-%B-%Y"), y=24, label="Phase One Agreement", size=3.5) +
  theme(axis.title.x = element_text(size = 14,),
        plot.title = element_text(size = 16), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 8),size = 12))+
  labs(x = "Time", title="US-China Trade War Tariff Plot",linetype = "Variable",
       color = "Variable", y="Tariff Level")

################### Plot 6 ###################
trade_data <- read.csv("us-china-trade.csv")
trade_data %>% mutate(time=as.yearmon(Month, "%B-%y"))->trade_data

trade_data$Exports = as.double(gsub(",", "", trade_data$Exports))
trade_data$Imports = as.double(gsub(",", "", trade_data$Imports))

trade_data %>%
  filter(time>=as.yearmon("Jan-18", "%B-%y"))%>%
  filter(time<=as.yearmon("Jan-21", "%B-%y"))%>%
  ggplot(aes(x=time))+
  theme_stata()+
  geom_bar(aes(y=Exports),stat='identity', colour="dodgerblue", fill="deepskyblue",alpha=0.5)+
  theme(axis.title.x = element_text(size = 14,),
        plot.title = element_text(size = 16), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 8),size = 12))+
  labs(x = "Time", title="US Export to China",y="Export in Millions of USD")-> Export_Plot

trade_data %>%
  filter(time>=as.yearmon("Jan-18", "%B-%y"))%>%
  filter(time<=as.yearmon("Jan-21", "%B-%y"))%>%
  ggplot(aes(x=time))+
  theme_stata()+
  geom_bar(aes(y=Imports),stat='identity', colour="dodgerblue", fill="deepskyblue",alpha=0.5)+
  theme(axis.title.x = element_text(size = 14,),
        plot.title = element_text(size = 16), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 8),size = 12))+
  labs(x = "Time", title="US Import from China",y="Import in Millions of USD")-> Import_Plot

grid.arrange(Export_Plot, Import_Plot, nrow=2)

################### Plot 7 ###################

