packages <- c("tidyverse", "reshape2", "fauxnaif", "gganimate", "ggthemes",
              "stringr", "gridExtra", "gifski", "png", "ggrepel")
lapply(packages, require, character.only = TRUE)

library(dplyr)
detach(package:plyr)
emission <- read.csv("annual-co-emissions-by-region.csv")
target <- c('Indonesia','India','China','European Union (28)','United States' )

emission %>% 
  group_by(Entity)%>%
  filter(all(Entity %in% target))%>%
  filter(Year>2010)%>%
  mutate(ems_val=Annual.CO2.emissions..zero.filled./10^9)->sum_ems_res
  #Find the emission in the given time period of the selected regions
  #Change the unit to billion tonnes

sum_ems_res %>%
  group_by(Entity) %>%
  summarise(mean_ems = mean(ems_val, na.rm = T), 
            se_ems = sd(ems_val, na.rm = T)/sqrt(10)) %>%
  #Calculate the mean and average for each country/region
  mutate(lower_bound = mean_ems - (se_ems*1.96), 
        upper_bound = mean_ems + (se_ems*1.96))-> sum_ems_res
  #Assume a 95% confidence, we can use the equation above to find confidence interval


sum_ems_res %>%
  ggplot(aes(Entity, mean_ems, fill = Entity)) +
  geom_bar(stat = "identity") + 
  labs(x="Region", y = "Average CO2 Emissions (Billion Tonnes)", 
       title="2010-2020 Average CO2 Emissions from Fossil Fuels, by World Region") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.5,size=0.8)

data <- na.omit(read.csv("VisBlog1.csv"))
data %>% 
  group_by(year,month,day)%>%
  filter(year==2021)%>%
  summarise(total = sum(cases, na.rm = T))%>%
  mutate(total=total/1000)->cleaned
#Select the 2021 data and sum up the case counts for all EU countries

cleaned %>% 
  group_by(month)%>%
  summarise(sample_size = n(),
            #find the number of days in each month
            mean_cases=mean(total, na.rm = T),
            #find the average number of cases per day for every month
            se_cases = sd(total, na.rm = T)/sqrt(sample_size))%>%
            #find the standard error
  mutate(lower_bound = mean_cases - (se_cases*1.96), 
         upper_bound = mean_cases + (se_cases*1.96))-> cases_data
            #integrate the standard error by finding the upper and lower bound


cases_data %>%
  ggplot(aes(month, mean_cases)) +
  geom_line(linetype = "dashed",size=0.8) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  labs(x="Month", y = "Confirmed COVID Cases per Day (Thousand)", 
       title="EU 2021 Average Daily Confirmed COVID Cases by Month") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1,size=0.8)+
  geom_point(color="red")
  

