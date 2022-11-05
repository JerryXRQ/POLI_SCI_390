#install.packages("rlang") 
# install devtools, then use that to install the vdem data
#install.packages("devtools")
devtools::install_github("vdeminstitute/vdemdata")

library(devtools)
library(vdemdata)

packages <- c("tidyverse", "reshape2", "fauxnaif", "gganimate", "ggthemes",
              "stringr", "gridExtra", "gifski", "png", "ggrepel", "scales",
              "lubridate", "paletteer", "GGally", "systemfonts", "extrafont", "colorspace")
lapply(packages, require, character.only = TRUE)
loadfonts(device = "all")
summary(vdem$v2peapsgeo_osp)

new_labels <- c("1" = "Eastern Europe and Central Asia", "2" = "Latin America and the Caribbean", "3" = "The Middle East and North Africa", 
                "4" = " Sub-Saharan Africa", "5"="Western Europe and North America ","6"="Asia and Pacific")
vdem %>% 
  filter(year == 2020) %>% 
  ggplot(aes(y = v2xnp_regcorr, x = v2mecenefm)) + 
  facet_wrap(~ e_regionpol_6C, scales="free",labeller = labeller(e_regionpol_6C = new_labels)) +
  geom_text(aes(label = country_name),size=2.0,check_overlap = T, nudge_y = -0.02)+
  labs(x = "Government censorship effort—Media", y = "Regime corruption index", 
       title = "Relationship between Government Censorship of Media and Regime Corruption, 2020") +
  geom_point()

vdem %>% 
  filter(year == 2020) %>% 
  mutate(ranges=cut(v2mecenefm,seq(-4, 4, 1),labels=c("-4 to -3","-3 to -2","-2 to -1","-1 to 0","-1 to 0","0 to 1","1 to 2","2 to 3"))) %>%
  group_by(ranges)%>%
  summarize(Mean=mean(v2xnp_regcorr),sample_size = n(),
            se = sd(v2xnp_regcorr)/sqrt(sample_size))%>%
  mutate(lower_bound = Mean - (se*1.96), 
         upper_bound = Mean + (se*1.96))%>%
  ggplot(aes(y = Mean, x = ranges,fill=as_factor(ranges))) +
  labs(x = "Range of Government Media Censorship Effort Index", y = "Average Regime Corruption Index",
       title = "Relationship between Government Censorship of Media and Regime Corruption, 2020") +
  theme(legend.position = "none")+
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.3,size=0.5)
  