#install.packages("rlang") 
# install devtools, then use that to install the vdem data
#install.packages("ggdist")
devtools::install_github("vdeminstitute/vdemdata")

library(devtools)
library(vdemdata)

packages <- c("tidyverse", "reshape2", "fauxnaif", "gganimate", "ggthemes",
              "stringr", "gridExtra", "gifski", "png", "ggrepel", "scales", "ggdist",
              "lubridate", "paletteer", "GGally", "systemfonts", "extrafont", "colorspace")
lapply(packages, require, character.only = TRUE)
loadfonts(device = "all")

new_labels <- c("1" = "Eastern Europe and Central Asia", "2" = "Latin America and the Caribbean", "3" = "The Middle East and North Africa", 
                "4" = " Sub-Saharan Africa", "5"="Western Europe and North America ","6"="Asia and Pacific")
vdem %>% 
  filter(year == 2020) %>% 
  ggplot(aes(y = v2x_execorr, x = v2mecenefm_osp)) + 
  facet_wrap(~ e_regionpol_6C, scales="free",labeller = labeller(e_regionpol_6C = new_labels)) +
  geom_text(aes(label = country_name),size=3.0,check_overlap = T, nudge_y = -0.02)+
  labs(x = "Government Censorship Effort—Media", y = "Executive Corruption Index ", 
       title = "Relationship between Government Censorship of Media and Executive Corruption, 2020") +
  theme_stata()+theme(axis.title.x = element_text(size = 18,margin = margin(t = 8)),axis.title.y = element_text(size = 18,margin = margin(r = 8)),
                      plot.title = element_text(size = 20), axis.text.y = element_text(size = 13),
                      axis.text.x = element_text(size = 13),
                      strip.text.x = element_text(size = 15),
                      panel.spacing = unit(1.2, "lines"))+
  geom_point()->plot1
plot1
ggsave(plot=plot1,filename="Plot1.png",dpi = 1000)

vdem %>% 
  filter(year == 2020) %>% 
  mutate(ranges=cut(v2mecenefm,seq(-4, 4, 1),labels=c("-4 to -3","-3 to -2","-2 to -1","-1 to 0","-1 to 0","0 to 1","1 to 2","2 to 3"))) %>%
  group_by(ranges)%>%
  summarize(Mean=mean(v2x_execorr),sample_size = n(),
            se = sd(v2x_execorr)/sqrt(sample_size))%>%
  mutate(lower_bound = Mean - (se*1.96), 
         upper_bound = Mean + (se*1.96))%>%
  ggplot(aes(y = Mean, x = ranges,fill=as_factor(ranges))) +
  labs(x = "Range of Government Media Censorship Effort Index", y = "Average Executive Corruption Index",
       title = "Relationship between Government Censorship of Media and Executive Corruption, 2020") +
  theme_stata()+
  theme(legend.position = "none")+
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.3,size=0.5)+ 
  theme(axis.title.x = element_text(size = 20, margin = margin(t = 8),),
        plot.title = element_text(size = 24), axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 8),size = 20))
  
vdem %>% filter(year>2000)%>% 
  group_by(e_regionpol_6C,year) %>%
  summarize(mean_corr=mean(v2x_execorr), mean_cens=mean(v2mecenefm))%>%
  ggplot() + 
  facet_wrap(~ e_regionpol_6C, scales="free",labeller = labeller(e_regionpol_6C = new_labels)) +
  labs(x = "Year", y = "Censorship and Corruption Index", color="Data Type",
       title = "Relationship between Government Censorship of Media and Executive Corruption from 2000 to 2020") +
  geom_line(aes(y = mean_corr, x = year,color='Mean Corruption Index'),size=0.8)+
  geom_line(aes(y = mean_cens, x = year,color='Mean Censorship Index'),size=0.8)+ 
  theme_stata() + theme(axis.title.x = element_text(size = 18,margin = margin(t = 8)),axis.title.y = element_text(size = 18,margin = margin(r = 8)),
                        plot.title = element_text(size = 24),legend.text = element_text(size = 13),strip.text.x = element_text(size = 15),
                        panel.spacing = unit(1.2, "lines"),axis.text.x = element_text(size = 13),axis.text.y = element_text(size = 13),)

vdem %>% 
  filter(year == 2020) %>% 
  mutate(ranges=cut(v2mecenefm,seq(-4, 4, 1),labels=c("-4 to -3","-3 to -2","-2 to -1","-1 to 0","-1 to 0","0 to 1","1 to 2","2 to 3"))) %>%
  group_by(ranges)%>%
  mutate(average=mean(v2x_execorr)) %>%
  ggplot(aes(x=v2x_execorr,fill=ranges))+
  labs(x = "Executive Corruption Index", y = "Density in Corresponding Media Censorship Level",
       title = "Density Plot of Executive Corruption Index based on Level of Government Censorship of Media, 2020") +
  theme_stata()+
  geom_density() +
  geom_vline(aes(xintercept=average),
           color="blue", linetype="dashed", size=1) +
  facet_wrap(~ ranges, scales="free")+
  theme(axis.title.x = element_text(size = 18,margin = margin(t = 8)),axis.title.y = element_text(size = 18,margin = margin(r = 8)),
        plot.title = element_text(size = 24), axis.text.y = element_text(size = 14),strip.text.x = element_text(size = 15),
       axis.text.x = element_text(size = 14),legend.position = "none") ->plot2
plot2
ggsave(plot=plot2,filename="Plot4.jpg",width = 15, height = 8)


