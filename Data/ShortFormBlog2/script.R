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

vdem %>% 
  filter(year == 2010) %>% 
  ggplot(aes(y = v2xnp_regcorr, x = v2mecenefm)) + 
  geom_text(aes(label = country_name),check_overlap = T, nudge_y = -0.03)+
  geom_point()
  