packages <- c("tidyverse", "reshape2", "fauxnaif", "gganimate", "ggthemes",
              "stringr", "gridExtra", "gifski", "png", "ggrepel", "scales", "ggdist",
              "lubridate", "paletteer", "GGally", "systemfonts", "extrafont", "colorspace")
lapply(packages, require, character.only = TRUE)

export <- read.csv("world-trade-exports-constant-prices.csv")

export %>% 
  group_by(Entity,Year)%>%
  ggplot(aes(y=World.Trade..relative.to.1913...Federico.and.Tena.Junguito..2016..,x=Year))+
  labs(x="Year", y = "World Trade Relative to 1913 (With 1913 as 100%)",
       title="Global Export Adjusted to Inflation Relative to 1913 Level")+
  theme_stata()+scale_y_continuous(labels = function(x) paste0(x, "%"))+ 
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 8)),
        plot.title = element_text(size = 18), axis.text.y = element_text(size = 10, angle=0),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 8),size = 12))+
  geom_line(size=1)+
  annotate(geom="text", x=1928, y=500, label="End of World War II", size=3) +
  geom_vline(xintercept=1945, linetype = "longdash")
