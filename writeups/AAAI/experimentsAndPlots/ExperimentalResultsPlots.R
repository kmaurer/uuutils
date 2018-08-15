# Experimental results plots

library(tidyverse)

setwd("C:/Users/maurerkt/Documents/GitHub/uuutils/writeups/AAAI/experimentsAndPlots")
bw_results <- read.csv("bansalWeldAugust15.csv")
head(bw_results)

filter(bw_results)

ggplot()+
  geom_line(aes(x=b,y=utility, color=phi, group=interaction(phi,iteration)), data=bw_results)+
  facet_grid(dataset ~ ., scales="free_y") + 
  labs(title="Evidence showing favors most uncertain" ,
       subtitle="standin plot")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave("CoverageVsMostUncertainPlaceholder.png", dpi=600)
