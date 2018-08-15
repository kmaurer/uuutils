# Experimental results plots

library(tidyverse)

setwd("C:/Users/maurerkt/Documents/GitHub/uuutils/writeups/AAAI/experimentsAndPlots")
bw_results <- read.csv("bansalWeldAugust15.csv")
head(bw_results)

monte_carlo_envelope <- bw_results  %>%
  group_by(phi, b, dataset) %>%
  summarize(lower=quantile(utility,.05),
            upper=quantile(utility,.95),
            q3=quantile(utility,.75),
            q1=quantile(utility,.25),
            median = median(utility, na.rm=T))%>%
  mutate(algo=ifelse(phi=="most_uncertain","Most Uncertain","Coverage-Based"))

ggplot()+
  geom_ribbon(aes(x=b,ymin=lower,ymax=upper,
                  group=phi, color=algo,fill=algo),linetype=2,
              alpha=.04,
              data=monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=algo),
            data=monte_carlo_envelope,size=1.5)+
  facet_grid(dataset~., scales="free_y")+
  scale_color_manual("Algorithm:", values=c("red","black"))+
  scale_fill_manual("Algorithm:", values=c("red","black"))+
  theme_bw()+
  labs(x="Query Step (b)",
       y="Coverage-Based Utility")+
  theme(axis.title = element_text(size=15),
        legend.position = "bottom")

ggsave("CoverageVsMostUncertainPlaceholder.png", dpi=600)


mb_results <- read.csv("maurerBennetteAugust15.csv")
str(mb_results)
head(mb_results)
summary(mb_results)

monte_carlo_envelope <- mb_results  %>%
  group_by(phi, b, dataset) %>%
  summarize(lower=quantile(utility,.05),
            upper=quantile(utility,.95),
            q3=quantile(utility,.75),
            q1=quantile(utility,.25),
            median = median(utility, na.rm=T))%>%
  ungroup() %>%
  mutate(algo=factor(phi,levels=c("cluster_prior","logistic","most_uncertain"),
                     labels=c("Coverage-Based","Facility Locations","Most Uncertain")))
        
table(monte_carlo_envelope$phi, monte_carlo_envelope$algo) 
        
ggplot()+
  geom_ribbon(aes(x=b,ymin=lower,ymax=upper,
                  group=algo, color=algo,fill=algo),linetype=2,
              alpha=.04,
              data=monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=algo),
            data=monte_carlo_envelope,size=1.5)+
  facet_grid(dataset~., scales="free_y")+
  scale_color_manual("Algorithm:", values=c("red","blue","black"))+
  scale_fill_manual("Algorithm:", values=c("red","blue","black"))+
  theme_bw()+
  labs(x="Query Step (b)",
       y="Coverage-Based Utility")+
  theme(axis.title = element_text(size=15),
        legend.position = "bottom")



ggplot()+
  geom_line(aes(x=b,y=utility, color=phi, group=interaction(phi,iteration)), data=kw_results)+
  facet_grid(dataset ~ ., scales="free_y") + 
  labs(title="Evidence showing favors most uncertain" ,
       subtitle="standin plot")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave("CoverageVsMostUncertainPlaceholder.png", dpi=600)
