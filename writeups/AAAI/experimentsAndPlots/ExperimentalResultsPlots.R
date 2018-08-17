# Experimental results plots

library(tidyverse)
library(RColorBrewer)

algo_colors <- brewer.pal(n = 5,name = "Set1")[c(2,1,5)]

setwd("C:/Users/maurerkt/Documents/GitHub/uuutils/writeups/AAAI/experimentsAndPlots")
bw_results <- read.csv("bansalWeldAugust15.csv")
head(bw_results)

bw_monte_carlo_envelope <- bw_results  %>%
  mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv"),
                          labels=c("pang04","pang05","mcauley15")) ) %>%
  group_by(phi, b, dataset) %>%
  summarize(lower=quantile(utility,.05),
            upper=quantile(utility,.95),
            q3=quantile(utility,.75),
            q1=quantile(utility,.25),
            median = median(utility, na.rm=T))%>%
  mutate(algo=ifelse(phi=="most_uncertain","Most Uncertain","Coverage-Based"),
         algo = factor(algo, levels=c("Most Uncertain","Coverage-Based"))) %>%
  as.data.frame()


ggplot()+
  geom_ribbon(aes(x=b,ymin=lower,ymax=upper,
                  group=phi, color=algo,fill=algo),
              linetype=2, alpha=.04,
              data=bw_monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=algo),
            data=bw_monte_carlo_envelope,size=1.5)+
  facet_wrap(~dataset, scales="free_y",nrow=2)+
  scale_color_manual("Algorithm:", values=algo_colors[c(1,2)])+
  scale_fill_manual("Algorithm:", values=algo_colors[c(1,2)])+
  theme_bw()+
  labs(x="Query Step (b)",
       y="Coverage-Based Utility")+
  theme(axis.title = element_text(size=15),
        legend.position = "bottom")

ggsave("CoverageVsMostUncertainPlaceholder.png", dpi=600,
       height=5,width=7,units="in")

#------------------------------------------------------------------------------

mb_results <- read.csv("maurerBennetteAugust15.csv")
str(mb_results)
head(mb_results)
summary(mb_results)

monte_carlo_envelope <- mb_results  %>%
  mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv"),
                          labels=c("pang04","pang05","mcauley15")) ) %>%
  group_by(phi, b, dataset) %>%
  summarize(lower=quantile(utility,.05),
            upper=quantile(utility,.95),
            q3=quantile(utility,.75),
            q1=quantile(utility,.25),
            median = median(utility, na.rm=T))%>%
  ungroup() %>%
  mutate(algo=factor(phi,levels=c("cluster_prior","logistic","most_uncertain"),
                     labels=c("Cluster FL","Facility Locations","Most Uncertain")),
         algo=as.character(algo)) %>%
  filter(algo != "Cluster FL") %>%
  mutate(algo = factor(algo, levels=c("Most Uncertain","Facility Locations")))
        
ggplot()+
  geom_ribbon(aes(x=b,ymin=lower,ymax=upper,
                  group=algo, color=algo,fill=algo, 
                  linetype=algo),size=1
              alpha=.1,
              data=monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=algo,linetype=algo),
            data=monte_carlo_envelope,size=1)+
  facet_grid(dataset~., scales="free_y")+
  scale_color_manual("Algorithm:", values=algo_colors[c(1,3)])+
  scale_fill_manual("Algorithm:", values=algo_colors[c(1,3)])+
  theme_bw()+
  labs(x="Query Step (b)",
       y="Coverage-Based Utility")+
  theme(axis.title = element_text(size=15),
        legend.position = "bottom")

ggsave("flUtilPlaceholder.png", dpi=600,
       height=6,width=3.75,units="in")

#-------------------------------------------------------------------------------
#  Standardized mortality ratio style comparison 
#  number of UUs found : number UUs exected under confidence

bw_smr <- bw_results %>%
  filter(b==100) %>%
  mutate(algo = factor(phi,levels=c("cluster_prior","most_uncertain"),
                     labels=c("Coverage-Based","Most Uncertain")),
         smr = found/(B-cumulativeConfidence)) %>%
  group_by(dataset, algo) %>%
  summarize(smrLower = quantile(smr,.05),
            smrMedian = quantile(smr,.5),
            smrUpper = quantile(smr,.95))
bw_smr

mb_smr <- mb_results %>%
  filter(b==100) %>%
  mutate(algo = factor(phi,levels=c("cluster_prior","logistic","most_uncertain"),
                       labels=c("Cluster FL","Facility Locations","Most Uncertain")),
         smr = found/(B-cumulativeConfidence)) %>%
  group_by(dataset, algo) %>%
  summarize(smrLower = quantile(smr,.05),
            smrMedian = quantile(smr,.5),
            smrUpper = quantile(smr,.95)) 


smr <- rbind(bw_smr, filter(mb_smr, algo != "Most Uncertain")) %>%
  filter(algo %in% c("Coverage-Based","Facility Locations","Most Uncertain")) %>%
  ungroup() %>%
  mutate(algo = factor(algo, levels=c("Most Uncertain","Coverage-Based","Facility Locations")),
         dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv"),
                          labels=c("pang04","pang05","mcauley15")) ) 

smr_table <- smr %>%
  mutate(value = paste0(round(smrMedian,2)," (",round(smrLower,2),",",round(smrUpper,2),")")) %>%
  select(dataset,algo,value) %>%
  spread(key=algo, value=value)
smr_table
library(xtable)

print(xtable(smr_table), include.rownames=FALSE)




ggplot() +
  geom_errorbar(aes(ymin=smrLower,ymax=smrUpper,
                    color=algo, x=dataset),width=.5,
                position = "dodge", data=smr, size=1) +
  geom_point(aes(y=smrMedian, color=algo, x=dataset),
                position=position_dodge(width=.5), 
             shape=18, data=smr, size=3) +
  scale_color_manual("Algorithm:", values=algo_colors) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=seq(0,10,2), 
                     limits=c(0,max(smr$smrUpper))) +
  labs(y="Misclassification Discovery Ratio \n Found : Expected",
       x="") +
  theme_bw() +
  theme(legend.position = c(.65,.35),
        axis.text.y= element_text(angle=90, hjust=.5))+
  coord_flip()

ggsave("discoveryRatioPlaceholder.png", dpi=600, 
       height=3,width=4.5,units="in")

#--------------------------------------------------------------------------------------------------------------------

bw_conf <- bw_results  %>%
  mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv"),
                          labels=c("pang04","pang05","mcauley15")),
         algo=ifelse(phi=="most_uncertain","Most Uncertain","Coverage-Based"),
         algo = factor(algo, levels=c("Most Uncertain","Coverage-Based"))) %>%
  group_by(algo,dataset,iteration) %>%
  mutate(conf = c(cumulativeConfidence[1],diff(cumulativeConfidence)))%>%
  as.data.frame()

ggplot() +
  geom_density(aes(x=conf, color=algo), 
               data=bw_conf) +
  theme_bw() +
  facet_grid(dataset~.)

mb_conf <- mb_results  %>%
  mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv"),
                          labels=c("pang04","pang05","mcauley15")),
         algo=factor(phi,levels=c("cluster_prior","logistic","most_uncertain"),
                     labels=c("Cluster FL","Facility Locations","Most Uncertain"))) %>%
  group_by(algo,dataset,iteration) %>%
  mutate(conf = c(cumulativeConfidence[1],diff(cumulativeConfidence)))%>%
  as.data.frame() %>%
  filter(algo == "Facility Locations")

head(mb_conf)
head(bw_conf)

all_conf <- rbind(select(bw_conf, algo,conf,dataset,iteration), 
                  select(mb_conf, algo,conf,dataset,iteration) )

conf_density <- ggplot() +
  geom_density(aes(x=conf, color=algo), 
               data=all_conf ) +
  theme_bw() +
  facet_grid(.~dataset, scales="free_y")+
  theme(legend.position = "bottom")

# combine with overconfidence plot from final_report_plots.R code
gridExtra::grid.arrange(overconf_faceted, conf_density, nrow=2)
  