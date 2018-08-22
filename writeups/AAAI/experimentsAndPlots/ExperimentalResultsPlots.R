# Experimental results plots

library(tidyverse)
library(RColorBrewer)

# algo_colors <- brewer.pal(n = 5,name = "Set1")[c(2,1,5)]
algo_colors <- c("#d95f02","#1b9e77","#7570b3")

setwd("C:/Users/maurerkt/Documents/GitHub/uuutils/writeups/AAAI/experimentsAndPlots")

#---------------------------------------------------------------------------------------
# Figure 1: comparing MU and BW on coverage-based utility
bw_results <- read.csv("C:/Users/maurerkt/Downloads/outputFiles/bansalWeld.csv")
head(bw_results)
unique(bw_results$dataset)

bw_monte_carlo_envelope <- bw_results  %>%
  mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv","kaggle"),
                          labels=c("pang04","pang05","mcauley15","kaggle13")) ) %>%
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

# ggsave("CoverageVsMostUncertainPlaceholder.png", dpi=600,
#        height=5,width=7,units="in")

#---------------------------------------------------------------------
# Figure 2: overconfidence plot
library(ggplot2)
library(dplyr)

# create cubic splines model to fit rate of correct class as function of confidence
datasetvec <- c("pang04","pang05","mcauley15","kaggle13")
all_overconfidence <- NULL
for (dataset in datasetvec){
  dat <- read.csv(paste0("C:/Users/maurerkt/Documents/GitHub/uuutils/writeups/AAAI/experimentsAndPlots/inputFiles/",dataset,"_predictionResults.csv"))
  dat <- dplyr::filter(dat, Confidence > .65, Prediction==1)
  mod <- lm(1-dat$Misclassified ~ splines::bs(dat$Confidence, 3))
  all_overconfidence <- rbind(all_overconfidence,
                              data.frame(c_MX =dat$Confidence,
                                         overconfidence = dat$Confidence - predict(mod, data.frame(c_MX=dat$Confidence)),
                                         data_source = dataset)) 
}
head(all_overconfidence)

ggplot()+
  geom_line(aes(x=c_MX,y=overconfidence),
            size=1, data=all_overconfidence)+
  geom_hline(yintercept = 0)+
  # geom_text(aes(x=x,y=y,label=label,hjust=hjust), data=labels_data,
  #           color="darkorange")+
  facet_wrap(~data_source, scales="free_y")+
  theme_bw()+
  labs(x="Model Confidence", y="Overconfidence on Test Points")

# ggsave("overconfidence.png",dpi=600,width=4,height=3.8,units="in")

#---------------------------------------------------------------------------------------
# Figure 3: comparing MU and FL searches on FL utility

mb_results <- read.csv("C:/Users/maurerkt/Downloads/maurerBennetteAugust20.csv")
str(mb_results)
head(mb_results)
summary(mb_results)

#!# temporary kaggle addition (final version should save all experiments to single file)
mb_kag <- read.csv("C:/Users/maurerkt/Downloads/maurerBennetteKaggle.csv")
head(mb_kag)
#!# fix var order
mb_kag <- mb_kag %>%
  select(cost:B,dataset,utilityType)
mb_results <- rbind(mb_results,mb_kag)
head(mb_results)

# mb_results <- read.csv("C:/Users/maurerkt/Downloads/outputFiles/maurerBennette.csv")
# head(mb_results)
# unique(mb_results$dataset)
# unique(mb_results$phi)

monte_carlo_envelope <- mb_results  %>%
  mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv","kaggle"),
                          labels=c("pang04","pang05","mcauley15","kaggle13")) ) %>%
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
                  group=algo, color=algo,fill=algo),
              linetype=2, alpha=.1,
              data=monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=algo),linetype=1,size=1,
            data=monte_carlo_envelope)+
  facet_wrap(~dataset, scales="free_y")+
  scale_color_manual("Algorithm:", values=algo_colors[c(1,3)])+
  scale_fill_manual("Algorithm:", values=algo_colors[c(1,3)])+
  theme_bw()+
  labs(x="Query Step (b)",
       y="Coverage-Based Utility")+
  theme(axis.title = element_text(size=15),
        legend.position = "bottom")

# ggsave("flUtilPlaceholder.png", dpi=600,
#        height=5,width=7,units="in")

#-------------------------------------------------------------------------------
# Figure 4: Standardized mortality ratio style comparison 
#  number of UUs found : number UUs exected under confidence

bw_results <- read.csv("bansalWeldAugust15.csv")
mb_results <- read.csv("maurerBennetteAugust15.csv")

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
         dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv","kaggle"),
                          labels=c("pang04","pang05","mcauley15","kaggle14")) ) 

# smr_table <- smr %>%
#   mutate(value = paste0(round(smrMedian,2)," (",round(smrLower,2),",",round(smrUpper,2),")")) %>%
#   select(dataset,algo,value) %>%
#   spread(key=algo, value=value)
# smr_table
# library(xtable)
# 
# print(xtable(smr_table), include.rownames=FALSE)

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
  coord_flip() + guides(colour = guide_legend(reverse=T))

# ggsave("discoveryRatioPlaceholder.png", dpi=600,
#        height=3.4,width=4.5,units="in")





#--------------------------------------------------------------------------------------------------------------------
# 
# bw_conf <- bw_results  %>%
#   mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv"),
#                           labels=c("pang04","pang05","mcauley15")),
#          algo=ifelse(phi=="most_uncertain","Most Uncertain","Coverage-Based"),
#          algo = factor(algo, levels=c("Most Uncertain","Coverage-Based"))) %>%
#   group_by(algo,dataset,iteration) %>%
#   mutate(conf = c(cumulativeConfidence[1],diff(cumulativeConfidence)))%>%
#   as.data.frame()
# 
# ggplot() +
#   geom_density(aes(x=conf, color=algo), 
#                data=bw_conf) +
#   theme_bw() +
#   facet_grid(dataset~.)
# 
# mb_conf <- mb_results  %>%
#   mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv"),
#                           labels=c("pang04","pang05","mcauley15")),
#          algo=factor(phi,levels=c("cluster_prior","logistic","most_uncertain"),
#                      labels=c("Cluster FL","Facility Locations","Most Uncertain"))) %>%
#   group_by(algo,dataset,iteration) %>%
#   mutate(conf = c(cumulativeConfidence[1],diff(cumulativeConfidence)))%>%
#   as.data.frame() %>%
#   filter(algo == "Facility Locations")
# 
# head(mb_conf)
# head(bw_conf)
# 
# all_conf <- rbind(select(bw_conf, algo,conf,dataset,iteration), 
#                   select(mb_conf, algo,conf,dataset,iteration) )
# 

  