### Experimental results plots from Figures 1,2,3,4
library(tidyverse)
library(RColorBrewer)

# colorblind safe palette
algo_colors <- c("#d95f02","#1b9e77","#7570b3","#e7298a")

#---------------------------------------------------------------------------------------
# Figure 1: comparing MU and BW on coverage-based utility
bw_results <- read.csv("bansalWeld.csv")
head(bw_results)
unique(bw_results$dataset)

bw_monte_carlo_envelope <- bw_results  %>%
  mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv","kaggle"),
                          labels=c("Pang04","Pang05","McAuley15","Kaggle13")) ) %>%
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
#        height=6,width=8.4,units="in")

#---------------------------------------------------------------------
# Figure 2: overconfidence profile plot

datasetvec <- c("pang04","pang05","mcauley15","kaggle13")
all_overconfidence <- NULL
for (dataset in datasetvec){
  dat <- read.csv(paste0("C:/Users/maurerkt/Documents/GitHub/uuutils/writeups/AAAI/experimentsAndPlots/inputFiles/",dataset,"_predictionResults.csv"))
  dat <- dplyr::filter(dat, Confidence > .65, Prediction==1)
  if(dataset=="images") dat$Misclassified <- ifelse(dat$Misclassified=="True",1,0)
  all_overconfidence <- rbind(all_overconfidence,
                              data.frame(c_MX =dat$Confidence,
                                         misclass = dat$Misclassified,
                                         data_source = dataset)) 
}

bin_widths = .05
bin_breaks = seq(.65,1,by=bin_widths)
bin_centers = seq(.65+bin_widths/2,1-bin_widths/2,by=bin_widths)

overconf_bins <- all_overconfidence %>%
  mutate(data_source = factor(all_overconfidence$data_source,labels=c("Pang04","Pang05","McAuley15","Kaggle13")),
         conf_bin = cut(all_overconfidence$c_MX,breaks=bin_breaks)) %>%
  group_by(data_source,conf_bin) %>%
  summarize(count=n(),
            observed= sum(misclass==0)/count)%>%
  mutate(expected = bin_centers[as.numeric(conf_bin)],
         xmin=expected-bin_widths/2,
         xmax=expected+bin_widths/2) %>%
  gather(key="type",value="accuracy",observed:expected) %>%
  mutate(type = factor(type, levels=c("expected","observed"),
                       labels=c("Overconfidence","Observed Accuracy")))%>%
  arrange(type)

ggplot()+
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=accuracy, color=type,fill=type), data=overconf_bins) +
  annotate(geom="segment", x=.65,y=.65,xend=1,yend=1, size=1, color="gray30",linetype=2) +
  facet_grid(~data_source)+
  scale_x_continuous(breaks=c(.6,.7,.8,.9,1))+
  scale_y_continuous(breaks=seq(0,1,by=.2))+
  scale_fill_manual("",values=c("#f77c01","#81d1f9"))+
  scale_color_manual("",values=c("black","black"))+
  labs(x="Model Confidence", y="Accuracy")+
  theme_bw()+
  theme(legend.position = "bottom")+
  coord_fixed()
# ggsave("overconfidence_2.png",dpi=600,width=4.1,height=3.8,units="in")

#---------------------------------------------------------------------------------------
# Figure 3: comparing MU and FL searches on FL utility
mb_results <- read.csv("facilityLocation.csv")
head(mb_results)
unique(mb_results$dataset)
unique(mb_results$phi)

monte_carlo_envelope <- mb_results  %>%
  mutate(dataset = factor(dataset, levels=c("pang04Out.csv","pang05Out.csv","mcauley15Out.csv","kaggle"),
                          labels=c("Pang04","Pang05","McAuley15","Kaggle13")) ) %>%
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
       y="Facility Locations Utility")+
  theme(axis.title = element_text(size=15),
        legend.position = "bottom")

# ggsave("flUtilPlaceholder.png", dpi=600,
#        height=6,width=8.4,units="in")

#-------------------------------------------------------------------------------
# Figure 4: Standardized mortality ratio style comparison 
#  number of UUs found : number UUs exected under confidence
lak_results <- read.csv("lakkaraju.csv")

# create standardized discovery ratio summaries for each method
bw_sdr <- bw_results %>%
  filter(b==100) %>%
  mutate(algo = factor(phi,levels=c("cluster_prior","most_uncertain"),
                     labels=c("Coverage-Based","Most Uncertain")),
         sdr = found/(B-cumulativeConfidence)) %>%
  group_by(dataset, algo) %>%
  summarize(sdrLower = quantile(sdr,.05),
            sdrMedian = quantile(sdr,.5),
            sdrUpper = quantile(sdr,.95))

mb_sdr <- mb_results %>%
  filter(b==100) %>%
  mutate(algo = factor(phi,levels=c("cluster_prior","logistic","most_uncertain"),
                       labels=c("Cluster FL","Facility Locations","Most Uncertain")),
         sdr = found/(B-cumulativeConfidence)) %>%
  group_by(dataset, algo) %>%
  summarize(sdrLower = quantile(sdr,.05),
            sdrMedian = quantile(sdr,.5),
            sdrUpper = quantile(sdr,.95)) 

lak_sdr <- lak_results %>%
  filter(b==100) %>%
  mutate(algo = factor(phi,levels=c("Bandits"),
                       labels=c("Bandit")),
         sdr = found/(B-cumulativeConfidence),
         dataset = ifelse(dataset=="kaggle13Out.csv","kaggle",as.character(dataset))) %>%
  group_by(dataset, algo) %>%
  summarize(sdrLower = quantile(sdr,.05),
            sdrMedian = quantile(sdr,.5),
            sdrUpper = quantile(sdr,.95)) 

# combine all
sdr <- rbind(bw_sdr, filter(mb_sdr, algo != "Most Uncertain"),lak_sdr) %>%
  filter(algo %in% c("Coverage-Based","Facility Locations","Most Uncertain","Bandit")) %>%
  ungroup() %>%
  mutate(algo = factor(algo, levels=c("Most Uncertain","Bandit","Coverage-Based","Facility Locations")),
         dataset = factor(dataset, levels=c("kaggle","mcauley15Out.csv","pang05Out.csv","pang04Out.csv"),
                          labels=c("Kaggle13","McAuley15","Pang05","Pang04")) ) %>%
  arrange(dataset, algo)

sdr %>% 
  group_by(dataset) %>%
  mutate(vs_fl = sdrMedian[algo=="Facility Locations"]/sdrMedian)

#plot
ggplot() +
  geom_errorbar(aes(ymin=sdrLower,ymax=sdrUpper,
                    color=algo, x=dataset),width=.5,
                position = "dodge", data=sdr, size=1) +
  geom_point(aes(y=sdrMedian, color=algo, x=dataset, shape=algo),
                position=position_dodge(width=.5), data=sdr, size=3) +
  scale_color_manual("Algorithm:", values=algo_colors[c(1,4,2,3)]) +
  scale_shape_manual("Algorithm:", values=15:18) +
  # scale_linetype_manual("Algorithm:", values=1:3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=seq(0,10,2), 
                     limits=c(0,max(sdr$sdrUpper))) +
  labs(y="Standardized Discovery Ratio",
       x="") +
  theme_bw() +
  theme(legend.position = c(.9999,.9999),legend.justification = c(1,1),
        axis.text.y= element_text(angle=90, hjust=.5),
        legend.background =  element_rect(color="black"))+
  coord_flip() + guides(colour = guide_legend(reverse=T),
                        shape = guide_legend(reverse=T),
                        linetype = guide_legend(reverse=T))

# ggsave("discoveryRatioPlaceholder.png", dpi=600,
#        height=4,width=4,units="in")