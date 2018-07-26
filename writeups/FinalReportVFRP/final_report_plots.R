library(ggplot2)
library(dplyr)
library(gridExtra)

setwd("~/GitHub/uuutils/writeups/FinalReportVFRP")

#-----------------------------------------------------
# Demonstrate how coverage-based utility model favors most uncertain
load("~/GitHub/uuutils/experiments/results_mcauley15.Rdata")

monte_carlo_envelope <- random_test  %>%
  group_by(cost, b,iteration) %>%
  mutate(rel_util = utility/max(utility)) %>%
  filter(cost == "conf_cost" &
           phi %in% c("most_uncertain","cluster_prior"))%>%
  ungroup() %>%
  group_by(phi,cost, b) %>%
  summarize(lower=quantile(rel_util,.95),
            upper=quantile(rel_util,.05),
            q3=quantile(rel_util,.75),
            q1=quantile(rel_util,.25),
            median = median(rel_util))%>%
  filter(phi!="omniscient")%>%
  mutate(algo=ifelse(phi=="most_uncertain","Most Uncertain","Adaptive-Greedy"))

ggplot()+
  geom_ribbon(aes(x=b,ymin=lower,ymax=upper,
  group=phi, color=algo,fill=algo),linetype=2,
              alpha=.04,
              data=monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=algo),
            data=monte_carlo_envelope,size=1.5)+
  scale_color_manual("Algorithm:", values=c("red","black"))+
  scale_fill_manual("Algorithm:", values=c("red","black"))+
  theme_bw()+
  labs(x="Query Step (b)",
       y="Coverage-Based Utility \n(Proportion Relative to Upper Bound)")+
  theme(axis.title = element_text(size=15),
        legend.position = "bottom")

#-------------------------------------------------------------------------------
# Showing where model is overconfident

load(file="~/GitHub/uuutils/experiments/mcauley15config.Rdata")
c_MX[c_MX == 1] <- .9999999

# create cubic splines model to fit rate of correct class as function of confidence
mod <- lm(1-true_misclass ~ splines::bs(c_MX, 3))
overconfidence <- c_MX - predict(mod, data.frame(c_MX=c_MX))

overconfidence_plot <- ggplot()+
  geom_line(aes(x=c_MX,y=overconfidence),color="red",size=1.5)+
  geom_hline(yintercept = 0)+
  theme_bw()+
  coord_fixed()+
  labs(x=expression("c"[Mx]), y="Overconfidence")+
  annotate(geom="text",x=.95, y=.025,color="red", label="more mistakes \nthan advertised",
           vjust=.5)
overconfidence_plot

### show observed rate of correct class as function of confidence
# spline_degree=3
# wherewrong <- ggplot()+
#   geom_point(aes(x=c_MX,y=1-true_misclass), alpha=.02)+
#   geom_smooth(aes(x=c_MX,y=1-true_misclass), method = lm, formula = y ~ splines::bs(x, spline_degree))+
#   geom_abline(intercept = 0,slope=1, color="red") +
#   theme_bw()+
#   coord_fixed()+
#   annotate(geom="text",x=.9, y=.9,color="red", label="Confidence Predicted",
#            angle=45, vjust=-.2)+  
#   annotate(geom="text",x=.9, y=.82,color="blue", label="Observed Rate",
#            angle=25, vjust=-.2)+
#   labs(x=expression("c"[Mx]), y="Rate of Correct Classification")
# wherewrong 

### Binned version of overconfidence plot
# bin_width =.025
# bindata <- data.frame(c_MX, true_misclass, 
#            bin=factor(cut(c_MX,seq(.65,1,by=bin_width)),labels=seq(.65+.5*bin_width,1-.5*bin_width,by=bin_width))) %>%
#   group_by(bin) %>%
#   summarize(prop_correct = 1-mean(true_misclass))
# bindata
# 
# ggplot() + 
#   geom_bar(aes(x=as.numeric(as.character(bin)),y=prop_correct),stat="identity",
#            width=bin_width, data=bindata, color="black",fill="pink")+
#   geom_abline(intercept = 0, slope=1, color="blue",size=1)+
#   annotate(geom="text",x=.825, y=.825,color="blue", label="Expected Accuracy",
#            angle=10, vjust=-1.1, size=5)+
#   scale_y_continuous(limits=c(0,1))+
#   # coord_fixed()+
#   theme_bw() +
#   labs(x=expression("c"[Mx]), y="Observed Rate of Correct Classification") +
#   theme(axis.title = element_text(size=15))

#=====================================================================================
# combine all data tests results

datasetvec <- c("pang04","pang05","mcauley15","kaggle14")
all_random_test <- NULL
for (dataset in datasetvec){
  load(file=paste0("~/GitHub/uuutils/experiments/results_fl_",dataset,".Rdata"))
  all_random_test <- rbind(all_random_test,
                           data.frame(random_test,data_source=dataset))
}
head(all_random_test)
tail(all_random_test)

monte_carlo_envelope <- all_random_test  %>%
  group_by(data_source, cost, b,iteration) %>%
  mutate(rel_util = (utility-min(utility))/(max(utility)-min(utility))) %>%
  filter(phi != "omniscient", phi != "rf")%>%
  ungroup() %>%
  group_by(data_source, phi,cost, b) %>%
  summarize(lower=quantile(rel_util,.95),
            upper=quantile(rel_util,.05),
            q3=quantile(rel_util,.75),
            q1=quantile(rel_util,.25),
            median = median(rel_util))%>%
  filter(phi!="omniscient")%>%
  ungroup() %>%
  mutate(algo=factor(phi, labels=c("Logistic~~phi(x)","Cluster~~phi(x)",
                                   "Most~~Uncertain")))


all_envelopes <- ggplot()+
  geom_ribbon(aes(x=b,ymin=lower,ymax=upper,
                  group=phi, color=algo,fill=algo),linetype=2,
              alpha=.04,
              data=monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=algo),
            data=monte_carlo_envelope,size=1.5)+
  facet_grid(data_source~., scales="free_y")+
  scale_color_manual("Algorithm:", values=c("blue","red","black"), labels = scales::parse_format())+
  scale_fill_manual("Algorithm:", values=c("blue","red","black"), labels = scales::parse_format())+
  theme_bw()+
  theme(plot.margin = unit(c(.2,.2,.2,.5), "cm"))+
  labs(x="Query Step (b)",
       y="Facility Location Utility \n(Proportion Relative to Upper Bound)")

all_envelopes

# create cubic splines model to fit rate of correct class as function of confidence
datasetvec <- c("pang04","pang05","mcauley15","kaggle14")
all_overconfidence <- NULL
for (dataset in datasetvec){
  load(file=paste0("~/GitHub/uuutils/experiments/",dataset,"config.Rdata"))
  mod <- lm(1-true_misclass ~ splines::bs(c_MX, 3))
  all_overconfidence <- rbind(all_overconfidence,
                              data.frame(c_MX =c_MX,
                                         overconfidence = c_MX - predict(mod, data.frame(c_MX=c_MX)),
                                         data_source = dataset)) 
}
head(all_overconfidence)
tail(all_overconfidence)

labels_data <- data.frame(x=c(.65,.73,.99,.93),y=c(.05,.08,.022,.15),
                          data_source=datasetvec,
                          hjust=c(0,.5,1,.6),
                          label=c("worst \noverconfidence"))

all_overconfidence_plots <- ggplot()+
  geom_line(aes(x=c_MX,y=overconfidence),color="darkorange",
            size=1.5, data=all_overconfidence)+
  geom_hline(yintercept = 0)+
  geom_text(aes(x=x,y=y,label=label,hjust=hjust), data=labels_data,
            color="darkorange")+
  facet_grid(data_source~., scales="free_y")+
  theme_bw()+
  labs(x=expression("c"[Mx]), y="Overconfidence")+
  theme(plot.margin = unit(c(.2,.5,.2,.2), "cm"))
all_overconfidence_plots


allplots <- grid.arrange(all_overconfidence_plots,all_envelopes,
                         nrow=1,widths=c(.42,.58))
ggsave(filename="allplotsFL.png", plot=allplots,
       width=11, height=11, units="in",
       dpi=600)
