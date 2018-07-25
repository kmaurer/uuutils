library(ggplot2)
library(dplyr)

setwd("~/Downloads")
load("~/Downloads/results_mcauley15.Rdata")

head(random_test)

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
  # geom_ribbon(aes(x=b,ymin=q1,ymax=q3,
  #                 group=phi, fill=phi),
  #             alpha=.25,
  #             data=monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=algo),
            data=monte_carlo_envelope,size=1.5)+
  # facet_grid(.~phi, scales="free_y")+
  scale_color_manual("Algorithm:", values=c("black","red"))+
  scale_fill_manual("Algorithm:", values=c("black","red"))+
  theme_bw()+
  labs(x="Query Step (b)",
       y="Coverage-Based Utility \n(Proportion Relative to Upper Bound)")+
  theme(axis.title = element_text(size=15),
        legend.position = "bottom")

#-------------------------------------------------------------------------------
# showing disparity between where confidence is wrong

load(file="./experiments/mcauley15config.Rdata")

c_MX[c_MX == 1] <- .9999999

spline_degree=3
wherewrong <- ggplot()+
  geom_point(aes(x=c_MX,y=1-true_misclass), alpha=.02)+
  geom_smooth(aes(x=c_MX,y=1-true_misclass), method = lm, formula = y ~ splines::bs(x, spline_degree))+
  geom_abline(intercept = 0,slope=1, color="red") +
  theme_bw()+
  coord_fixed()+
  annotate(geom="text",x=.9, y=.9,color="red", label="Confidence Predicted",
           angle=45, vjust=-.2)+  
  annotate(geom="text",x=.9, y=.82,color="blue", label="Observed Rate",
           angle=25, vjust=-.2)+
  labs(x=expression("c"[Mx]), y="Rate of Correct Classification")
wherewrong 



overconfidence_plot <- ggplot()+
  geom_line(aes(x=c_MX,y=overconfidence),color="red",size=1.5)+
  geom_hline(yintercept = 0)+
  theme_bw()+
  coord_fixed()+
  labs(x=expression("c"[Mx]), y="Overconfidence")+
  annotate(geom="text",x=.95, y=.025,color="red", label="more mistakes \nthan advertised",
           vjust=.5)
overconfidence_plot



bin_width =.025
bindata <- data.frame(c_MX, true_misclass, 
           bin=factor(cut(c_MX,seq(.65,1,by=bin_width)),labels=seq(.65+.5*bin_width,1-.5*bin_width,by=bin_width))) %>%
  group_by(bin) %>%
  summarize(prop_correct = 1-mean(true_misclass))
bindata

ggplot() + 
  geom_bar(aes(x=as.numeric(as.character(bin)),y=prop_correct),stat="identity",
           width=bin_width, data=bindata, color="black",fill="pink")+
  geom_abline(intercept = 0, slope=1, color="blue",size=1)+
  annotate(geom="text",x=.825, y=.825,color="blue", label="Expected Accuracy",
           angle=10, vjust=-1.1, size=5)+
  scale_y_continuous(limits=c(0,1))+
  # coord_fixed()+
  theme_bw() +
  labs(x=expression("c"[Mx]), y="Observed Rate of Correct Classification") +
  theme(axis.title = element_text(size=15))


ggplot()+
  geom_histogram(aes(x=c_MX,fill=as.character(true_misclass)))+
  theme_bw()

ggplot()+
  geom_smooth(aes(x=c_MX, y=1-true_misclass), method="loess", stat="identity")+
  geom_abline(intercept = 0,slope=1)

ggplot()+
  geom_histogram(aes(x=c_MX, fill=as.character(true_misclass)))


ggplot()+
  geom_line(aes(x=b,y=utility,group=phi,
                color=phi),
            data=all_results, size=1.5)+
  facet_grid(cost~.,scales="free_y")+
  theme_bw()



# monte_carlo_envelope <- random_test %>%
#   group_by(phi,cost, b) %>%
#   summarize(lower=quantile(utility,.95),
#             upper=quantile(utility,.05),
#             q3=quantile(utility,.75),
#             q1=quantile(utility,.25),
#             median = median(utility))
# 
# ggplot()+
#   # geom_ribbon(aes(x=b,ymin=lower,ymax=upper, 
#   #                 group=phi, fill=phi),
#   #             alpha=.25,
#   #             data=monte_carlo_envelope)+ 
#   # geom_ribbon(aes(x=b,ymin=q1,ymax=q3, 
#   #                 group=phi, fill=phi),
#   #             alpha=.25,
#   #             data=monte_carlo_envelope)+
#   geom_line(aes(x=b,y=median, color=phi),
#             data=monte_carlo_envelope)+
#   facet_grid(cost~.)+
#   theme_bw()
#   
# 
# ggplot()+
#   geom_smooth(aes(x=c_MX, y=1-true_misclass), method="loess")+
#   geom_abline(intercept = 0,slope=1)
# 
# ggplot()+
#   geom_histogram(aes(x=c_MX, fill=as.character(true_misclass)))
# 
# 
# ggplot()+
#   geom_line(aes(x=b,y=utility,group=phi,
#                 color=phi),
#             data=all_results, size=1.5)+
#   facet_grid(cost~.,scales="free_y")+
#   theme_bw()

