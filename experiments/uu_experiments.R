# setwd("~/Documents/github/uuutils")

library(ggplot2)
library(dplyr)
library(tidyr)
source("bansal_weld_functions.R")

# define cost functions to test
conf_cost <- function(c_MX,...)  c_MX 
power_cost <- function(c_MX,phi_D_test, power=2) c_MX^power
power_cost_diff <- function(c_MX, phi_D_test, power=2)  c_MX^power - phi_D_test^power

# set general parameters
tau=.65
sigma=.001
clust_max=5
scale=TRUE
B=50

# load pang04 D_test, c_MX and true_missclass
load(file="./experiments/pang04config.Rdata")
all_results <- adaptive_query_comparison(phi_mod_types = c("rf","cluster_prior", "most_uncertain"),
                                         cost_fctn_vec=c("conf_cost","power_cost","power_cost_diff"),
                                         D_test, c_MX, true_misclass, Q_prime_idx = NULL,
                                         prior=1-c_MX, B=B, tau = tau, sigma=sigma,
                                         clust_max=clust_max,scale=scale)

# load pang05 D_test, c_MX and true_missclass
load(file="./experiments/pang05config.Rdata")
all_results <- adaptive_query_comparison(phi_mod_types = c("rf","cluster_prior"),
                                         cost_fctn_vec=c("conf_cost","power_cost","power_cost_diff"),
                                         D_test, c_MX, true_misclass, Q_prime_idx = NULL,
                                         prior=1-c_MX, B=B, tau = tau, sigma=sigma,
                                         clust_max=clust_max,scale=scale)

# load pang05 D_test, c_MX and true_missclass
load(file="./experiments/mcauley15config.Rdata")
all_results <- adaptive_query_comparison(phi_mod_types = c("rf","cluster_prior"),
                                         cost_fctn_vec=c("conf_cost","power_cost","power_cost_diff"),
                                         D_test, c_MX, true_misclass, Q_prime_idx = NULL,
                                         prior=1-c_MX, B=B, tau = tau, sigma=sigma,
                                         clust_max=clust_max,scale=scale)

ggplot()+
  geom_line(aes(x=b,y=utility,group=phi,
                color=phi), 
            data=all_results)+
  facet_grid(cost~.)



B=100
random_test <- NULL
for(i in 1:50){ 
  sample_idx <- sample(1:nrow(D_test),1000)
  timer <- Sys.time()
  random_test <- rbind(random_test, 
                         data.frame(iteration=rep(i,B),
                                    adaptive_query_comparison(phi_mod_types = c("cluster_prior","rf","logistic","most_uncertain"),
                                                              cost_fctn_vec=c("conf_cost","power_cost","power_cost_diff"),
                                                              D_test=D_test[sample_idx,], c_MX=c_MX[sample_idx], true_misclass=true_misclass[sample_idx],
                                                              Q_prime_idx = NULL,
                                                              prior=1-c_MX, B=B, tau = .65, sigma=.001,
                                                              clust_max=5,scale=TRUE),
                                    time=rep(Sys.time()-timer),B) )    
  print(paste("Done with iteration",i)) 
} 
# save(random_test,file="./experiments/mcauley15_100subsets")

monte_carlo_envelope <- random_test %>%
  group_by(phi,cost, b) %>%
  summarize(lower=quantile(utility,.95),
            upper=quantile(utility,.05),
            q3=quantile(utility,.75),
            q1=quantile(utility,.25),
            median = median(utility))

ggplot()+
  # geom_ribbon(aes(x=b,ymin=lower,ymax=upper, 
  #                 group=phi, fill=phi),
  #             alpha=.25,
  #             data=monte_carlo_envelope)+ 
  # geom_ribbon(aes(x=b,ymin=q1,ymax=q3, 
  #                 group=phi, fill=phi),
  #             alpha=.25,
  #             data=monte_carlo_envelope)+
  geom_line(aes(x=b,y=median, color=phi),
            data=monte_carlo_envelope)+
  facet_grid(cost~.)+
  theme_bw()
  

ggplot()+
  geom_smooth(aes(x=c_MX, y=1-true_misclass), method="loess")+
  geom_abline(intercept = 0,slope=1)

ggplot()+
  geom_histogram(aes(x=c_MX, fill=as.character(true_misclass)))


