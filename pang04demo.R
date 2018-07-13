library(ggplot2)
library(dplyr)
library(tidyr)

setwd("~/Documents/karsten/poster")
load(file="pang04gathered.Rdata")

# D_test should only consist of high confidence predictions in critical class
tau=.65
test_subset <- which(confidence > tau & pang04preds$V1==1)

D_test = as.matrix(pang04D_test[test_subset,])
c_MX = confidence[test_subset]
true_misclass = misclass[test_subset]
Q_prime_idx = NULL
phi_mod_type = "rf"
prior=1-confidence[test_subset]
B=5
sigma=.001
clust_max=5
scale=TRUE
add_cost=TRUE

conf_cost <- function(c_MX,...)  c_MX 
power_cost <- function(c_MX, power=2) c_MX^power
power_cost_diff <- function(c_MX, phi_D_test, power=2)  c_MX^power - phi_D_test^power

all_results <- adaptive_query_comparison(phi_mod_types = c("rf","cluster_prior"),
                                          cost_fctn_list=list(conf_cost,power_cost,power_cost_diff),
                                          D_test, c_MX, true_misclass, Q_prime_idx = NULL,
                                          prior=1-c_MX, B=B, tau = .65, sigma=.001,
                                          clust_max=5,scale=TRUE, cost_fctn=conf_cost)
