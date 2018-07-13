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
B=10
sigma=.001
clust_max=5
scale=TRUE
add_cost=TRUE

results_conf <- adaptive_query_comparison(D_test, c_MX, true_misclass, 
                                              phi_mod_types = c("rf","cluster_prior"),
                                              B=B, cost_fctn =conf_cost)

# Try squaring conf as cost
power_cost <- function(c_MX, power=2){
  c_MX^power
}
results_costsq <- adaptive_query_comparison(D_test, c_MX, true_misclass, 
                                                phi_mod_types = c("rf","cluster_prior"),
                                                B=B, cost_fctn = power_cost, power=2)
# try squared conf diff from phi
power_cost_diff <- function(c_MX, phi_D_test, power=2){
  c_MX^power - phi_D_test^power
}
results_costsqdiff <- adaptive_query_comparison(D_test, c_MX, true_misclass, 
                                                    phi_mod_types = c("rf","cluster_prior"),
                                                    B=B, cost_fctn = power_cost_diff, power=9)

allresults <- rbind(results_conf,results_costsq,results_costsqdiff)
allresults
