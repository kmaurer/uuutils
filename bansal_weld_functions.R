### Create modular functions for recreating and extending Bansal and Weld coverage-based utilities 

#---------------------------------------------------
### function to create nXn matrix of similarities scores between all pairs in test data
make_sim_mat <- function(D_test, sigma=.001, scale=TRUE){
  dist_test <- as.matrix(dist(D_test)) 
  if(scale) dist_test <- dist_test / max(dist_test)
  exp(-dist_test^2/sigma)
}

#---------------------------------------------------
### P(explain D_test | Q, y_Q) function
# sim_mat is nXn matrix of similarities between all pairs in test data
# Q_idx index of oracle queried test observations from D_test
# ...
P_explainx_Q <- function(sim_mat, Q_idx, true_misclass, c_MX, tau, sigma=.1,...){
  D_test_idx <- 1:nrow(sim_mat)
  # pick out indeces for identified UUs
  Q_ind_uu <- Q_idx[which(true_misclass[Q_idx]==1 & c_MX[Q_idx]>tau)]
  if(length(Q_ind_uu)==0){
    tmp <- rep(0,nrow(sim_mat))
  } else {
    tmp <- sapply( D_test_idx, function(x_idx) max(sim_mat[x_idx , Q_ind_uu]) )
  }
  return(tmp)
}

#---------------------------------------
# function to look up unqueried indeces 
make_Qc_index <- function(D_test_idx, Q_idx){
  D_test_idx[!D_test_idx %in% Q_idx]
}
# make_Qc_index(1:10,c(2,4,6,8))

#---------------------------------------
# define default cost function (matchs bansal and weld weights)
conf_cost <- function(c_MX,...) I(c_MX)

#---------------------------------------
# Expected changes in utility function for adding observations in set of possible UUs (S)
exp_utility_step_all <- function(Sc_idx, phi_D_test, sim_mat, c_MX, P_expx_Q,
                                 cost_fctn="conf_cost", ...){
  sims_Dtest_Sc <- sim_mat[,Sc_idx]
  # for each candidate evaluate change in utility assuming it is UU, then multiply by phi(x)
  util_s <- sapply(1:length(Sc_idx), function(s){
    P_expx_Qands <- ifelse(sims_Dtest_Sc[,s] > P_expx_Q , sims_Dtest_Sc[,s], P_expx_Q)
    cost_vals <- get(cost_fctn)(c_MX,phi_D_test)
    t(cost_vals) %*% P_expx_Qands
  })
  as.vector(phi_D_test[Sc_idx] * util_s)
}

#---------------------------------------------------
# Adaptive Greedy algorithm for selecting new oracle queries:
# inputs: test set, confidence values, true prediction errors, initial oracle queries and a budget
# output: index of observations added to oracle query
adap_greedy <- function(D_test, c_MX, true_misclass, Q_prime_idx, B, tau=.65,
                        phi_mod_type="rf",clust_max=5,clust_set=NULL, prior=rep(.5,nrow(D_test)),
                        sigma=.001, scale=TRUE, updateprior=FALSE, lambda=2,
                        cost_fctn="conf_cost", ...){ 
  # Set up index partion of observations that are in/out of oracle query set
  D_test_idx <- 1:nrow(D_test)
  if(phi_mod_type=="most_uncertain") Q_prime_idx <- order(c_MX)[1:B]
  Q_idx <- Q_prime_idx
  Qc_idx <- make_Qc_index(D_test_idx,Q_idx)
  # initalize utility storage, and make Q and Q-compliment set indeces
  utility <- rep(NA,B)
  # calculate distance matrix for repeated use in P(explain_x | Q)
  sim_mat <- make_sim_mat(D_test=D_test, sigma, scale)
  # generate clusters #!# this could be replaced with sampling algorithm to initialize our model based method
  clust_out <- UUclust(D_test, c_MX, clust_max=5, clust_set=clust_set)
  # initialize phi function with priors 
  phi_D_test <- phi_all(D_test, c_MX, true_misclass, Q_idx, tau, prior, phi_mod_type,
                        clust_out, updateprior, lambda)
  # calculate and store utility gain in query priming set
  if(length(Q_prime_idx) > 0){
    for (b in 1:length(Q_prime_idx)){
      cost_vals <- get(cost_fctn)(c_MX,phi_D_test)
      utility[b] <- t(cost_vals) %*% P_explainx_Q(sim_mat, Q_prime_idx[1:b], true_misclass, c_MX, tau)
    }
  }
  # For each step until we reach budget B:
  P_expx_Q <- P_explainx_Q(sim_mat, Q_idx, true_misclass, c_MX, tau)
  if(length(Q_prime_idx) < B){
    for (b in (length(Q_prime_idx)+1):B){
      # find optimal new q observations
      Sc_idx <- Qc_idx[c_MX[Qc_idx]>tau] #candidates must be of high enough confidence to possibly be UU
      q_new_idx <- Sc_idx[which.max(exp_utility_step_all(Sc_idx, phi_D_test, sim_mat, c_MX, P_expx_Q, cost_fctn))]
      # updated query set
      Q_idx <- c(Q_idx,q_new_idx)
      Qc_idx <- make_Qc_index(D_test_idx,Q_idx)
      # update utility
      P_expx_Q <- P_explainx_Q(sim_mat, Q_idx, true_misclass, c_MX, tau)
      utility[b] <- t(get(cost_fctn)(c_MX, phi_D_test)) %*% P_expx_Q
      # update phi 
      phi_D_test <- phi_all(D_test, c_MX, true_misclass, Q_idx, tau, prior, phi_mod_type,
                            clust_out,updateprior, lambda)
    }
  }
  print(paste0("phi:",phi_mod_type," with cost:",cost_fctn," Complete"))
  return(data.frame(cost=cost_fctn,
                    phi=phi_mod_type, utility=utility, Q_idx=Q_idx, b=1:B))
}

#---------------------------------------------------
### bundle into function to produce all result sets of interest
adaptive_query_comparison <- function(phi_mod_types = c("most_uncertain","omniscient"),
                                      cost_fctn_vec=c("conf_cost"),
                                      D_test, c_MX, true_misclass, Q_prime_idx = NULL,
                                      prior=1-c_MX, B=20, tau = .65, sigma=.001,
                                      clust_max=5,scale=TRUE, ...){
  # apply greedy to all in list
  cost_results_list <-lapply(1:length(cost_fctn_vec), function(cost_fctn_idx){
    phi_results <- lapply(phi_mod_types, function(phi_mod_type){
        adap_greedy(D_test=D_test, c_MX=c_MX, true_misclass=true_misclass,
                    Q_prime_idx=Q_prime_idx, B=B, tau=tau,
                    phi_mod_type=phi_mod_type,clust_max=clust_max,clust_set=NULL, 
                    prior=prior,sigma=sigma, scale=scale, updateprior=FALSE,lambda=2,
                    cost_fctn=cost_fctn_vec[[cost_fctn_idx]])
    })
    do.call(rbind, phi_results)
  })
  do.call(rbind, cost_results_list)
} 
# # Test Case components
# D_test = as.matrix(pang04D_test[test_subset,])
# c_MX = confidence[test_subset]
# prior = 1-c_MX
# true_misclass = misclass[test_subset]
# clust_set=NULL
# B=50
# tau=.65
# phi_mod_type = "omniscient"
# dist_test = as.matrix(dist(D_test))
# Q_idx = 1:20
# sigma=.1
# scale=T
# timer <- Sys.time()