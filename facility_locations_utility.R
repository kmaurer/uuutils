source("bansal_weld_functions.R")

make_dist_mat <- function(D_test){
  dist_mat <- as.matrix(dist(D_test)) 
  dist_mat / max(dist_mat)
}

min_dist <- function(dist_mat, Q_idx){
  if(is.null(Q_idx)){
    rep(1,nrow(dist_mat))
  } else{
    sapply(1:nrow(dist_mat), function(x_idx){
      min(dist_mat[x_idx,Q_idx])
    })
  }
}

best_candidate <- function(Q_idx, true_misclass, phi_D_test, dist_mat, c_MX, B, alpha=.5){
  if(is.null(Q_idx)){
    candidates <-1:length(c_MX)
  }else{
    candidates <-(1:length(c_MX))[-Q_idx]
  }
  # scale_reward <- 1/B
  # scale_min_dist <- 1/nrow(D_test)
  scale_reward <- alpha*sum(c_MX)/(nrow(D_test)*B)
  scale_min_dist <- (1-alpha)*mean(dist_mat)/nrow(D_test)
  
  
  S_idx <- Q_idx[true_misclass[Q_idx]==1]
  min_dist_S <- min_dist(dist_mat, S_idx)
  sum_dist_S <- sum(min_dist_S)
  current_sum <- ifelse(is.null(S_idx),0,sum(c_MX[S_idx]))
  candidates[which.max(
    sapply(candidates, function(q){
      phi_D_test[q] * (scale_reward*(c_MX[q] + current_sum) - scale_min_dist*sum(ifelse(min_dist_S < dist_mat[,q],min_dist_S, dist_mat[,q]))) +
        (1-phi_D_test[q]) * (scale_reward*current_sum  - scale_min_dist*sum_dist_S)
    })
  )]
}

fac_loc_utility_algorithm <- function(D_test, c_MX, true_misclass, Q_idx, B=10, tau=.65, prior=rep(.5, length(D_test)), 
                                    phi_mod_type="rf",clust_out, updateprior=FALSE, lambda=2, alpha=.5){
  # define
  dist_mat <- make_dist_mat(D_test)
  utility <- 0 ; reward <- 0 ; sum_min_dist <- 1
  Q_idx <- NULL
  # scale_reward <- 1/B
  # scale_min_dist <- 1/nrow(D_test)
  scale_reward <- alpha*sum(c_MX)/(nrow(D_test)*B)
  scale_min_dist <- (1-alpha)*mean(dist_mat)/nrow(D_test)
  # iterate
  for(b in 1:B){
    phi_D_test <- phi_all(D_test, c_MX, true_misclass, Q_idx, tau=.65, prior=rep(.5, length(D_test)), 
                          phi_mod_type=phi_mod_type,clust_out, updateprior=FALSE, lambda=2)
    Q_idx <- c(Q_idx,best_candidate(Q_idx, true_misclass, phi_D_test, dist_mat, c_MX, B))
    S_idx <- Q_idx[true_misclass[Q_idx]==1]
    reward[b] <- scale_reward*ifelse(is.null(S_idx), 0, sum(c_MX[S_idx]))
    sum_min_dist[b] <- scale_min_dist*sum(min_dist(dist_mat, S_idx))
    utility[b] <- reward[b] - sum_min_dist[b]
  }
  return(data.frame(cost="c_MX",phi=phi_mod_type, Q_idx=Q_idx, b=1:B,
                    utility=utility,reward=reward, sum_min_dist =sum_min_dist))
}



# set general parameters
tau=.65
sigma=.001
clust_max=5
scale=TRUE
clust_set=NULL
alpha=.9
load(file="./experiments/mcauley15config.Rdata")

set.seed(1)
samp_idx <- sample(1:nrow(D_test),1000)
D_test <- D_test[samp_idx,]
c_MX <- c_MX[samp_idx]
true_misclass <- true_misclass[samp_idx]
clust_out <- UUclust(D_test, c_MX, clust_max=5, clust_set=clust_set)
B = 150 
phis <- c("rf","logistic","cluster_prior","omniscient")
phis <- c("rf","logistic","cluster_prior","omniscient")
# run query with fac location utility using several phi's
res_list <- lapply(phis, function(phi){
  fac_loc_utility_algorithm(D_test, c_MX, true_misclass, Q_idx, B=B,tau=.65, prior=rep(.5, length(D_test)), 
                          phi_mod_type=phi,clust_out, updateprior=FALSE, lambda=2, alpha=alpha)
})
fl_util_results <- do.call(rbind,res_list)

#add most uncertain 
mu_idx <- order(c_MX)
dist_mat <- make_dist_mat(D_test)
# scale_reward <- 1/B
# scale_min_dist <- 1/nrow(D_test)
scale_reward <- alpha*sum(c_MX)/(nrow(D_test)*B)
scale_min_dist <- (1-alpha)*mean(dist_mat)/nrow(D_test)

mu_util <- sapply(1:B, function(b){
  S_idx <- mu_idx[1:b][true_misclass[mu_idx[1:b]]==1]
  reward <- scale_reward* ifelse(is.null(S_idx), 0, sum(c_MX[S_idx]))
  sum_min_dist <- scale_min_dist * mean(min_dist(dist_mat, S_idx))
  utility <- reward - sum_min_dist
  c(utility, reward, sum_min_dist)
})
mu_util_df <- as.data.frame(t(mu_util)) 
colnames(mu_util_df) = c("utility","reward","sum_min_dist")

fl_util_results <- rbind(fl_util_results ,
                           data.frame(cost="c_MX",phi="most_uncertain", Q_idx=mu_idx[1:B], b=1:B, mu_util_df))

fl_util_results


library(ggplot2)
ggplot()+
  geom_line(aes(x=b,y=utility, color=phi),
            data=fl_util_results ,size=1.5)
