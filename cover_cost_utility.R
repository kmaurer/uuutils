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

sum_sim <- function(x,sigma=.001){
  sum(exp(-x^2/sigma))
}

# best_candidate <- function(Q_idx, true_misclass, phi_D_test, dist_mat, c_MX){
#   if(is.null(Q_idx)){
#     candidates <-1:length(c_MX)
#   }else{
#     candidates <-(1:length(c_MX))[-Q_idx]
#   }
#   
#   S_idx <- Q_idx[true_misclass[Q_idx]==1]
#   cover_dist_S <- ifelse(is.null(S_idx),1,cover_dist(dist_mat, S_idx))
#   current_sum <- ifelse(is.null(S_idx),0,sum(c_MX[S_idx]))
#   candidates[which.max(
#     sapply(candidates, function(q){
#       phi_D_test[q]*(c_MX[q]+current_sum)/max(cover_dist_S,min(dist_mat[,q])) +
#         (1-phi_D_test[q])*current_sum/cover_dist_S
#     })
#   )]
# }

best_candidate <- function(Q_idx, true_misclass, phi_D_test, dist_mat, c_MX){
  if(is.null(Q_idx)){
    candidates <-1:length(c_MX)
  }else{
    candidates <-(1:length(c_MX))[-Q_idx]
  }
  b = length(Q_idx)
  S_idx <- Q_idx[true_misclass[Q_idx]==1]
  min_dist_S <- min_dist(dist_mat, S_idx)
  avg_min_dist_S <- sum_sim(min_dist_S)
  current_sum <- ifelse(is.null(S_idx),0,sum(c_MX[S_idx]))
  candidates[which.max(
    sapply(candidates, function(q){
      phi_D_test[q] * (c_MX[q] + current_sum + sum_sim(ifelse(min_dist_S < dist_mat[,q],min_dist_S, dist_mat[,q]))) +
        (1-phi_D_test[q]) * (current_sum + avg_min_dist_S)
    })
  )]
}

cover_utility_algorithm <- function(D_test, c_MX, true_misclass, Q_idx, B=10, tau=.65, prior=rep(.5, length(D_test)), 
                                    phi_mod_type="rf",clust_out, updateprior=FALSE, lambda=2){
  # define
  dist_mat <- make_dist_mat(D_test)
  utility <- 0
  Q_idx <- NULL
  # iterate
  for(b in 1:B){
    phi_D_test <- phi_all(D_test, c_MX, true_misclass, Q_idx, tau=.65, prior=rep(.5, length(D_test)), 
                          phi_mod_type=phi_mod_type,clust_out, updateprior=FALSE, lambda=2)
    Q_idx <- c(Q_idx,best_candidate(Q_idx, true_misclass, phi_D_test, dist_mat, c_MX))
    S_idx <- Q_idx[true_misclass[Q_idx]==1]
    utility[b] <- sum(c_MX[S_idx]) + sum_sim(min_dist(dist_mat, S_idx))
  }
  return(data.frame(cost="c_MX",phi=phi_mod_type, utility=utility, Q_idx=Q_idx, b=1:B))
}



# set general parameters
tau=.65
sigma=.001
clust_max=5
scale=TRUE
clust_set=NULL
load(file="./experiments/pang04config.Rdata")
clust_out <- UUclust(D_test, c_MX, clust_max=5, clust_set=clust_set)
B=500

res_list <- lapply(c("rf","logistic","cluster_prior","omniscient"), function(phi){
  cover_utility_algorithm(D_test, c_MX, true_misclass, Q_idx, B=B,tau=.65, prior=rep(.5, length(D_test)), 
                                               phi_mod_type=phi,clust_out, updateprior=FALSE, lambda=2)
})
simp_util_results <- do.call(rbind,res_list)

mu_idx <- order(c_MX)
dist_mat <- make_dist_mat(D_test)
utility <- sapply(1:B, function(b){
  S_idx <- mu_idx[1:b][true_misclass[mu_idx[1:b]]==1]
  sum(c_MX[S_idx]) + sum_sim(min_dist(dist_mat, S_idx))
})
simp_util_results <- rbind(simp_util_results ,
      data.frame(cost="c_MX",phi="most_uncertain", utility=utility, Q_idx=mu_idx[1:B], b=1:B))

library(ggplot2)
ggplot()+
  geom_line(aes(x=b,y=utility, color=phi),
            data=simp_util_results ,size=1.5)








