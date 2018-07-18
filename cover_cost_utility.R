make_dist_mat <- function(D_test){
  dist_mat <- as.matrix(dist(D_test)) 
  dist_mat / max(dist_mat)
}

cover_dist <- function(dist_mat, Q_idx){
  if(is.null(Q_idx)){
    1
  } else{
  mean(sapply(1:nrow(dist_mat), function(x_idx){
    min(dist_mat[x_idx,Q_idx])
  }))
  }
}

best_candidate <- function(Q_idx, true_misclass, phi_D_test, dist_mat, c_MX){
  if(is.null(Q_idx)){
    candidates <-1:length(c_MX)
  }else{
    candidates <-(1:length(c_MX))[-Q_idx]
  }
  
  S_idx <- Q_idx[true_misclass[Q_idx]==1]
  cover_dist_S <- ifelse(is.null(S_idx),1,cover_dist(dist_mat, S_idx))
  current_sum <- ifelse(is.null(S_idx),0,sum(c_MX[S_idx]))
  candidates[which.max(
    sapply(candidates, function(q){
      phi_D_test[q]*1/max(cover_dist_S,min(dist_mat[,q]))*(c_MX[q]+sum(c_MX[S_idx])) +
        (1-phi_D_test[q])*1/cover_dist_S*sum(c_MX[S_idx])
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
                          phi_mod_type="rf",clust_out, updateprior=FALSE, lambda=2)
    Q_idx <- c(Q_idx,best_candidate(Q_idx, true_misclass, phi_D_test, dist_mat, c_MX))
    S_idx <- Q_idx[true_misclass[Q_idx]==1]
    cover_dist_Q <- cover_dist(dist_mat, S_idx)
    utility[b] <-(1/cover_dist_Q) * sum(c_MX[S_idx])
  }
  return(data.frame(cost="c_MX",phi=phi_mod_type, utility=utility, Q_idx=Q_idx, b=1:B))
}

# set general parameters
tau=.65
sigma=.001
clust_max=5
scale=TRUE
clust_out=NULL
B=15
load(file="./experiments/pang04config.Rdata")

cover_utility_algorithm(D_test, c_MX, true_misclass, Q_idx, B=15,tau=.65, prior=rep(.5, length(D_test)), 
                                    phi_mod_type="rf",clust_out, updateprior=FALSE, lambda=2)









