### Create modular function for recreating and extending Bansal and Weld work

make_sim_mat <- function(D_test, sigma=.1, scale=TRUE){
  dist_test <- as.matrix(dist(D_test)) 
  if(scale) dist_test <- dist_test / max(dist_test)
  exp(-dist_test^2/sigma)
}

### P(explain D_test | Q, y_Q) function
# dist_test is nXn distance matrix for numeric features of test data
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
exp_utility_step_all <- function(Sc_idx, phi_D_test, sim_mat, c_MX, P_expx_Q, cost_fctn=conf_cost, ...){
  sims_Dtest_Sc <- sim_mat[,Sc_idx]
  # for each candidate evaluate change in utility assuming it is UU, then multiply by phi(x)
  util_s <- sapply(1:length(Sc_idx), function(s){
    P_expx_Qands <- ifelse(sims_Dtest_Sc[,s] > P_expx_Q , sims_Dtest_Sc[,s], P_expx_Q)
    t(cost_fctn(c_MX,phi_D_test,...)) %*% P_expx_Qands
  })
  as.vector(phi_D_test[Sc_idx] * util_s)
}

#---------------------------------------
### cluster elbow function
elbow <- function(X, clust_max=5){
  Ks <- 2:clust_max
  perc_within <- sapply(Ks, function(nclust){
    km_out <- kmeans(X,centers=nclust)
    km_out$tot.withinss / km_out$totss
  })
  diffs <- diff(perc_within)
  Ks[which.max(diffs[1:(length(diffs)-1)]/diffs[2:length(diffs)])]+1
}

### clustering to run other UU algorithms (and potentially seed our algorithm)
UUclust <- function(D_test, c_MX, clust_max=5, clust_set=NULL){
  if(is.numeric(clust_set)){
    kmeans(cbind(D_test,c_MX),centers=clust_set)
  } else {
    conf_clusts <- kmeans(c_MX,centers=elbow(c_MX, clust_max))
    clust_out <- rep(0,length(c_MX))
    for(clust in 1:length(conf_clusts$centers)){
      in_clust <- conf_clusts$cluster==clust
      clust_out[in_clust] <- max(clust_out)+
                              kmeans(D_test[in_clust,], centers=elbow(D_test[in_clust], clust_max))$cluster
    }
    
    # conf_clusts <- kmeans(c_MX,centers=4)
    # clust_out <- rep(0,length(c_MX))
    # for(clust in 1:length(conf_clusts$centers)){
    #   in_clust <- conf_clusts$cluster==clust
    #   clust_out[in_clust] <- max(clust_out)+
    #     kmeans(D_test[in_clust,], centers=3)$cluster
    # }
    clust_out
  }
}
# D_test <- as.matrix(iris[,1:4])
# c_MX <- runif(nrow(D_test),0,1)
# clust_out <- UUclust(D_test, c_MX, clust_max=5)
# length(unique(clust_out))
#------------------------------------------


### clust_phi_update 
clust_phi <- function(true_misclass, D_test, Q_idx, clust_out, c_MX, 
                      prior=rep(.5, length(D_test)), lambda=2, tau=.65){
  UU_ind <- (c_MX > tau & true_misclass==1)[Q_idx]
  nclust <- length(unique(clust_out))
  clust_UU <- rep(NA,nclust) 
  clust_size <- rep(NA,nclust)
  for (clust in 1:nclust){
    clust_UU[clust] <- sum(UU_ind & clust_out[Q_idx]==clust)
    clust_size[clust] <-  sum(clust_out[Q_idx]==clust)
  }
  sapply(1:nrow(D_test), function(x_idx){ 
    (lambda*prior[x_idx] + clust_UU[clust_out[x_idx]]) / (lambda+clust_size[clust_out[x_idx]])
  })
}
# D_test <- as.matrix(iris[,1:4])
# c_MX <- runif(nrow(D_test),0,1)
# true_misclass <- sample(0:1,nrow(D_test),replace=T)
# Q_idx <- sample(1:nrow(D_test),20)
# clust_out <- UUclust(D_test, c_MX, clust_max=20)
# clust_phi(misclass=true_misclass, D_test, Q_idx, clust_out,
#           c_MX, prior_type="unif", lambda=2, tau=.65)

#---------------------------------------
### model-based phi function  P[M(X)!=y_X | Q]
# misclass is a boolean vector indicating misclassifications for observations in Q
# Q is bXp matrix of features for corresponding observations in oracle query set
# X is nXp of matrix of features

model_phi <- function(true_misclass, D_test, Q_idx, c_MX, mod_type = "rf", 
                      prior=rep(.5, length(D_test)), updateprior=FALSE, lambda=10, tau=.65){
  Q_dat <- data.frame(UU=as.factor(as.numeric(true_misclass==1 & c_MX > tau))[Q_idx],
                      D_test[Q_idx,],
                      c_MX=c_MX[Q_idx])
  if( mod_type == "rf"){
    phi_mod <- randomForest::randomForest(UU ~ . , data=Q_dat)
    phi_val <- predict(phi_mod,data.frame(D_test,c_MX), type="prob")[,2] 
  } 
  if( mod_type == "logistic") {
    if(nrow(Q_dat) >= (ncol(Q_dat)-2)){
      phi_mod <- glm(UU ~ . , data=Q_dat, family="binomial")
      phi_val <- predict(phi_mod,data.frame(D_test,c_MX=c_MX), type="response")
    } else { phi_val=prior=rep(.5, length(D_test)) } 
  }
  if(updateprior) phi_val <- ((1-c_MX)*lambda + phi_val*length(Q_idx)) / (lambda + length(Q_idx))
  phi_val
}
# model_phi(true_misclass=sample(0:1,150,replace=T),
#         D_test=as.matrix(iris[,1:4]),
#         c_MX=runif(150),Q_idx=1:25)

#----------------------------------------
#!# Make this less of a mess next week
phi_all <- function(D_test, c_MX, true_misclass, Q_idx, tau=.65, prior=rep(.5, length(D_test)), 
                    phi_mod_type="rf",clust_out, updateprior=FALSE, lambda=2){
  if(phi_mod_type == "omniscient"){ 
    phi_D_test <- true_misclass
  } else if(length(Q_idx)>0){
    if(phi_mod_type == "rf"){ 
      # random forest needs at least one observation from each class (UU and non-UU) 
      if(length(unique(true_misclass[Q_idx]))==2){
        phi_D_test <- model_phi(true_misclass, D_test, Q_idx, c_MX, mod_type = phi_mod_type,
                                prior=prior, tau=tau, updateprior=updateprior, lambda=lambda)
      } else {
        phi_D_test <-clust_phi(true_misclass=true_misclass, D_test, Q_idx, clust_out,
                               c_MX, prior=prior, lambda=lambda, tau=tau)
      }
      
    } else if(phi_mod_type == "logistic"){ 
      # logistic needs at least one observation from each class (UU and non-UU) and n>p 
      if(length(unique(true_misclass[Q_idx]))==2 & length(Q_idx)>ncol(D_test)){
        phi_D_test <- model_phi(true_misclass, D_test, Q_idx, c_MX, mod_type = phi_mod_type, 
                                prior=prior, tau=tau,updateprior=updateprior, lambda=lambda)
      } else {
        phi_D_test <-clust_phi(true_misclass=true_misclass, D_test, Q_idx, clust_out,
                               c_MX, prior=prior, lambda=lambda, tau=tau)
      }
      
    } else if(phi_mod_type == "cluster_prior") {
      phi_D_test <-clust_phi(true_misclass=true_misclass, D_test, Q_idx, clust_out,
                             c_MX, prior=prior, lambda=lambda, tau=tau)
    } else {
      phi_D_test <- 1-c_MX
    }
  } else {
    phi_D_test <- 1-c_MX
  }
  return(phi_D_test)
}
# phi_all(D_test=as.matrix(iris[,1:4]),
#         true_misclass=sample(0:1,150,replace=T),
#         Q_idx=1:25, c_MX=runif(150), tau=.65, phi_mod_type="rf")

###------------------------------------------------------------------------------------------
# Adaptive Greedy algorithm for selecting new oracle queries:
# inputs: test set, confidence values, true prediction errors, initial oracle queries and a budget
# output: index of observations added to oracle query
adap_greedy <- function(D_test, c_MX, true_misclass, Q_prime_idx, B, tau=.65,
                        phi_mod_type="rf",clust_max=5,clust_set=NULL, prior=rep(.5,nrow(D_test)),
                        sigma=.001, scale=TRUE, updateprior=FALSE, lambda=2,
                        cost_fctn=conf_cost, ...){ 
  # Set up index partion of observations that are in/out of oracle query set
  D_test_idx <- 1:nrow(D_test)
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
      utility[b] <- t(cost_fctn(c_MX,phi_D_test)) %*% P_explainx_Q(sim_mat, Q_prime_idx[1:b], true_misclass, c_MX, tau)
    }
  }
  # For each step until we reach budget B:
  P_expx_Q <- P_explainx_Q(sim_mat, Q_idx, true_misclass, c_MX, tau)
  for (b in (length(Q_prime_idx)+1):B){
    # find optimal new q observations
    Sc_idx <- Qc_idx[c_MX[Qc_idx]>tau] #candidates must be of high enough confidence to possibly be UU
    q_new_idx <- Sc_idx[which.max(exp_utility_step_all(Sc_idx, phi_D_test, sim_mat, c_MX, P_expx_Q, cost_fctn, ...))]
    # updated query set
    Q_idx <- c(Q_idx,q_new_idx)
    Qc_idx <- make_Qc_index(D_test_idx,Q_idx)
    # update utility
    P_expx_Q <- P_explainx_Q(sim_mat, Q_idx, true_misclass, c_MX, tau)
    utility[b] <- t(cost_fctn(c_MX, phi_D_test)) %*% P_expx_Q
    # update phi 
    phi_D_test <- phi_all(D_test, c_MX, true_misclass, Q_idx, tau, prior, phi_mod_type,
                          clust_out,updateprior, lambda)
    if(b%%10==0) print(paste("Query",b,"of",B,"Complete"))
  }
  # return(list(Q_idx=Q_idx,utility=utility))
  return(data.frame(cost=as.character(body(cost_fctn))[length(as.character(body(cost_fctn)))],
                    phi=phi_mod_type, utility=utility, Q_idx=Q_idx, b=1:B))
}

### bundle into function to produce all result sets of interest
adaptive_query_comparison <- function(phi_mod_types = c("most_uncertain","omniscient"),
                                      cost_fctn_list=list(conf_cost),
                                      D_test, c_MX, true_misclass, Q_prime_idx = NULL,
                                      prior=1-c_MX, B=20, tau = .65, sigma=.001,
                                      clust_max=5,scale=TRUE, cost_fctn=conf_cost, ...){
  # apply greedy to all in list
  cost_results_list <-lapply(1:length(cost_fctn_list), function(cost_fctn_idx){
    phi_results <- lapply(phi_mod_types, function(phi_mod_type){
      adap_greedy(D_test=D_test, c_MX=c_MX, true_misclass=true_misclass,
                  Q_prime_idx=Q_prime_idx, B=B, tau=tau,
                  phi_mod_type=phi_mod_type,clust_max=clust_max,clust_set=NULL, 
                  prior=prior,sigma=sigma, scale=scale, updateprior=FALSE,lambda=2,
                  cost_fctn=cost_fctn_list[[cost_fctn_idx]])
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