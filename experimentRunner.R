source("/home/bennettew/Documents/Summer2018/unknownUnknowns/uuutils/bansal_weld_functions.R")
source("/home/bennettew/Documents/Summer2018/unknownUnknowns/uuutils/phi_functions.R")
source("/home/bennettew/Documents/Summer2018/unknownUnknowns/uuutils/facility_locations_utility.R")
source("/home/bennettew/Documents/Summer2018/unknownUnknowns/uuutils/bandits.R")







##
## This function runs the Bansal Weld Greedy search
## for a set budget, phi_mod_type, and cost_fctn_vec
##
#' @param D_test A feature space of the test dataset stored in a matrix
#' @param c_MX The cost function stored in a vector
#' @param true_misclass Indicator of actual misclassification stored in a vector
#' @param Q_prime_idx Index of points which have already been labeled
#' @param tau Threshold confidence must exceed to be called an unknown unknown
#' @param phi_mod_type Mechanism to model the probability of an instance being an unknown unknown
#' @param clust_max If phi_mode_type is cluster based this dictates the maximum number of clusters per clustering phase
#' @param clust_set If clustering is done externally it can be passed in this way
#' @param prior The initial probability of an instance being an unknown unknown
#' @param sigma Value used for scaling
#' @param scale Scale or not scale
#' @param updateprior ?
#' @param lambda ?
#' @param B Query budget
#' @param cost_fctn The cost function to use
bansalWeldGreedyBudget <- function(D_test, c_MX, true_misclass, Q_prime_idx, tau=.65,
                                   phi_mod_type="rf",clust_max=5,clust_set=NULL, prior=rep(.5,nrow(D_test)),
                                   sigma=.001, scale=TRUE, updateprior=FALSE, lambda=2, B = 100,
                                   cost_fctn="conf_cost", ...){ 
  
  ##
  ## Set up queriable observations
  ##
  D_test_idx <- 1:nrow(D_test)
  if(phi_mod_type=="most_uncertain") Q_prime_idx <- order(c_MX)      ## if most uncertain the prime set is the most uncertain points
  
  ##
  ## Add priming set to tracked elements
  ##
  Q_idx <- Q_prime_idx
  Qc_idx <- make_Qc_index(D_test_idx,Q_idx)
  
  
  
  
  ##
  ## Initialize values
  ##
  dist_mat <- make_dist_mat(D_test)
  utility <- rep(NA,length(c_MX))                                         ## initalize utility storage, and make Q and Q-compliment set indeces
  numFound <- 0
  cumulativeConfidenceUnk <- 0
  cumulativeConfidence <- 0
  reward <- 0 
  found <- 0
  sum_min_dist <- 1
  unscaledDist <- 1
  unscaledReward <- 0
  sim_mat <- make_sim_mat(D_test=D_test, sigma, scale)                    ## calculate distance matrix for repeated use in P(explain_x | Q)
  clust_out <- UUclust(D_test, c_MX, clust_max=5, clust_set=clust_set)    ## generate clusters #!# this could be replaced with sampling algorithm to initialize our model based method
  
  phi_D_test <- phi_all(D_test, c_MX, true_misclass, Q_idx, tau, prior,
                        phi_mod_type, clust_out, updateprior, lambda)     ## initialize phi function with priors 
  
  b <- 1
  if(length(Q_prime_idx) > 0){                                            ## calculate and store utility gain in query priming set
    while(b <= B){
      cost_vals <- get(cost_fctn)(c_MX,phi_D_test)
      utility[b] <- t(cost_vals) %*% P_explainx_Q(sim_mat, Q_prime_idx[1:b], true_misclass, c_MX, tau)
      smallQ_idx <- Q_idx[1:b]
      S_idx <- smallQ_idx[true_misclass[smallQ_idx]==1]
      cumulativeConfidence[b] <- ifelse(is.null(S_idx), 0, sum(c_MX[smallQ_idx]))
      cumulativeConfidenceUnk[b] <- ifelse(is.null(S_idx), 0, sum(c_MX[S_idx]))
      unscaledDist[b] <- sum(min_dist(dist_mat, S_idx))/length(c_MX)
      unscaledReward[b] <- ifelse(is.null(S_idx), 0, sum(cost_c_MX(c_MX[S_idx])))
      if(true_misclass[Q_prime_idx[b]] == 1){
        numFound <- numFound + 1
      }
      found[b] <- numFound
      b <- b + 1
    }
  }
  
  ##
  ## For each step until we reach budget B:
  ##
  P_expx_Q <- P_explainx_Q(sim_mat, Q_idx, true_misclass, c_MX, tau)
  if(b <= B){
    while(b <= B){
      # find optimal new q observations
      Sc_idx <- Qc_idx[c_MX[Qc_idx]>tau] #candidates must be of high enough confidence to possibly be UU
      q_new_idx <- Sc_idx[which.max(exp_utility_step_all(Sc_idx, phi_D_test, sim_mat, c_MX, P_expx_Q, cost_fctn))]
      # updated query set and set of uncovered unknown unknowns
      Q_idx <- c(Q_idx,q_new_idx)
      S_idx <- Q_idx[true_misclass[Q_idx]==1]
      
      
      Qc_idx <- make_Qc_index(D_test_idx,Q_idx)
      # update utility
      P_expx_Q <- P_explainx_Q(sim_mat, Q_idx, true_misclass, c_MX, tau)
      utility[b] <- t(get(cost_fctn)(c_MX, phi_D_test)) %*% P_expx_Q
      # update phi 
      phi_D_test <- phi_all(D_test, c_MX, true_misclass, Q_idx, tau, prior, phi_mod_type,
                            clust_out,updateprior, lambda)
      cumulativeConfidence[b] <- ifelse(is.null(S_idx), 0, sum(c_MX[Q_idx]))
      cumulativeConfidenceUnk[b] <- ifelse(is.null(S_idx), 0, sum(c_MX[S_idx]))
      unscaledDist[b] <- sum(min_dist(dist_mat, S_idx))/length(c_MX)
      unscaledReward[b] <- ifelse(is.null(S_idx), 0, sum(cost_c_MX(c_MX[S_idx])))
      if(true_misclass[q_new_idx] == 1){
        numFound <- numFound + 1
      }
      found[b] <- numFound
      b <- b + 1
      
    }
    
  }
  
  
  #print(paste0("phi:",phi_mod_type," with cost:",cost_fctn," Complete"))
  return(data.frame(cost=cost_fctn,
                    phi=phi_mod_type,
                    utility=utility[1:(b-1)],
                    Q_idx=Q_idx[1:(b-1)],
                    b=1:(b-1),
                    cumulativeConfidence = cumulativeConfidence,
                    cumulativeConfidenceUnk = cumulativeConfidenceUnk,
                    unscaledDist = unscaledDist,
                    unscaledReward = unscaledReward,
                    found = found))
}


##
## This method runs and compiles results for 
## the Bansal Weld Greedy search for a number
## of phi_mod_types and cost_fctn_vec
##
#' 
#'
bansalWeldLargeBudget <- function(phi_mod_types = c("most_uncertain","omniscient"),
                                  cost_fctn_vec=c("conf_cost"),
                                  D_test, c_MX, true_misclass, Q_prime_idx = NULL,
                                  prior=1-c_MX, B=100, tau = .65, sigma=.001,
                                  clust_max=5,scale=TRUE, ...){
  # apply greedy to all in list
  cost_results_list <-lapply(1:length(cost_fctn_vec), function(cost_fctn_idx){
    phi_results <- lapply(phi_mod_types, function(phi_mod_type){
      bansalWeldGreedyBudget(D_test=D_test, c_MX=c_MX, true_misclass=true_misclass,
                             Q_prime_idx=Q_prime_idx, tau=tau, B = B,
                             phi_mod_type=phi_mod_type,clust_max=clust_max,clust_set=NULL, 
                             prior=prior,sigma=sigma, scale=scale, updateprior=FALSE,lambda=2,
                             cost_fctn=cost_fctn_vec[[cost_fctn_idx]])
    })
    do.call(rbind, phi_results)
  })
  do.call(rbind, cost_results_list)
} 


##
## This function runs the Facility Locatioin Greedy search
## for a set budget, phi_mod_type, and cost_fctn_vec
##
facilityLocationGreedyBudget <- function(D_test, c_MX, true_misclass, B = 100, tau=.65, prior=rep(.5, length(D_test)), 
                                          phi_mod_type="rf",clust_out, updateprior=FALSE, alpha=.5){
  
  # define
  dist_mat <- make_dist_mat(D_test)
  utility <- 0
  cumulativeConfidence <- 0
  cumulativeConfidenceUnk <- 0
  reward <- 0 
  unscaledReward <- 0
  found <- 0
  sum_min_dist <- 1
  unscaledDist <- 1
  Q_idx <- NULL
  scale_reward <- alpha
  scale_min_dist <- (1-alpha)/length(c_MX)
  numFound = 0
  b = 1
  # iterate
  while(b <= B){
    phi_D_test <- phi_all(D_test, c_MX, true_misclass, Q_idx, tau=.65, prior=rep(.5, length(D_test)), 
                          phi_mod_type=phi_mod_type,clust_out, updateprior=FALSE, lambda=2)
    new_q <- ifelse(phi_mod_type=="most_uncertain", order(c_MX)[b], best_candidate(Q_idx, true_misclass, phi_D_test, dist_mat, c_MX, B))
    Q_idx <- c(Q_idx,new_q)
    S_idx <- Q_idx[true_misclass[Q_idx]==1]
    reward[b] <- scale_reward*ifelse(is.null(S_idx), 0, sum(cost_c_MX(c_MX[S_idx])))
    sum_min_dist[b] <- scale_min_dist*sum(min_dist(dist_mat, S_idx))
    
    cumulativeConfidence[b] <- ifelse(is.null(S_idx), 0, sum(c_MX[Q_idx]))
    cumulativeConfidenceUnk[b] <- ifelse(is.null(S_idx), 0, sum(c_MX[S_idx]))
    unscaledDist[b] <- sum(min_dist(dist_mat, S_idx))/length(c_MX)
    unscaledReward[b] <- ifelse(is.null(S_idx), 0, sum(cost_c_MX(c_MX[S_idx])))
    utility[b] <- reward[b] - sum_min_dist[b]
    if(true_misclass[new_q] == 1){
      numFound <- numFound + 1
    }
    found[b] <- numFound
    b <- b + 1
  }
  print(paste("done with",phi_mod_type))
  return(data.frame(cost="c_MX",phi=phi_mod_type, Q_idx=Q_idx,
                    b=1:(b-1), utility=utility,
                    reward=reward, sum_min_dist =sum_min_dist,
                    cumulativeConfidence = cumulativeConfidence,
                    cumulativeConfidenceUnk = cumulativeConfidenceUnk,
                    unscaledDist = unscaledDist,
                    unscaledReward = unscaledReward,
                    found = found))
}


















