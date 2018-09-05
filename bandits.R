##
## Needs access to functions in "bansa_weld_functions.R", "facility_locations_utility.R", and "phi_functions.R"
##

bandit_search <- function(D_test, c_MX, true_misclass, B, tau=.65, clust_max=5, clust_set=NULL, sigma=.001, scale=TRUE){
  
  conf_cost <- function(c_MX,...) I(c_MX)

  clust_out <- UUclust(D_test, c_MX, clust_max, clust_set)      ## Create clusters+
  k <- length((unique(clust_out)))
  
  ##
  ## Places to hold information
  ##
  solution <- 0
  utility <- c()
  
  numFound <- 0
  cumulativeConfidenceUnk <- 0
  cumulativeConfidence <- 0
  found <- 0
  unscaledDist <- 1
  unscaledReward <- 0
  
  arms <- vector("list", k)
  arm_rewards <- vector("list", k)
  arm_times <- vector("list", k)
  sim_mat <- make_sim_mat(D_test)
  
  ##
  ## Each arm is a cluster. 
  ## Put the indicies of the instances from each cluster in the arm
  ##
  arm_initial_size <- c()
  for(i in sort(unique(clust_out))){
    arms[i] = list(which(clust_out == i))
    arm_initial_size <- c(arm_initial_size, length(arms[[i]]))
  }
     
  ##
  ## Helper functions
  ##
  disc <- function(arm, j, t){
    partOne <- arm_initial_size[arm] - length(arm_times[[arm]])
    partTwo <- arm_initial_size[arm] - length(which(arm_times[[arm]] <= j))
    return(partOne/partTwo)
  }
  
  n <- function(arm){
    return(length(arm_times[[arm]]))
  }
  
  u <- function(arm){
    return(sum(arm_rewards[[arm]])/n(arm))
  }
  
  n_disc <- function(arm, t){
    
    toReturn <- 0
    for(j in arm_times[[arm]]){
      toReturn <- toReturn + disc(arm, j, t)
    }
    return(toReturn)
  }
  
  ##
  ## Count number of uknowns found in that arm up to time t
  ##
  u_disc <- function(arm, t){
    toReturn <- 0
    for(i in 1:length(arm_times[[arm]])){
      partOne <- disc(arm, arm_times[[arm]][i], t) * arm_rewards[[arm]][i]
      toReturn <- toReturn + partOne
    }
    return(toReturn/n_disc(arm, t))
  }
  
  isvalid <- function(arm){
    if(length(arms[[arm]]) > 0){
      return(1)
    }
    return(0)
  }
  
  ##
  ## List of valid arms
  ##
  ne_arms <- c()
  for(i in 1:k){
    if(isvalid(i)){
      ne_arms <- c(ne_arms, i)
    }
  }
  
  ##
  ## Do this until out of budget
  ##
  for(t in 1:B){

    ##
    ## Find current arm
    ##   If not all arms sampled, curr_arm is the next arm
    ##   Otherwise curr_arm is the most promising arm
    curr_arm <- -10
    if(t <= length(ne_arms)){
      curr_arm <- ne_arms[t]
    }else{
      findMax <- c()
      for(i in 1:k){
        if(isvalid(i) == 1){
          partOne <- u_disc(i, t)
          partTwo <- sqrt(2*log(t)/n_disc(i, t))
          findMax <- c(findMax, partOne + partTwo)
        }else{
          findMax <- c(findMax, 0)
        }
      }
      curr_arm <- which.max(findMax) 
    }

    ##
    ## Find instance in arm to select
    ##
    toAdd <- 0
    if(length(arms[[curr_arm]]) != 1){
      # find optimal new q observations
      Sc_idx <- arms[[curr_arm]] 
      P_expx_Q <- P_explainx_Q(sim_mat, solution, true_misclass, c_MX, tau)
      q_new_idx <- Sc_idx[which.max(exp_utility_step_all(Sc_idx, rep(1/length(c_MX), length(P_expx_Q)), sim_mat, c_MX, P_expx_Q, "conf_cost"))]
      toAdd <- q_new_idx 
    }else{
      toAdd <- arms[[curr_arm]][1]
    }
    ##
    ## Remove selected instance from arm
    ##
    arms[[curr_arm]] = arms[[curr_arm]][which(arms[[curr_arm]] != toAdd)]
    
    ## 
    ## Add instance to solution
    ## 
    solution[t] <- toAdd
    
    ##
    ## Update utility
    ##
    P_expx_Q <- P_explainx_Q(sim_mat, solution, true_misclass, c_MX, tau)
    utility <- c(utility, t(get("conf_cost")(c_MX, rep(1/length(c_MX), length(P_expx_Q))) %*% P_expx_Q))
    
    ##
    ## Track reward
    ##
    reward <- utility[length(utility)]
    if(length(utility) > 1){
      reward <- utility[length(utility)] - utility[length(utility)-1]
    }
    if(reward > 0.01){
      reward = 1
    }else{
      reward = 0
    }
    
    arm_rewards[[curr_arm]] <- c(arm_rewards[[curr_arm]], reward)
    arm_times[[curr_arm]] <- c(arm_times[[curr_arm]], t)

    ##
    ## Track a bunch of other stuff
    ##
    S_idx <- solution[true_misclass[solution]==1]
    cumulativeConfidence[t] <- ifelse(is.null(solution), 0, sum(c_MX[solution]))
    cumulativeConfidenceUnk[t] <- ifelse(is.null(S_idx), 0, sum(c_MX[S_idx]))
    unscaledDist[t] <- sum(min_dist(sim_mat, S_idx))/length(c_MX)
    unscaledReward[t] <- ifelse(is.null(S_idx), 0, sum(cost_c_MX(c_MX[S_idx])))
    found[t] <- length(S_idx)
    
  }
  
  return(data.frame(cost="cost_conf",
                    phi="Bandits",
                    utility=utility,
                    Q_idx=solution,
                    b=1:B,
                    cumulativeConfidence = cumulativeConfidence,
                    cumulativeConfidenceUnk = cumulativeConfidenceUnk,
                    unscaledDist = unscaledDist,
                    unscaledReward = unscaledReward,
                    found = found))
}









































