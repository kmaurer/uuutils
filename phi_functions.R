# Functions for generating phi models using 
# cluster-based (BW), logistic or random forests

#!# these functions are a mess of if/else statements, need work to clarify

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

#---------------------------------------------------
### clustering to run other UU algorithms
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
        phi_D_test <-1-c_MX
        # phi_D_test <-clust_phi(true_misclass=true_misclass, D_test, Q_idx, clust_out,
        #                        c_MX, prior=prior, lambda=lambda, tau=tau)
      }
      
    } else if(phi_mod_type == "logistic"){ 
      # logistic needs at least one observation from each class (UU and non-UU) and n>p 
      if(length(unique(true_misclass[Q_idx]))==2 & length(Q_idx)>ncol(D_test)){
        phi_D_test <- model_phi(true_misclass, D_test, Q_idx, c_MX, mod_type = phi_mod_type, 
                                prior=prior, tau=tau,updateprior=updateprior, lambda=lambda)
      } else {
        phi_D_test <-1-c_MX
        # phi_D_test <-clust_phi(true_misclass=true_misclass, D_test, Q_idx, clust_out,
        #                        c_MX, prior=prior, lambda=lambda, tau=tau)
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
