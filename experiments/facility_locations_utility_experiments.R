#-------------------------------------------------------------------------------
### Facility Location Utility Testing
#-------------------------------------------------------------------------------
setwd("~/Documents/github/uuutils/experiments")

library(ggplot2)
library(dplyr)
library(tidyr)
source("~/Documents/github/uuutils/bansal_weld_functions.R")
source("~/Documents/github/uuutils/facility_locations_utility.R")

# set general parameters
tau=.65
sigma=.001
clust_max=5
alpha=.5
B=100
Iterations = 200
sampsize = 1000
clust_set=NULL
phis <- c("rf","logistic","cluster_prior","omniscient","most_uncertain")

## Fast test check params
# B=5
# Iterations = 2
# sampsize = 100

#=================================================================================

# load pang04 D_test, c_MX and true_missclass
load(file="pang04config.Rdata")
clust_out <- UUclust(D_test, c_MX, clust_max=5, clust_set=NULL)

random_test <- NULL
for(i in 1:Iterations){ 
  sample_idx <- sample(1:nrow(D_test),sampsize)
  timer <- Sys.time()
  res_list <- lapply(phis, function(phi){
    fac_loc_utility_algorithm(D_test[sample_idx,], c_MX[sample_idx], true_misclass[sample_idx], Q_idx, B=B,tau=.65, prior=rep(.5, length(D_test)), 
                              phi_mod_type=phi,clust_out, updateprior=FALSE, alpha=alpha)
  })
  fl_util_results <- do.call(rbind,res_list)
  fl_util_results$time = difftime(Sys.time(), timer, units="secs")
  fl_util_results$iteration= i
  
  random_test <- rbind(random_test, fl_util_results )    
  print(paste("Done with iteration",i)) 
} 
# save(random_test, file="results_fl_pang04.Rdata")

#-------------------------------------------------------------------------------
# load pang05 D_test, c_MX and true_missclass
load(file="pang05config.Rdata")
clust_out <- UUclust(D_test, c_MX, clust_max=5, clust_set=NULL)

random_test <- NULL
for(i in 1:Iterations){ 
  sample_idx <- sample(1:nrow(D_test),sampsize)
  timer <- Sys.time()
  res_list <- lapply(phis, function(phi){
    fac_loc_utility_algorithm(D_test[sample_idx,], c_MX[sample_idx], true_misclass[sample_idx], Q_idx, B=B,tau=.65, prior=rep(.5, length(D_test)), 
                              phi_mod_type=phi,clust_out, updateprior=FALSE, alpha=alpha)
  })
  fl_util_results <- do.call(rbind,res_list)
  fl_util_results$time = difftime(Sys.time(), timer, units="secs")
  fl_util_results$iteration= i
  
  random_test <- rbind(random_test, fl_util_results )    
  print(paste("Done with iteration",i)) 
} 
# save(random_test, file="results_fl_pang05.Rdata")

#-------------------------------------------------------------------------------
load(file="mcauley15config.Rdata")
c_MX[c_MX == 1] <- .9999999
clust_out <- UUclust(D_test, c_MX, clust_max=5, clust_set=clust_set)

random_test <- NULL
for(i in 1:Iterations){ 
  sample_idx <- sample(1:nrow(D_test),sampsize)
  timer <- Sys.time()
  res_list <- lapply(phis, function(phi){
    fac_loc_utility_algorithm(D_test[sample_idx,], c_MX[sample_idx], true_misclass[sample_idx], Q_idx, B=B,tau=.65, prior=rep(.5, length(D_test)), 
                              phi_mod_type=phi,clust_out, updateprior=FALSE, alpha=alpha)
  })
  fl_util_results <- do.call(rbind,res_list)
  fl_util_results$time = difftime(Sys.time(), timer, units="secs")
  fl_util_results$iteration= i
  
  random_test <- rbind(random_test, fl_util_results )    
  print(paste("Done with iteration",i)) 
} 
# save(random_test, file="results_fl_mcauley15.Rdata")


#--------------------------------------------------------------------------------------------
# dat <- read.csv("/home/walter/Documents/karsten/poster/out.csv", header=T)
# # create columns for prediction name and misclassification indicator
# dat$pred <- ifelse(dat$PredictedClass==1,"dog","cat")
# dat$misclass <- ifelse(dat$pred != dat$DogCat, 1, 0)
# dat$FurType <- factor(dat$FurType,levels=c("lightfur","mixedfur","darkfur","other"))
# # drop all columns filled with zeros (or non changing values)
# feature_flag <- sapply(paste0("X",1:256), function(xs){
#   ifelse(length(unique(dat[,xs]))>1, 1,0)
# })
# true_features <- names(feature_flag[feature_flag==1])
# # apply to only light/dark furred from test set
# tau = .65
# set.seed(12345)
# test_data <- dplyr::filter(dat, Type=="test", 
#                            FurType %in% c("lightfur","darkfur"),
#                            Confidence > tau) 
# 
# pca_fur <- princomp(as.matrix(test_data[,true_features]))
# pca_fur$sdev[1:6]^2/sum(pca_fur$sdev^2)
# pca_fur$scores[,1:2]
# 
# D_test = pca_fur$scores[,1:2]
# c_MX = test_data$Confidence
# true_misclass = test_data$misclass
# save(D_test,c_MX,true_misclass, file="furdataconfig.Rdata")

file="furdataconfig.Rdata"
c_MX[c_MX == 1] <- .9999999
clust_out <- UUclust(D_test, c_MX, clust_max=5, clust_set=clust_set)

random_test <- NULL
for(i in 1:Iterations){ 
  sample_idx <- sample(1:nrow(D_test),sampsize)
  timer <- Sys.time()
  res_list <- lapply(phis, function(phi){
    fac_loc_utility_algorithm(D_test[sample_idx,], c_MX[sample_idx], true_misclass[sample_idx], Q_idx, B=B,tau=.65, prior=rep(.5, length(D_test)), 
                              phi_mod_type=phi,clust_out, updateprior=FALSE, alpha=alpha)
  })
  fl_util_results <- do.call(rbind,res_list)
  fl_util_results$time = difftime(Sys.time(), timer, units="secs")
  fl_util_results$iteration= i
  
  random_test <- rbind(random_test, fl_util_results )    
  print(paste("Done with iteration",i)) 
} 
# save(random_test, file="results_fl_kaggle14.Rdata")

#=====================================================================================
# combine all data tests results

datasetvec <- c("pang04","pang05","mcauley15","kaggle14")
all_random_test <- NULL
for (dataset in datasetvec){
  load(file=paste0("results_fl_",dataset,".Rdata"))
  all_random_test <- rbind(all_random_test,
                           data.frame(random_test,data_source=dataset))
}
head(all_random_test)
tail(all_random_test)
