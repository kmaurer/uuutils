### Configure data and parameters for running query algorithms in experiments
# need to save a D_test matrix of features, c_MX vector of confidence values, and true_misclass vector indicating where UU's have occured
tau=.65

### pang04
dat <- read.csv("./experiments/pang04Out.csv")
test_subset <- which(dat$Confidence > tau & dat$Prediction==1)
D_test = as.matrix(dat[test_subset,c("X1","X2")])
c_MX = dat$Confidence[test_subset]
true_misclass = dat$Misclassified[test_subset]
save(D_test,c_MX,true_misclass, file="./experiments/pang04config.Rdata")

# pang05
dat <- read.csv("./experiments/pang05Out.csv")
test_subset <- which(dat$Confidence > tau & dat$Prediction==1)
D_test = as.matrix(dat[test_subset,c("X1","X2")])
c_MX = dat$Confidence[test_subset]
true_misclass = dat$Misclassified[test_subset]
save(D_test,c_MX,true_misclass, file="./experiments/pang05config.Rdata")


# mcauley15
dat <- read.csv("./experiments/mcauley15Out.csv")
test_subset <- which(dat$Confidence > tau & dat$Prediction==1)
D_test = as.matrix(dat[test_subset,c("X1","X2")])
c_MX = dat$Confidence[test_subset]
true_misclass = dat$Misclassified[test_subset]
save(D_test,c_MX,true_misclass, file="./experiments/mcauley15config.Rdata")
