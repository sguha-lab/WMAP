
rm(list=ls())

# install package in parent folder
source("../installPackage.R")

# load package
library(WMAP)

library(zeallot)

# set runtime parameters
num.random=25/5
B=200/20

# load demo dataset
data(demo)

## Stage 1 analysis: balancing.weights()

##########################################
############# FLEXOR WEIGHTS
##########################################

output1 = balancing.weights(S, Z, X, method="FLEXOR", naturalGroupProp, num.random)

summary(output1)



## Stage 2 analysis: causal.estimate()

##########################################
############# FLEXOR
##########################################

output2.f %<-% causal.estimate(S, Z, X, Y, B, method="FLEXOR", naturalGroupProp=naturalGroupProp, num.random, gammaMin=1e-3, gammaMax = (1-1e-3), seed=NULL)
summary(output2.f)

##########################################
############# IGO
##########################################

output2.igo %<-% causal.estimate(S, Z, X, Y, B, method="IGO")
summary(output2.igo)

##########################################
############# IC
##########################################
output2.ic %<-% causal.estimate(S, Z, X, Y, B, method="IC")
summary(output2.ic)


####### Table V results
# 95% CIs of mean, median and sd

write_res = function(estimates, CI){
  lower_bound <- CI[, 1]  # 2.5% confidence bound
  upper_bound <- CI[, 2]  # 97.5% confidence bound

  sapply(1:length(estimates), function(i) {
    paste0(round(estimates[i],2), " (", round(lower_bound[i], 2), ",", round(upper_bound[i], 2), ")")
  })
}


write_CI_res = function(output){ # the output function writing moments and corresponding 95% CI


  moments.ci = apply(output$collatedMoments.ar, c(1, 2, 3), function(x) {
    quantile(x, probs = c(0.025,0.975))
  })
  moments = list()
  for(i in 1:8){
    moments[[i]] = cbind(group1 = write_res(output$moments.ar[,1,i],t(moments.ci[,,1,i])), # group 1 mean sd median
                            group2 = write_res(output$moments.ar[,2,i],t(moments.ci[,,2,i]))) # group 2 mean sd median

  }

  return(moments)
}

  # output results
write_CI_res(output2.f)
write_CI_res(output2.ic)
write_CI_res(output2.igo)


# sigma ratios: \sigma^{(1)} / \sigma^{(2)}
write_sigma_ratio = function(output){ # the output function writing \sigma^{(1)} / \sigma^{(2)} results
  sigma_ratio_est = rep(NA, 8)
  for (i in 1:8) {
    sigma_ratio_est[i] = output$moments.ar[,,i][2,1]/output$moments.ar[,,i][2,2]
  }

  CI_sigma_ratio = matrix(0,nrow = 8, ncol = 2)
  for (i in 1:8) {
    sigma_ratio = rep(NA,B)
    for(b in 1:B){
      sigma_ratio[b] = (output$collatedMoments.ar[,,i,b][2,1]/output$collatedMoments.ar[,,i,b][2,2])
    }
    CI_sigma_ratio[i,] = quantile(sigma_ratio, probs = c(0.025,0.975))
  }
  write_res(sigma_ratio_est,CI_sigma_ratio)
}

  # output results
cbind(
  flexor = write_sigma_ratio(output2.f),
  ic = write_sigma_ratio(output2.ic),
  igo = write_sigma_ratio(output2.igo)
)

### summary functions
# Stage I FLEXOR weights
class(output1)
summary(output1)

# Stage II
# FLEXOR
class(output2.f)
summary(output2.f)

# IC
class(output2.ic)
summary(output2.ic)

# IGO
class(output2.igo)
summary(output2.igo)

#### plot() function for causal.estimate()
library(gridExtra)

grid.arrange(plot(output2.f, y_limit = c(0,40)),
             plot(output2.igo, y_limit = c(0,40), color = "green"),
             plot(output2.ic, y_limit = c(0,40), color = "blue"),
             ncol=3)



