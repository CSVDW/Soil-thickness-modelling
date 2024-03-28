#This script shows how to implement the IPC weighted random forest model (IPC-RF)
#
#
#
# Author: S VD WESTHUIZEN
# Stellenbosch University and Wageningen University (January 2024)
#
#

#Required packages
library(randomForestSRC) #for random forest modelling
library(survival)        #for calculating the Kaplan-Meier curves


#Import synthetic data
d = read.table("dat.txt", sep=",", header=T)

#Split data to train and test sets
set.seed(1243)
trn_ids = sample(1:nrow(d), .8*400) 
trn = d[trn_ids,]
tst = d[-trn_ids,]


#Calculate KM curve
surv = survfit(Surv(D_obs,delta_obs) ~ 1, data = trn)
surv_probs = surv$surv

#Make last entry non-zero
if(surv_probs[length(surv_probs)]==0) {surv_probs[length(surv_probs)] = .001}

#Get distinct observations
surv_ti = surv$time

#Set tau parameter (could be the percentile or a predefined expert deduced value)
tau = surv_ti[sum(surv_probs > .05)]  #use 5% for example

#Determine weights
ipc_surv = function(x) {
  ipc_surv = sapply(1:length(x), function(y) {
    surv_probs[min(which(surv_ti >= x[y]))]})} 
ipc_calc = ipc_surv(pmin(trn$D_obs,tau))
wts = ifelse(trn$D_obs >= tau | trn$delta_obs == 1, 1/ipc_calc, 0)

#Fit random forest model with weights (use defaults for mtry, node size, etc.)
RF = rfsrc(D_obs~X1+X2+X3, data=trn, case.wt = wts, samptype="swr")

#compare with regular model
RF_regular = rfsrc(D_obs~X1+X2+X3, data=trn)

#Assess models
(RF_p = predict(RF, tst))
(RF_regular_p = predict(RF_regular, tst))

#Mean error
mean(tst$D_obs - RF_p$predicted)
mean(tst$D_obs - RF_regular_p$predicted)

#Root mean square error
sqrt(mean((tst$D_obs - RF_p$predicted)^2))
sqrt(mean((tst$D_obs - RF_regular_p$predicted)^2))

#Mean absolute error
mean(abs(tst$D_obs - RF_p$predicted))
mean(abs(tst$D_obs - RF_regular_p$predicted))





