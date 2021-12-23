#READ ME: 
# Hyper-contextual targeting has been a popular and effective advertisement tool due to global digital-
# ization. To enhance customer responses to marketing campaigns, companies have been collecting data
# and exploiting not only customer characteristics but also environmental factors. The main contextual
# factor that is analyzed in this research, is crowdedness in public transport. 
# variations in crowdedness on purchase likelihood, the Causal Forest algorithm is implemented (Athey, Tibshirani, Wager, et al. 2019). 
# This Machine Learning technique estimates heterogeneous treatment effects and allows for statistical inference.
# This research constructs a simulation framework that models customer heterogeneity and the non-linear effects of crowdedness.
# Below, you can find the implementation of the simulation framework and the Casual Forest from the grf-package in R.



#Installing of Packages

#Import the datasets 'Dataset1' and 'Data2' from the zip-file povided by INFORMS
#Install packages
install.packages("tidyverse")
library(tidyverse)
install.packages("grf")
library("grf")
install.packages("DiagrammeRsvg")
library("DiagrammeRsvg")
install.packages("DiagrammeR")
library("DiagrammeR")
install.packages("NMOF")
library("NMOF")
install.packages("matrixStats")
library("matrixStats")
install.packages("haven")
library("haven")
install.packages("glm")
library("glm")
install.packages("contrib.url")
library("contrib.url")



#Uploading the Data


Dataset1 = read_sav("C:/Users/509792ay/Downloads/Dataset1.sav")
Data2 = read_sav("C:/Users/509792ay/Downloads/Data2.sav")


min(Data2$v8)
max(Data2$v8)
min(Dataset1$v8)
max(Dataset1$v8)
#Preparing for Analysis

#Cleaning Data
Data2$v3[which(Data2$v3 == 2)] = 0 
datas = rbind(Data2, Dataset1)
datas = na.omit(datas)
datas = datas[,c(3:6,11:14)]
colnames(datas)[1] = 'weekend'
colnames(datas)[2] = 'peek'
colnames(datas)[3] = 'crowd'
colnames(datas)[4] = 'purchase'
colnames(datas)[5] = 'arpu'
colnames(datas)[6] = 'mou'
colnames(datas)[7] = 'sms'
colnames(datas)[8] = 'gprs'
summary(datas)
#plot(datas)
#cors = cor(datas)
#View(cors)

#Instrumentel Variables
lm = lm(datas$crowd ~ datas$weekend + datas$peek, data = datas)
summary(lm)
datas[,9] = lm$residuals
colnames(datas)[9] = 'exocrowd'

#Data generation
set.seed(1)
clusterdatas = as.matrix(datas[,c(1:2,5:8)])
cluster = kmeans(scale(clusterdatas), 4)
centers = cluster$centers
datas[,10] = cluster$cluster
colnames(datas)[10] = 'group'

#Recalculating centers
rcenters = as.data.frame(matrix(0,4,6))
for(g in 1:4){
  rcenters[g,] = colMeans(as.matrix(clusterdatas[which(datas$group == g),]))
}
colnames(rcenters)[1] = 'weekend'
colnames(rcenters)[2] = 'peek'
colnames(rcenters)[3] = 'arpu'
colnames(rcenters)[4] = 'mou'
colnames(rcenters)[5] = 'sms'
colnames(rcenters)[6] = 'gprs'
rcenters


#Implementation of Logit
lgt = glm(purchase ~ crowd + weekend + peek +arpu + mou + sms + gprs ,data = datas[which(datas$crowd>2),], family = "binomial")
lgt = glm(purchase ~ crowd + weekend + peek +arpu + mou + sms + gprs ,data = datas[which(datas$crowd<2),], family = "binomial")
lgt = glm(purchase ~ crowd + weekend + peek +arpu + mou + sms + gprs ,data = datas, family = "binomial")
summary(lgt)






#Simulating

beta = as.vector(c(-4.86,0.306,0.13,-0.03,-0.04,0.3,-0.06,0.03,-0.015))
coeff = as.vector(c(10,10,10,10,10,10,10,10,10))
counter = 90

#while(5*sum(coeff[c(2:3)]^2) + sum(coeff[c(6:9)]^2) > 0.01 ){

set.seed(counter)

datas[,11] = runif(6441,0.5,11)
colnames(datas)[11] = 'gcrowd'
datas[,12] = rbinom(6441, 1, 0.5)
colnames(datas)[12] = 'treatment'



nonlin = function(x){
  nl = -.18*cos(0.57*x)
  return(nl)
}

utility = numeric(6441)
indiv = numeric(6441)

for(i in 1:6441){
  for(t in 0:1){
    indiv[i] =0.27*nonlin(datas$gcrowd[i] + 0.72)  + -0.06*datas$weekend[i] -0.09*datas$peek[i] + 0.2*datas$arpu[i] + -0.1*datas$mou[i] + 0.1*datas$sms[i] - 0.05*datas$gprs
    
    utility[i] = -4.3 + indiv[i]*as.numeric(datas$treatment[i] == t) + nonlin(datas$gcrowd[i])*datas$gcrowd[i] + -0.03*datas$weekend[i] +  -0.045*datas$peek[i] + 0.3*datas$arpu[i] + -0.055*datas$mou[i] + 0.035*datas$sms[i] + -0.015*datas$gprs[i]  
  }
}

psm = exp(utility)/(1+exp(utility))
mean(psm)

#Assesment of heterogeneity

newpurchase = numeric(6441)
for(i in 1:6441){
  datas$newpurchase[i] = rbinom(1,1,psm[i])
}
colnames(datas)[13] = 'newpurchase'
sum(datas$newpurchase)/6441

table(datas$newpurchase, datas$group, datas$treatment)
centers
#table(datas$purchase, datas$group, datas$treatment)

#Assesment of non-linearity and regression
#not so important because experiment has changed
#lgt = glm(newpurchase ~ gcrowd + weekend + peek +arpu + mou + sms + gprs 
#          ,data = datas[which(datas$gcrowd>2 &datas$gcrowd<5.5 ),], family = "binomial", control = list(maxit = 50))
#fairly important to asses if it is close
#lgt = glm(newpurchase ~ gcrowd + treatment + gcrowd*treatment + weekend + peek +arpu + mou + sms + gprs 
#         ,data = datas[which(datas$gcrowd>2 &datas$gcrowd<5.5 ),], family = "binomial")
lgt = glm(newpurchase ~ gcrowd + treatment + weekend + peek +arpu + mou + sms + gprs 
          ,data = datas[which(datas$gcrowd>2 &datas$gcrowd<5.37 ),], family = "binomial")
coeff = lgt$coefficients  - beta
print(c(counter,5*sum(coeff[c(2:3)]^2) + sum(coeff[c(6:9)]^2), lgt$coefficients[c(2:3)]))
lgt$coefficients
summary(lgt)
lgt = glm(newpurchase ~ gcrowd + treatment + weekend + peek +arpu + mou + sms + gprs 
          ,data = datas[which(datas$gcrowd>0 &datas$gcrowd<2 ),], family = "binomial")
summary(lgt)
#counter = counter + 1

#131 798==========================================
#Causal forest
set.seed(8)

X = datas[,c(1:2,5:8,11)]
Y = datas$newpurchase
W = datas$treatment
tau.forest = causal_forest(X, Y, W,min.node.size = 100, 
                           tune.parameters = c("mtry", "honesty.fraction", "honesty.prune.leaves", "alpha"))
hist(tau.forest$predictions)
tree = get_tree(tau.forest,3)
plot(tree)

# Estimate treatment effects for the training data using out-of-bag prediction.
tau.hat.oob <- predict(tau.forest)
hist(tau.hat.oob$predictions)

# Estimate the conditional average treatment effect on the full sample/treated (CATE/CATT).
average_treatment_effect(tau.forest, target.sample = "all")
average_treatment_effect(tau.forest, target.sample = "control")
average_treatment_effect(tau.forest, target.sample = "treated")

variable_importance(tau.forest)
test_calibration(tau.forest)

#Causal forest for different thresholds
#low threshold 
Xl = datas[which(datas$gcrowd<3.185),]
Yl = Xl$newpurchase
Wl = Xl$treatment
Xl = Xl[,c(1:2,5:8,11)]
l = nrow(Xl)
tau.forest_l = causal_forest(Xl, Yl, Wl,min.node.size = floor(0.01*l), 
                             tune.parameters = c( "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha"))
tree_l = get_tree(tau.forest_l,2000)
plot(tree_l)
average_treatment_effect(tau.forest_l, target.sample = "all")
average_treatment_effect(tau.forest_l, target.sample = "control")
average_treatment_effect(tau.forest_l, target.sample = "treated")
variable_importance(tau.forest_l)
test_calibration(tau.forest_l)


#high threshold 
Xh = datas[which(datas$gcrowd>7.05),]
Yh = Xh$newpurchase
Wh = Xh$treatment
Xh = Xh[,c(1:2,5:8,11)]
h = nrow(Xh)
tau.forest_h = causal_forest(Xh, Yh, Wh, min.node.size = floor(0.01*h), 
                             tune.parameters = c( "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha"))
tree_h = get_tree(tau.forest_h,2000)
plot(tree_h)
average_treatment_effect(tau.forest_h, target.sample = "all")
average_treatment_effect(tau.forest_h, target.sample = "control")
average_treatment_effect(tau.forest_h, target.sample = "treated")
variable_importance(tau.forest_h)
test_calibration(tau.forest_h)

#middle threshold 
Xm = datas[which(datas$gcrowd>=3.185 & datas$gcrowd<=7.05),]
Ym = Xm$newpurchase
Wm = Xm$treatment
Xm = Xm[,c(1:2,5:8,11)]
m = nrow(Xm)
tau.forest_m = causal_forest(Xm, Ym, Wm, min.node.size = floor(0.01*m), 
                             tune.parameters = c( "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha"))
tree_m = get_tree(tau.forest_m,1001)
plot(tree_m)

average_treatment_effect(tau.forest_m, target.sample = "all")
average_treatment_effect(tau.forest_m, target.sample = "control")
average_treatment_effect(tau.forest_m, target.sample = "treated")
variable_importance(tau.forest_m)
test_calibration(tau.forest_m)

#others
Xm = datas[which(datas$gcrowd < 0.6),]
Ym = Xm$newpurchase
Wm = Xm$treatment
Xm = Xm[,c(1:2,5:8,11)]
m = nrow(Xm)
tau.forest_m = causal_forest(Xm, Ym, Wm, min.node.size = floor(0.01*m), 
                             tune.parameters = c( "mtry", "honesty.fraction", "honesty.prune.leaves", "alpha"))

average_treatment_effect(tau.forest_m, target.sample = "all")
test_calibration(tau.forest_m)

which(tau.forest_m$predictions/sd(tau.forest_m$predictions) >1.96)


