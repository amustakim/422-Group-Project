source("data.r")
source("functions.r")
source("score.r")
library(MASS)
library(pROC)

#function for using cross validation to test variable removal
glm.cv = function( f, k ) {
  
  profit = rep(0,k)  
  
  set.seed(1)
  folds = sample(1:k,nrow(xy.train), replace=TRUE)
  
  for( i in 1:k) {
    cv.fit <- glm( f, data=xy.train[folds!=i,], family=binomial("logit"))
    cv.pred = predict(cv.fit,newdata=xy.train[folds==i,],type="response")
    p = score_class_valid( c.train[folds==i], cv.pred )
    profit[i] = p$profit.max[2]
  }
  return(mean(profit))
}

#expand and standarize - expanding factors improves the fit
#data_dframe(expand=TRUE)
data_dframe(expand=FALSE)

xy.train = data_rm_dups(xy.train,"log_")
xy.valid = data_rm_dups(xy.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xy.train.y = data_rm_dups(xy.train.y,"log_")
xy.valid.y = data_rm_dups(xy.valid.y,"log_")

# test variable removal
glm.cv(
  
  #experiment with forumula
  donr ~ .-tlag -tdon -hinc + I(hinc^2)
  -genf
  -log_lgif
  -log_rgif
  -log_avhv 
  -log_agif 
  -reg4
  -log_inca
  
  #not significant but removing them makes profit worse
  -reg3
  -plow
  -npro
  
  # all in = 2351.45
  # all out = 2353.35
  
  # include reg3 2353.1
  # include plow 2352.95
  # include npro 2353.05
  
  # include reg3, plow 2353.15
  # include reg3, npro 2352.55
  # include plow npro 2351.85
  ,10)



#use when variables are not expanded
glm.fit <- glm(donr ~ .-tlag  -tdon -hinc +I(hinc^2)
               -genf
               -log_lgif
               -log_rgif
               -log_avhv 
               -log_agif 
               -reg4
               -log_inca
               
               #not significant but removing them makes profit worse
               -reg3
               -plow
               -npro
               
               ,data=xy.train, family=binomial("logit"))

glm.pred <- predict(glm.fit, xy.valid, type="response") # n.valid post probs
summary(glm.fit)
profit= score_class_valid( c.valid, glm.pred )
vif(glm.fit)
profit.glm = profit$profit.max[2]
profit.glm

#profit = 11820.5

#diagnostic plots
glm.roc <-  roc(response = c.valid, predictor=glm.pred)
plot(glm.roc)
auc(glm.roc)
glm.lift = mhlift(c.valid,glm.pred)
plot.mhlift(glm.lift,col="red")
glm.cal = mhcalibration(c.valid,glm.pred)
plot.mhcalibration(glm.cal,col="red")

############  use expanded variables ##############

data_dframe(expand=TRUE)

xy.train = data_rm_dups(xy.train,"log_")
xy.valid = data_rm_dups(xy.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xy.train.y = data_rm_dups(xy.train.y,"log_")
xy.valid.y = data_rm_dups(xy.valid.y,"log_")


#test variable removal 
glm.cv(
  
  #experiment with forumula
  donr ~ .-tlag -tdon
  -genf
  -log_lgif
  -log_rgif
  -log_avhv 
  -log_agif 
  -reg4
  -log_inca
  
  #not significant but removing them makes profit worse
  -reg3
  
  -hinc7
  -bin_hinc
  -wrat1
  
  #cross validation says that removing these two improves the fit
  #however, doing so makes it lightly worse (profit = 11919 -> 11914.5)
  -plow
  -npro
  
  #removed to improve vif
  #-wrat8
  -wrat9
  
  #all in = 2384.15
  #remove wrat9 = 2377.7
  #remove wrat8 = 2375.55
  #remove both = 2373.95
  ,10)



#profit = 11829.5
glm.fit <- glm(donr ~ .-tlag  -tdon
               -genf
               -log_lgif
               -log_rgif
               -log_avhv 
               -log_agif 
               -reg4
               -log_inca
               
               #not significant but removing them makes profit worse
               -reg3
               
               -hinc7
               -bin_hinc
               -wrat1

               #cross validation says that removing these two improves the fit
               #however, doing so makes it lightly worse (profit = 11919 -> 11914.5)
               -plow
               -npro
               
               #removed to improve vif
               #-wrat1
               #-wrat8
               -wrat9
               
               ,data=xy.train, family=binomial("logit"))

glm.pred <- predict(glm.fit, xy.valid, type="response") # n.valid post probs
summary(glm.fit)
profit= score_class_valid( c.valid, glm.pred )
vif(glm.fit)
profit.glm = profit$profit.max[2]
profit.glm

#diagnostic plots
glm.roc <-  roc(response = c.valid, predictor=glm.pred)
plot(glm.roc)
auc(glm.roc)
glm.lift = mhlift(c.valid,glm.pred)
plot.mhlift(glm.lift,col="red")
glm.cal = mhcalibration(c.valid,glm.pred)
plot.mhcalibration(glm.cal,col="red")


##### model with expanded factors is slightly better ######
#11820.5 vs 11829.5

