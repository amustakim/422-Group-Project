
source("data.r")
source("functions.r")
library(MASS)
library(pROC)
library(caret)

#don't expand
data_dframe(expand=FALSE)

xy.train = data_rm_dups(xy.train,"log_")
xy.valid = data_rm_dups(xy.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xy.train.y = data_rm_dups(xy.train.y,"log_")
xy.valid.y = data_rm_dups(xy.valid.y,"log_")

#full model = 11449.5
#best model 11483
qda.fit <- qda( donr ~ .-tlag  -tdon -hinc
                -genf
                -log_lgif
                -log_rgif
                -log_avhv
                -log_agif
                #-reg4
                -log_inca
                
                #-reg3
                -npro
                -plow

                ,data=xy.train)

qda.pred = predict(qda.fit,newdata=xy.valid, type="response")
qda.prob = qda.pred$posterior[,2]
summary(qda.fit)
#plot(qda.fit) no plot?
profit= score_class_valid( c.valid, qda.prob)
profit.qda = profit$profit.max[2]
profit.qda

qda.roc <-  roc(response = c.valid, predictor=qda.prob)
plot(qda.roc)
auc(qda.roc)
qda.lift = mhlift(c.valid,qda.prob)
plot.mhlift(qda.lift,col="red")
qda.cal = mhcalibration(c.valid,qda.prob)

#calibration chart sucks, glm is better
plot.mhcalibration(qda.cal,col="red")


###### now try expanded factors ###########


#don't expand
data_dframe(expand=TRUE)

xy.train = data_rm_dups(xy.train,"log_")
xy.valid = data_rm_dups(xy.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xy.train.y = data_rm_dups(xy.train.y,"log_")
xy.valid.y = data_rm_dups(xy.valid.y,"log_")

findLinearCombos(xy.train)

#get 'rank deficiency in group 0 error
#best = 11231
#11072 full model
qda.fit <- qda( donr ~ . -tlag  -tdon -bin_hinc
                -genf #11070
                -log_lgif #11070
                -log_rgif #11070
                -log_avhv  #11070
                -log_agif  #11067.5
                -reg4 #11068
                -log_inca #11071
                
                #-reg3
                -npro #11073
                -plow #11081.5
                
                #all gond = 11085.5
                -wrat1
                -wrat2 #11177.5
                -wrat3  #11173.5
                #-wrat4 #11153.5
                #-wrat5  #11145.5
                #-wrat6 #11119
                #-wrat7  #11094
                #-wrat8 #11112.5
                #-wrat9 #11119.5
                
                ,data=xy.train)

qda.pred = predict(qda.fit,newdata=xy.valid, type="response")
qda.prob = qda.pred$posterior[,2]
summary(qda.fit)
#plot(qda.fit) no plot?
profit= score_class_valid( c.valid, qda.prob)
profit.qda = profit$profit.max[2]
profit.qda

qda.roc <-  roc(response = c.valid, predictor=qda.prob)
plot(qda.roc)
auc(qda.roc)
qda.lift = mhlift(c.valid,qda.prob)
plot.mhlift(qda.lift,col="red")
qda.cal = mhcalibration(c.valid,qda.prob)

#calibration chart sucks, glm is better
plot.mhcalibration(qda.cal,col="red")

##### combinded data better than expanded ########
#11483 vs 11231
