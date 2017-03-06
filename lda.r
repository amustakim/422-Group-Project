

source("data.r")
source("functions.r")
library(MASS)
library(pROC)

#expand and standarize
data_dframe(expand=FALSE)

xy.train = data_rm_dups(xy.train,"log_")
xy.valid = data_rm_dups(xy.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xy.train.y = data_rm_dups(xy.train.y,"log_")
xy.valid.y = data_rm_dups(xy.valid.y,"log_")

#best = 11812.5
lda.fit <- lda( donr ~ .-tlag  -tdon -hinc +I(hinc^2)
               -genf  #11784.5
               -log_lgif #11776.5
               -log_rgif #11810.5
               -log_avhv #11810.5
               -log_agif #11808.5
               #-reg4 # 11804.5
               #-log_inca #11796.5

               #including: removing these = 11796.5  
               #remove: 11779.5
               #-reg3
               #-npro #11802
               -plow #11790
               
               ,data=xy.train)

lda.pred = predict(lda.fit,newdata=xy.valid, type="response")
lda.prob = lda.pred$posterior[,2]
summary(lda.fit)
plot(lda.fit)
profit= score_class_valid( c.valid, lda.prob)
profit.lda = profit$profit.max[2]
profit.lda

lda.roc <-  roc(response = c.valid, predictor=lda.prob)
plot(lda.roc)
auc(lda.roc)
lda.lift = mhlift(c.valid,lda.prob)
plot.mhlift(lda.lift,col="red")
lda.cal = mhcalibration(c.valid,lda.prob)

#calibration chart sucks, glm is better
plot.mhcalibration(lda.cal,col="red")


######## now try with expanded variables ###########

#expand and standarize
data_dframe(expand=TRUE)

xy.train = data_rm_dups(xy.train,"log_")
xy.valid = data_rm_dups(xy.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xy.train.y = data_rm_dups(xy.train.y,"log_")
xy.valid.y = data_rm_dups(xy.valid.y,"log_")

#get warnings that variables are collinear
#expanded variables don't work here?

#full = 11858
lda.fit <- lda( donr ~ .-tlag  -tdon
                -genf  #11859
                -log_lgif #11850
                -log_rgif #11861
                -log_avhv
                -log_agif
                
                -reg4 #11855
                #-log_inca #11851
                
                -reg3 #11858
                #-npro #11856.5
                #-plow #11854.5
                
                ,data=xy.train)

lda.pred = predict(lda.fit,newdata=xy.valid, type="response")
lda.prob = lda.pred$posterior[,2]
summary(lda.fit)
plot(lda.fit)
profit= score_class_valid( c.valid, lda.prob)
profit.lda = profit$profit.max[2]
profit.lda

lda.roc <-  roc(response = c.valid, predictor=lda.prob)
plot(lda.roc)
auc(lda.roc)
lda.lift = mhlift(c.valid,lda.prob)
plot.mhlift(lda.lift,col="red")
lda.cal = mhcalibration(c.valid,lda.prob)

#calibration chart sucks, glm is better
plot.mhcalibration(lda.cal,col="red")

#### expanded variables gives a better profit, but the warnings concern me #####
# numeric: 11812
# expanded factors: 11858

