# PREDICT 422 Practical Machine Learning

# Course Project - Example R Script File

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.

setwd("C:/Users/amustaki/Documents/MSPA")

source("functions.r")
#library(rJava)
library(bestglm)
library(glmulti)
library(glmnet)


# load the data
#charity <- read.csv(file.choose()) # load the "charity.csv" file
charity <- read.csv("charity.csv") # load the "charity.csv" file

# predictor transformations

#variable transformations to fix non-normal distributions
fixdata = function(dframe) {
  #convert categorical variables
  dframe$home = as.factor(dframe$home)
  dframe$hinc = as.factor(dframe$hinc)
  dframe$genf = as.factor(dframe$genf)
  dframe$wrat = as.factor(dframe$wrat)
  dframe$chld = as.factor(dframe$chld)
  
  dframe$region = with(dframe, ((reg1==1) + 2*(reg2==1) + 3*(reg3==1) + 4*(reg4==1)))
  dframe$region = as.factor(dframe$region)
  
  #remove the separate region variables
  dframe = dframe[,c(1,25,6:24)]

  dframe$log_avhv = log(1+dframe$avhv)
  dframe$log_incm = log(1+dframe$incm)
  dframe$log_inca = log(1+dframe$inca)
  dframe$log_tgif = log(1+dframe$tgif)
  dframe$log_lgif = log(dframe$lgif)
  dframe$log_rgif = log(dframe$rgif)
  
  #log doesn't seem to help
  #dframe$log_tlag = log(dframe$tlag)
  
  #not significant
  dframe$log_agif = log(dframe$agif)
  return(dframe)
}

charity.t = fixdata(charity)
head(charity)
head(charity.t)

#range of x values in charity.t
xrange.charity.t = c(2:18,22:28)
donr.charity.t = 19
damt.charity.t = 20

#range of numerics (don't include transformed variables)
xrange.numeric=8:17

#range of factors
xrange.factors=2:7

# set up data subsets for analysis

#training data, all columns
data.train <- charity.t[charity$part=="train",]

#training predictors
x.train <- data.train[,xrange.charity.t]

#training class response (donr)
c.train <- data.train[,donr.charity.t] # donr

#number of training observations
n.train.c <- length(c.train);n.train.c # 3984

#training regression response (damt)
data.train.y = data.train[c.train==1,]
x.train.y = data.train.y[,xrange.charity.t]
y.train <- data.train[c.train==1,damt.charity.t] # damt for observations with donr=1
n.train.y <- length(y.train); n.train.y # 1995

#validation data set (not used for fitting or EDA)
data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,xrange.charity.t]
c.valid <- data.valid[,donr.charity.t] # donr
n.valid.c <- length(c.valid);n.valid.c # 2018

#validation regression data set
data.valid.y = data.valid[c.valid==1,]
x.valid.y = data.train.y[,xrange.charity.t]
y.valid <- data.valid[c.valid==1,damt.charity.t] # damt for observations with donr=1
n.valid.y <- length(y.valid);n.valid.y # 999

#test data set (to be used for fitting final model)
data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1];n.test # 2007
x.test <- data.test[,xrange.charity.t]

### EDA ####

#this function calculates odd ratios for the specified column name
odds = function(dframe, col) {
  
  #calculate odds ratios
  tt = table(dframe$donr,dframe[[col]])
  p = tt[2,]/(tt[1,]+tt[2,])
  ods = tt[2,]/tt[1,]
  ratio = ods/ods[1]
  
  #sapply function for calculating odds ratio confidence intervals
  wald = function(ref, col) {
    o = oddsratioWald.proc(ref[1],col[1],ref[2],col[2])
  }
  xt = data.frame(tt[,-1])
  ci = sapply(xt,function(col) wald(tt[,1],col))
  
  #add to output
  lower = as.numeric(c(NA,ci["LowerCI",]))
  upper = as.numeric(c(NA,ci["UpperCI",]))

  x = list()
  x$confusion = tt
  x$table = data.frame(p=p,odds=ods,ratio=ratio,lower.ci=lower,upper.ci=upper)
  return(x)  
}

#perform EDA on named predictor variable vs DONR class response
eda_donr = function( dframe, col) {
  x = list()
  
  if( class(dframe[[col]]) != "factor" ) {
    par(mfrow=c(2,2))
    boxplot(dframe[[col]]~dframe$donr,col=c("blue","red"),horizontal=FALSE,main=paste(col,"by DONR"))
    x$t.test=t.test(dframe[[col]][dframe$donr==1],dframe[[col]][dframe$donr==0], alternative="two.sided")
    
    printf("Means: %f %f, p value: %f", x$t.test$estimate[1], x$t.test$estimate[2], x$t.test$p.value)
    
    #histogram 
    mbhist(x=dframe[[col]], class=dframe$donr,xlab=col,main=paste(col,"by DONR"))
  
    #qq plots
    qqnorm(dframe[[col]][dframe$donr==0],main=paste(col,"Donr==0" ))
    qqline(dframe[[col]][dframe$donr==0],col="blue")
    
    qqnorm(dframe[[col]][dframe$donr==1],main=paste(col,"Donr==1" ))
    qqline(dframe[[col]][dframe$donr==1],col="red")
    
    par(mfrow=c(1,1))
  } else {
    x = odds(dframe, col)
    print(x$confusion)
    print(x$table)
  }  
  
  return(x)
}

#perform EDA on named predictor variable vs DAMT class response
eda_damt = function( dframe, col) {
  x = list()
  if( class(dframe[[col]]) != "factor") {
    xx=dframe[[col]]
    yy=dframe$damt
    plot(x=xx, y=yy,xlab=col,ylab="Donor Amt")
    ll=loess.smooth(x=xx,y=yy,family="gaussian", evaluation = nrow(dframe))
    lines(ll, col="red",lty=1,lwd=2)
  } else {
    plot(x=dframe[[col]], y=dframe$damt,col="red",xlab=col,ylab="Donor Amt")
  }
  s = summary(lm(dframe$damt ~ dframe[[col]]))
  x$f.statistic=s$fstatistic[1]
  x$pvalue=1-pf(s$fstatistic[1],s$fstatistic[2],s$fstatistic[3])
  
  printf( "F Statistic: %f, p value: %f", x$f.statistic, x$pvalue)
  return(x)
}

# SET THIS TO TRUE TO SAVE PLOTS AS PNG
image_on = FALSE

saveimage = function(name){
  if(image_on==TRUE) {
    fname = paste("images/",name,".png",sep="")
    png(fname)
  }
}

doneimage = function() {
  if( image_on==TRUE) {
    dev.off()
  }
}

#pairs of factors vs donr
saveimage("pair_numerics")
mbpairs(data.train[xrange.numeric], c.train)
doneimage()

#pairs of numerics vs donr
saveimage("pair_factors")
mbpairs(data.train[xrange.factors], c.train)
doneimage()

#### EDA  #####


## factor variables

#some regions not significant
zz=eda_donr(data.train,"region")
saveimage("region")
zz=eda_damt(data.train.y,"region")
doneimage()

zz=eda_donr(data.train,"home")
saveimage("home")
zz=eda_damt(data.train.y,"home")
doneimage()

zz=eda_donr(data.train,"chld")
saveimage("chld")
zz=eda_damt(data.train.y,"chld")
doneimage()

zz=eda_donr(data.train,"hinc")
saveimage("hinc")
zz=eda_damt(data.train.y,"hinc")
doneimage()

#not significant
zz=eda_donr(data.train,"genf")
#not signficant
saveimage("genf")
zz=eda_damt(data.train.y,"genf")
doneimage()

zz=eda_donr(data.train,"wrat")
saveimage("wrat")
zz=eda_damt(data.train.y,"wrat")
doneimage()

### numeric variables

saveimage("avhv")
zz=eda_donr(data.train,"avhv")
doneimage()
saveimage("avhv_log")
zz=eda_donr(data.train,"log_avhv")
doneimage()

#not significant
saveimage("avhvdamt")
par(mfrow=c(1,2))
zz=eda_damt(data.train.y,"avhv")
zz=eda_damt(data.train.y,"log_avhv")
par(mfrow=c(1,1))
doneimage()

saveimage("incm")
zz=eda_donr(data.train,"incm")
doneimage()
saveimage("incm_log")
zz=eda_donr(data.train,"log_incm")
doneimage()

saveimage("incm_damt")
par(mfrow=c(1,2))
zz=eda_damt(data.train.y,"incm")
zz=eda_damt(data.train.y,"log_incm")
par(mfrow=c(1,1))
doneimage()

saveimage("inca")
zz=eda_donr(data.train,"inca")
doneimage()
saveimage("inca_log")
zz=eda_donr(data.train,"log_inca")
doneimage()
#not significant
saveimage("inca_damt")
par(mfrow=c(1,2))
zz=eda_damt(data.train.y,"inca")
zz=eda_damt(data.train.y,"log_inca")
par(mfrow=c(1,1))
doneimage()

#need a conversion, but not sure what
#*** not sure what this shows ***
saveimage("plow")
zz=eda_donr(data.train,"plow")
doneimage()
#need a conversion, but not sure what
#not significant
saveimage("plow_damt")
zz=eda_damt(data.train.y,"plow")
doneimage()

#log doesn't help, not linear
saveimage("npro")
zz=eda_donr(data.train,"npro")
doneimage()
saveimage("npro_damt")
zz=eda_damt(data.train.y,"npro")
doneimage()

saveimage("tgif")
zz=eda_donr(data.train,"tgif")
doneimage()
saveimage("tgif_log")
zz=eda_donr(data.train,"log_tgif")
doneimage()
#not linear
saveimage("tgif_damt")
par(mfrow=c(1,2))
zz=eda_damt(data.train.y,"tgif")
zz=eda_damt(data.train.y,"log_tgif")
par(mfrow=c(1,1))
doneimage()

#not significant
saveimage("lgif")
zz=eda_donr(data.train,"lgif")
doneimage()
saveimage("lgif_log")
zz=eda_donr(data.train,"log_lgif")
doneimage()
#not linear, log is?
saveimage("lgif_damt")
par(mfrow=c(1,2))
zz=eda_damt(data.train.y,"lgif")
zz=eda_damt(data.train.y,"log_lgif")
par(mfrow=c(1,1))
doneimage()

#not significant
saveimage("rgif")
zz=eda_donr(data.train,"rgif")
doneimage()
saveimage("rgif_log")
zz=eda_donr(data.train,"log_rgif")
doneimage()
#not linear, log is?
saveimage("rgif_damt")
par(mfrow=c(1,2))
zz=eda_damt(data.train.y,"rgif")
zz=eda_damt(data.train.y,"log_rgif")
par(mfrow=c(1,1))
doneimage()

#turn this into a bin? log doesn't help
#<14, 14-24, >24?
t(table(data.train$tdon,data.train$donr))
saveimage("tdon")
zz=eda_donr(data.train,"tdon")
doneimage()
#bins not necessary here?
saveimage("tdon_damt")
zz=eda_damt(data.train.y,"tdon")
doneimage()

#bin this?
#<8, >= 8?
t(table(data.train$tlag,data.train$donr))
saveimage("tlag")
zz=eda_donr(data.train,"tlag")
doneimage()
#bins not necessary here?
saveimage("tlag_damt")
zz=eda_damt(data.train.y,"tlag")
doneimage()

#not significant
saveimage("agif")
zz=eda_donr(data.train,"agif")
doneimage()
saveimage("agif_log")
zz=eda_donr(data.train,"log_agif")
doneimage()
#not linear, use log?
saveimage("agif_damt")
par(mfrow=c(1,2))
zz=eda_damt(data.train.y,"agif")
zz=eda_damt(data.train.y,"log_agif")
par(mfrow=c(1,1))
doneimage()

#### compare proportions of levels of factors in the sets

factors = list (
  "region",
  "home",
  "chld",
  "hinc",
  "genf",
  "wrat"
)

proportions = function(dframe) {
  
  for( x in factors) {
    nn = sum(as.numeric(table(dframe[[x]])))
    p = as.numeric(table(dframe[[x]]))/nn
  
    print(x)
    print(signif(p,2))  
  }
}

proportions(data.train)
proportions(data.valid)
proportions(data.test)


##### use variable selection to choose significant variables #####


# classification variables

## **** these don't have response variable ****
## *** scale these ***
Xy.train = data.frame(cbind(x.train,y=c.train))
Xy.valid = data.frame(cbind(x.valid,y=c.valid))

xm.train = model.matrix(y~.,data=Xy.train)[,-1]
xm.valid = model.matrix(y~.,data=Xy.valid)[,-1]

xm.train.damt = xm.train[c.train==1,]
xm.valid.damt = xm.valid[c.valid==1,]

nrow(xm.train)
nrow(xm.valid)
nrow(xm.train.damt)
nrow(xm.valid.damt)

standardize = function(x,y) {

  x = as.data.frame(x)  

  x.mean <- apply(x, 2, mean)
  
  #sd of all variables
  x.sd <- apply(x, 2, sd)
  
  #standardize all variables
  x.std <- t((t(x)-x.mean)/x.sd) # standardize to have zero mean and unit sd
  
  x.std=data.frame(cbind(x.std,y))

  return(x.std)
}


## **** these have standardized X variables, regular Y variables ***
x.train.std = standardize(xm.train,c.train)             #logistic training
x.train.std.damt = standardize(xm.train.damt,y.train)   #regression training
x.valid.std = standardize(xm.valid,c.valid)             #logistic validation
x.valid.std.damt = standardize(xm.valid.damt,y.valid)   #regression validation


nrow(x.train.std)
nrow(x.train.std.damt)
nrow(x.valid.std)
nrow(x.valid.std.damt)

### delete this stuff ###
#bestglm doesn't work with categorical variables with more than one level, so must
#expand them out.

#convert back to factors
factors = list(
  "region1",
  "region2",
  "region3",
  "region4",
  "home1",
  "chld1",
  "chld2",
  "chld3",
  "chld4",
  "chld5",
  "hinc2",
  "hinc3",
  "hinc4",
  "hinc5",
  "hinc6",
  "hinc7",
  "genf1",
  "wrat1",
  "wrat2",
  "wrat3",
  "wrat4",
  "wrat5",
  "wrat6",
  "wrat7",
  "wrat8",
  "wrat9",
  "y"
)

#for( f in factors) {
#  Xy[[f]]=as.factor(Xy[[f]])
#}

##### end of delete this stuff #####

#isn't working
#m_donr <- bestglm(x.train.std, IC="CV", CVArgs = list(Method = "HTF", K = 10, REP = 1), family=binomial)
#m_donr <- bestglm(x.train.std, IC="BIC", family=binomial)
#m_donr

m_donr = glm(y~., data=x.train.std, family=binomial)
summary(m_donr)

#takes too long
m_donr = glmulti(y~., data=x.train.std,
                 family=binomial,
                 crit="bic",
                 level = 1,               # No interaction considered
                 method = "h",            # Exhaustive approach
                 confsetsize = 5,         # Keep 5 best models
                 plotty = F, report = F,  # No plot or interim reports
                 fitfunction = "glm"  )  # glm function
summary(m_donr@objects[[1]])

#try using lasso
#cv.glmnet doesnt like data.frame, must be a matrix
#so recreate the model matrix from data.train, excluding the columns we don't want
#*** this includes both the original variables and the log transformations. Should it? ***

xx = model.matrix(donr~.-damt-part-ID,data=data.train)
cv.donr <- cv.glmnet(x=xx, y=c.train, alpha = 1, nfolds=10,family="binomial",type.measure="deviance")
plot(cv.donr)
plot.glmfit(cv.donr)

lambda = cv.donr$lambda.1se;lambda
m_donr=glmnet(x=xx, y=c.train, alpha=1, lambda=lambda,family = "binomial")
coef.donr = predict(m_donr,type="coefficients",s=lambda) #[1:p,]
coef.donr

#try again with scaled X variables
xx = model.matrix(y~.,data=x.train.std)
cv.donr <- cv.glmnet(x=xx, y=c.train, alpha = 1, nfolds=10,family="binomial",type.measure="deviance")
plot(cv.donr)
plot.glmfit(cv.donr)

lambda = cv.donr$lambda.1se;lambda
m_donr=glmnet(x=xx, y=c.train, alpha=1, lambda=lambda,family = "binomial")
coef.donr = predict(m_donr,type="coefficients",s=lambda) #[1:p,]
coef.donr

#insignificant variables: incm, rgif, log_inca, log_lgif, log_agif

#do cross validation on training data - didn't work
k = 10
#cv.damt = cv.regsubsets(y~.,data=x.train.std.damt,nvmax=2,nfolds=10)
cv.damt = cv.regsubsets(y~.,data=x.train.std.damt,nvmax=(ncol(x.train.std.damt)-1),nfolds=10)
cv.damt

m_damt = regsubsets( y~.,data=x.train.std.damt,nvmax=ncol(x.train.std.damt)-1)
s = summary(m_damt)
s

m = which.min(s$bic)
m

# chose 23 variables

cm = coef(m_damt,m)
cm

#chosen variables
#(Intercept)     region11     region21       home11       chld11       chld21       chld31       chld41       chld51 
#0.258353419  0.145377556  0.293234520  0.301288559 -0.373397411 -0.479361841 -0.558315975 -0.603325870 -0.683610314 
#hinc21       hinc31       hinc41       hinc51       hinc61       wrat41       wrat51       wrat61       wrat71 
#0.086716722  0.220596670  0.380243871  0.197475668  0.075855469  0.108565360  0.131213212  0.276395147  0.266948112 
#wrat81       wrat91         tdon         tlag     log_incm     log_tgif 
#0.274908050  0.265910669 -0.005176714 -0.013464978  0.121863501  0.095761842 

#### ***** EDA ENDS HERE ******



#### scale all variables: z = (x-u)/sd(x) ####

#means of all variables (this isn't working) - it wants a matrix?
xx = model.matrix( donr ~ .,data=data.train)

x.train.mean <- apply(x.train, 2, mean)

#sd of all variables
x.train.sd <- apply(x.train, 2, sd)

#standardize all variables
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd

#verify variables are standardized
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd

#combine with response variables
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

#standarize validation set
x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

#standardize test set
x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)



##### CLASSIFICATION MODELING ######

# linear discriminant analysis

library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
cutoff.lda1
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5

# logistic regression

model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5

# Results

# n.mail Profit  Model
# 1329   11624.5 LDA1
# 1291   11642.5 Log1

# select model.log1 since it has maximum profit in the validation sample

post.test <- predict(model.log1, data.test.std, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.log1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1676  331
# based on this model we'll mail to the 331 highest posterior probabilities

# See below for saving chat.test into a file for submission



##### PREDICTION MODELING ######

# Least squares regression

model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.867523
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1696615

# drop wrat for illustrative purposes
model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# 1.867433
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# 0.1696498

# Results

# MPE  Model
# 1.867523 LS1
# 1.867433 LS2

# select model.ls2 since it has minimum mean prediction error in the validation sample

yhat.test <- predict(model.ls2, newdata = data.test.std) # test predictions




# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="ABC.csv", row.names=FALSE) # use your initials for the file name

# submit the csv file in Canvas for evaluation based on actual test donr and damt values