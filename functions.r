
#include packages used by these functions:

library(ISLR)

#regsubsets
library(leaps)

#ridge and lasso
library(glmnet)

#principal components and partial least squares
library(pls)

#c style printf function
printf <- function(...) invisible(print(sprintf(...)))

#plots many kinds of models, including ridge and lasso
#library(plotmo)

#calculates the mse of 2 equal length vectors
calcmse = function(pred, y) {
  mse = mean((y-pred)^2)
  return( mse )
}


#makes preditions from an model fitted with regsubsets()
#object is the model
#newdata is the x matrix from which to calculate predictions
#id is the subset to use (# of variables)
predict.regsubsets=function(object,newdata,id,...){
  #call[[2]] is the formula (parameter 1 passed into regsubsets)
  form=as.formula(object$call[[2]])
  
  #formula was needed for model.matrix
  mat=model.matrix(form,newdata)
  
  #extract coefficents of the model
  coefi=coef(object,id=id)
  
  #names of the x variables (same as coefficent names)
  xvars=names(coefi)
  
  #matrix multiply to get predictions
  pred = mat[,xvars]%*%coefi
  
  #R automatically returns the results of the last calculation
  #but I prefer to specify it
  return( pred )
}

## returns the name of the response variable of a regsubsets regression object
yname = function(object) {
  
  f = as.formula(object$call[[2]])
  
  #response variable is element 2 of formula
  yname = as.character(f[2])
  return(yname)
}

#performs cross validation using a regsubsets object
#data is the data frame from which folds are drawn
#nfold is the number of folds
cv.regsubsets = function( formula, data, nfolds=10, nvmax=8, ... ) {
  
  #assign each row to a fold by drawing randomly from 1:nfold
  n = nrow(data)
  folds=sample(1:nfolds,n,replace=TRUE)

  #get vector of response variables
  yname = as.character(formula[2])
  #print(yname)
  #this isn't working
  y = data[[yname]]

  # make a matrix of errors [nfold x num variables]
  # dimnames - names of rows and columns
  errors=matrix(NA,nfolds,nvmax, dimnames=list(NULL, paste(1:nvmax)))
  
  #loop through folds
  for(j in 1:nfolds){
    #fit model using data not in the fold
    fit=regsubsets(formula,data=data[folds!=j,],nvmax=nvmax,...)
    
    #calculate predictions and errors for each variable size for this fold
    for(i in 1:nvmax){
      pred=predict(fit,data[folds==j,],id=i)
      ytest = y[folds==j]
      errors[j,i]=calcmse(pred, ytest)
    }
  }

  #apply works on a matrix (first parameter)
  # parameter 2: 1 = apply over rows, 2 = apply over columns
  # parameter 3: the function to apply to each row/column
  mean.cv.errors=apply(errors,2,mean)

  return(mean.cv.errors)
}

###
### calculate the MSE of each model size
### this also first gets predictions using newdata
### 
mse.regsubsets = function(object, newdata, ...) {
  k = object$nvmax-1
  yn = yname(object)
  y = newdata[yn]
  val.errors=rep(NA,k)
  for( i in 1:k) {
    pred = predict(object, newdata, id=i)
    val.errors[i]=mean((y-pred)^2)
  }
  return(val.errors)
}

## returns the smallest model id that is sd standard deviations from the minimum mse
which.mse = function(mse, sd) {

  #find the minimum mse  
  i = which.min(mse)
  val = mse[i]
  
  #the mse that we are looking for
  std = sd(mse)
  findmse = val + sd*std
  
  #start with the minimum value
  id = i;
  
  #numbers are not exact, so must do a search starting at minimum and counting down
  for( j in i:1) {
    
    #mse must be <= findmse
    if( mse[j] <= findmse) {
      id = j
    } else {
      break
    }
  }
  return(id)
}

#calculates MSE for each pcr/pls component that was fitted using cross validation
#fit is an object returned from pcr() or plsr() with validation="CV" specified
pcrmse = function(fit){
  ncomp = fit$ncomp
  mse = rep(NA,ncomp)
  yhat = fit$validation$pred
  
  #get y variable
  
  #get formula
  f = as.formula(fit$call[[2]])
  
  #response variable is element 2 of formula
  yname = as.character(f[2])
  y = fit$model[yname]
  
  #calculate mse
  for( i in 1:19) {
    mse[i] = mean((y-yhat[,,i])^2)
  }
  
  return(mse)  
}


#### plotting
#plot the model coefficents for various lambdas from cv.glmnet() output
plot.glmfit = function(cvout) {
  plot(cvout$glmnet.fit,xvar="lambda")
  abline(v=log(c(cvout$lambda.min,cvout$lambda.1se)),lty=2)
}

whitetest = function(r, yhat) {
  m = lm(I(r^2)~poly(yhat,2))
  s = summary(m)
  f = s$fstatistic
  pval =pf(q=f[1], df1=f[2], df2=f[3], lower=FALSE)
  
  ff = data.frame("F Statistic"=f[1], "P value"=pval)
  return(ff)
}


### Perceptron learning algorithm
# predict 422 Practical Machine Learning
# Input:
#   X: linearly separable training data set
#   y: label coding (assumed -1 and +1)
#   beta0: initital beta parameters, can be a single number or vector of size ncol(x)
#   tol: tolerance for convergence check
#   minepochs: minimum number of times to cycle all training points
#   maxepochs: maximum number of times to cycle all training points
#   verbose: print messages as iterations go
perceptron = function( x, y, beta0=1, tol=1e-8, minepochs=2, maxepochs=100, verbose=TRUE)
{
  N = nrow(x)
  p = ncol(x)
  
  if(length(y)!=N) {
    stop("The number of rows in x is not the length of y")
  }
  
  if( length(beta0) == 1 ) {
    beta0 = rep(beta0,p)
  }
  if( length(beta0) != p ) {
    stop("The dimension of beta0 is not the number of columns in x")
  }
  
  #make it stochastic!
  o = sample(N)
  x = x[o,]
  y = y[o]
  
  itlim = maxepochs * N
  itmin = minepochs * N
  
  eps = .Machine$double.xmax
  beta = beta0
  
  k = 0
  while(( eps>tol && k<itlim) || (k<itmin)) {
    beta0 = beta
    i = (k&&N) + 1
    
    #check if this point is misclassified
    d = y[i] * crossprod(beta,x[i,])
    if( d < 0 ) {
      #if it is misclassified then update beta
      beta = beta0 + y[1]*x[i,]
    }
    
    #update epsilon
    eps = sqrt(sum((beta0-beta)^2))/sqrt(sum(beta0^2))
    
    k = k+1
    if(verbose){
      cat(sprintf("It: %d i:%d d:%.4f eps: %.4f\n", k, i, d, eps))
    }
  }
  if( k==itlim) {
    warning("Failed to converge")
  }
  return(beta)
}

plot.bestglm = function(m,ic){
  errs = m$Subsets[[ic]]
  sdcol = paste("sd",ic,sep="")
  sd = m$Subsets[[sdcol]]
  if( length(sd)==0 ) {
    sd = rep(0,length(errs))
  }
  
  lo = errs - sd
  hi = errs + sd

  ymax = max(hi)
  ymin = min(lo)
  
  k = 0:(length(errs) - 1)
  plot(k, errs, ylim = c(ymin,ymax), type = "n", yaxt = "n",
       xlab="Number of Parameters",ylab=ic,main="Best Model")
  
  points(k,errs,cex = 2,col="black",pch=20)
  lines(k, errs, col = "black", lwd = 2)
  axis(2, yaxp = c(0.6, 1.8, 6))
  
  if( sum(sd) > 0){
    segments(k, lo, k, hi,col="blue", lwd = 2)
    eps = 0.15
    segments(k-eps, lo, k+eps, lo, col = "blue", lwd = 2)
    segments(k-eps, hi, k+eps, hi, col = "blue", lwd = 2)
  }
  
  #might not necessarily pick lowest err, use # of coefficients (excluding intercept)
  p = length(m$BestModel$coefficients)
  points(p-1,errs[p],cex = 2,col="red",pch=16)
}

#for extracting formula for use in glm()
formula.bestglm = function(m) {
  names = names(m$BestModel$coefficients[-1])
  form = as.formula(paste("y ~",paste(names,collapse=" + ")))
}

#plots posterior probabilities by class
hist.scored = function(class,prob){
  par(mfrow=c(2,1))
  hist(prob[class==1],main="Posterior Probabilities for Class=1",xlab="",col="red")
  hist(prob[class==0],main="Posterior Probabilities for Class=0",xlab="",col="blue")
  par(mfrow=c(1,1))
}

#class must be numeric (1 or 0), not factors
#convert to numeric before calling
mhlift = function(class, prob) {

  x = list()
  
  #sort the classes  
  indexes = order(prob,decreasing=TRUE)
  sorted_class = class[indexes]
  sorted_prob = prob[indexes]
  
  binsize = floor(length(indexes)/10)
  bins = c()
  for(i in 1:10) {
    b = 0
    l1 = (i-1)*binsize+1
    l2 = (i*binsize)
    b = sum(sorted_class[l1:l2]==1)
    bins = c(bins,b)
  }
  
  nevents = sum(class==1)
  rbin = rep(nevents/10,10)
  
  #get lift
  lift = bins/rbin
  x$vlift = lift
  x$lift = max(lift)
  
  cumbin = cumsum(bins)
  cumrbin = cumsum(rbin)
  pcum = cumbin/nevents
  prcum = cumrbin/nevents
  
  x$tested=prcum
  x$found=pcum
  dif = pcum-prcum;dif
  ks = max(dif)
  x$ks = max(dif)
  
  return(x)
}

#plot object returned from mhlift
plot.mhlift = function(x, add=FALSE, ...) {
   
  if( add == FALSE ) {
    op = par(pty="s")
    plot(x=c(0,x$tested),y=c(0,x$tested),lty=2,type='l', xlim=c(0,1), ylim=c(0,1), col="black",
         asp=1,xlab="Samples Tested",ylab="Samples Found",xaxs="i",main="Lift")
    
    axis(1, at = seq(0, 1, by = .1))
    axis(2, at = seq(0, 1, by = .1))
    
    pointsx = c(0,.5,1)
    pointsy = c(0,1,1)
    polygon(pointsx,pointsy,col = "grey95", border = FALSE)
    par(op)
  }
  
  lines(x=c(0,x$tested), y=c(0,x$found),type='l',...)
}

#create calibration object
mhcalibration = function(class, prob) {
  
  x = list()
  obsInBin = c()
  eventsInBin = c()
  
  bins=seq(0,1,by=.1)
  nbins = length(bins)
  for(i in 1:nbins) {
    #count events in bin
    if( bins[i] < 1) {
      #count events
      lo = bins[i]
      hi = bins[i+1]
      
      #get list of obs that fall in this range
      o = (prob>=lo & prob<hi)
      
      #number of obs
      n = sum(o)
      
      #number of events
      nevents = sum(o & class==1)
      
      obsInBin=c(obsInBin,n)
      eventsInBin=c(eventsInBin,nevents)
    }
  }
  
  x$midpoints = (bins[-1]+bins[-nbins])/2
  x$events = eventsInBin
  x$obs = obsInBin
  x$cal = eventsInBin/obsInBin
  return(x)
}

plot.mhcalibration = function(x, add = FALSE,...) {
  
  if( add == FALSE ) {
    op = par(pty="s")
    plot(x=c(0,x$midpoints),y=c(0,x$midpoints),lty=2,type='l', xlim=c(0,1), ylim=c(0,1), col="black",
         asp=1,xlab="Bin Midpoint",ylab="Observed Event Percentage",xaxs="i",main="Calibration")
    
    axis(1, at = seq(0, 1, by = .1))
    axis(2, at = seq(0, 1, by = .1))
    par(op)
  }
  
  lines(x=x$midpoints, y=x$cal,type='l',...)
}

#dframe contains the columns to pair
#class is the column with the classifications
mbpairs = function(dframe,class,...) {
  cols <- character(nrow(dframe))
  
  #event color = red
  rgb_event = rgb(1,0,0,0.5)
  
  #non event color = blue
  rgb_nonevent = rgb(0,0,1,0.5)
  
  #cols[class == 1] <- "red"
  #cols[class == 0] <- "blue"
  cols[class==1] = rgb_event
  cols[class==0] = rgb_nonevent
  
  pairs(dframe,col=cols, pch=1, 
        main="Pairs, Event vs non-Event")
}

#plot overlapping history of dframe by class
mbhist = function(x,class,...) {

  #event color = red
  rgb_event = rgb(1,0,0,0.5)
  
  #non event color = blue
  rgb_nonevent = rgb(0,0,1,0.5)
  
  #overlay histogram plots
  hist(x[class==0],col=rgb_nonevent,...)
  hist(x[class==1],col=rgb_event,add=TRUE,...)

  legend("topright", col = c(rgb_event,rgb_nonevent), 
         legend=c("Donor", "Non-Doner"), pch=15)
}

oddsratioWald.proc <- function(n00, n01, n10, n11, alpha = 0.05){
  #
  #  Compute the odds ratio between two binary variables, x and y,
  #  as defined by the four numbers nij:
  #
  #    n00 = number of cases where x = 0 and y = 0
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  #
  OR <- (n00 * n11)/(n01 * n10)
  #
  #  Compute the Wald confidence intervals:
  #
  siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  #
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  #
  oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, alpha = alpha)
  oframe
}

