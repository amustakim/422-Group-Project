## this file creates the data sets for the charity project
##
##  category variables are encoded as numerics, except for reg
##  provided example code uses these sets for the models
##
## data.train.std.c   = training set standardized X variables, transformations, and donr
## data.train.std.y   = training set standardized X variables, transformations, and damt
## data.valid.std.c   = validation set standardized X variables, transformations, and donr
## data.valid.std.y   = validation set standardized X variables, transformations, and damt
## data.test.std      = test set standardized X variables, transformations

##    Category variables are broken out into individual dummy variables
##
## x.train.std        = training set standardized X variables, transformations, expanded facors, and donr
## x.train.std.damt   = training set standardized X variables, transformation, expanded facors, and damt
## x.valid.std        = validation set standardized X variables, transformations, expanded facors, and donr
## x.valid.std.damt   = validation set standardized X variables, transformation, expanded facors, and damt
## x.test.std         = test set standardized X variables, transformations, expanded facors

## c.train            = training donr column
## c.valid            = validation donr column
## y.train            = training damt column
## y.valid            = validation damt column

## in addtion, it contains functions for generating data sets on the fly
## if expandedFactors = TRUE, factor are expanded into individual dummy variables (similar to x. data sets above)
## if std = TRUE, variables are converted to numeric and centered on the training set mean and standard deviation

## data_dframe( expandedFactors, std )
##    data sets are data.frames, contains all X variables and the appropriate y variable (donr or damt)
## xy.train       training set for donr
## xy.train.y     training set for damt
## xy.valid       validatin set for donr
## xy.valid.y     validation set for damt
## xx.test        test set, only X variables

## data_matrix( formula, expandedFactors, std )
##    data sets are matrix, contains only the x variables specified in the formula, does not have y variable
## xx.train       training set for donr
## xx.train.y     training set for damt
## xx.valid       validatin set for donr
## xx.valid.y     validation set for damt
## xx.test        test set, only X variables

source("functions.r")

# load the data
#charity <- read.csv(file.choose()) # load the "charity.csv" file
charity <- read.csv("charity.csv") # load the "charity.csv" file

# predictor transformations

#variable transformations to fix non-normal distributions
fixdata = function(dframe) {
  #convert categorical variables
  dframe$reg1 = as.factor(dframe$reg1)
  dframe$reg2 = as.factor(dframe$reg2)
  dframe$reg3 = as.factor(dframe$reg3)
  dframe$reg4 = as.factor(dframe$reg4)
  dframe$home = as.factor(dframe$home)
  dframe$hinc = as.factor(dframe$hinc)
  dframe$genf = as.factor(dframe$genf)
  dframe$wrat = as.factor(dframe$wrat)
  dframe$chld = as.factor(dframe$chld)
  
  #dframe$reg = with(dframe, ((reg1==1) + 2*(reg2==1) + 3*(reg3==1) + 4*(reg4==1)))
  #dframe$reg = as.factor(dframe$reg)
  
  #rebin hinc
  dframe$bin_hinc = as.factor(1*(dframe$hinc==4))
  
  #remove the separate region variables and reorder
  #lastcol = dim(dframe)[2]
  #dframe = dframe[,c(1,25,6:(lastcol-1))]
  
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
  
  #bin tdon
  #make the middle bin (where donr is more likely==1) the reference bin (bin 0)
  #bins are <14, 14-24, >24, bin 14-24 is reference bin
  #dframe$bin_tdon = as.factor(1 * (dframe$tdon<14) + 2 * (dframe$tdon>24))

  #bin = 1 if 14 <= tdon <= 24
  dframe$bin_tdon = as.factor(1 * (dframe$tdon>=14 & dframe$tdon <= 24))
  
  #bin tlag
  #to be consistent, the reference bin is where probability that donr==1 (tlag<8)
  #<8, >= 8?
  dframe$bin_tlag = as.factor(1*(dframe$tlag>7))
  
  #reorder - put factors in front, response and filter variables at end
  lastcol = dim(dframe)[2];lastcol
  #dframe = dframe[,c(1:7,29:30,8:18,22:28,19:21)]
  dframe = dframe[,c(1:10,25,33,34,11:21,26:32,22:24)]
  lastcol = dim(dframe)[2];lastcol
  
  return(dframe)
}

##### functions for changing data frame columns between factors and numerics #####

#gets a vector that indicates which columns are factors
get_factors = function(dframe) {
  
  f = sapply(names(dframe), function(col) is.factor(dframe[[col]]))
  return(f)
}

#set the columns of dframe to factors using as.factor if the corresponding
#item in the factors vector is TRUE
set_factors = function(dframe, factors) {
  z = dframe
  n.dframe = dim(dframe)[2]
  n.factors = length(factors)
  
  if( n.dframe == n.factors ) {
    for( i in 1:n.factors) {
      if( factors[i]) {
        z[,i]=as.factor(z[,i])
      } else {
        z[,i]=as.numeric(as.character(z[,i]))
      }
    }
    
  }else{
    printf( "Error: data frame has %d columns, but factors length = %d", n.dframe, n.factors )
  }
  return(z)
}

#### set all columns to numerics ####
set_all_numeric = function(dframe) {
  
  f = rep(FALSE, dim(dframe)[2])
  z = set_factors(dframe,f)
  return(z)
}

#### set factors with only 2 levels to numeric ####
#### to avoid unneccessarily mangling names when ###
#### model.matrix() creates dummy variables for factors ####
set_2level_numeric = function(dframe) {

  z = dframe
  
  for( col in names(z)) {
    if( class(z[[col]]) == "factor") {
      if( length(attributes(z[[col]])$levels) <= 2) {
        z[[col]] = as.numeric(as.character(z[[col]]))
      }
    }
  }
  
  return(z)
}

charity.t = fixdata(charity)
head(charity)
head(charity.t)

lastcol=dim(charity.t)[2];lastcol

#column range of x values in charity.t
xrange.charity.t = c(2:(lastcol-3))
#columns of factor variables
factors.range.charity.t = c(2:13)
#columns of numerics
numerics.range.charity.t = c(14:(lastcol-3))

#column of donr
donr.charity.t = lastcol-2
#column of damt
damt.charity.t = lastcol-1

#create separate data sets of the differenet parts
x.charity.t = charity.t[,xrange.charity.t]
factors.charity.t = charity.t[,factors.range.charity.t]
numerics.charity.t = charity.t[,numerics.range.charity.t]
c.charity.t = charity.t[,donr.charity.t]
y.charity.t = charity.t[,damt.charity.t]

#the different partitions
train = charity$part=="train"
valid = charity$part=="valid"
test = charity$part=="test"

# set up data subsets for analysis
# these are set up to be used for EDA
# regression functions will have to set their own if they need it

#training data, all columns
data.train <- charity.t[train,]

#training predictors
x.train <- x.charity.t[train,]

#training class response (donr)
c.train <- c.charity.t[train]

#number of training observations
n.train.c <- length(c.train);n.train.c # 3984

#training regression response (damt)
data.train.y = data.train[c.train==1,]
x.train.y = data.train.y[,xrange.charity.t]
y.train <- data.train[c.train==1,damt.charity.t] # damt for observations with donr=1
n.train.y <- length(y.train); n.train.y # 1995

#validation data set (not used for fitting or EDA)
data.valid <- charity.t[valid,]
x.valid <- x.charity.t[valid,]
c.valid <- c.charity.t[valid]
n.valid.c <- length(c.valid);n.valid.c # 2018

#validation regression data set
data.valid.y = data.valid[c.valid==1,]
x.valid.y = data.train.y[,xrange.charity.t]
y.valid <- data.valid[c.valid==1,damt.charity.t] # damt for observations with donr=1
n.valid.y <- length(y.valid);n.valid.y # 999

#test data set (to be used for fitting final model)
data.test <- charity.t[test,]
n.test <- dim(data.test)[1];n.test # 2007
x.test <- x.charity.t[test,];dim(x.test)

#make standardized data sets 
Xy.train = data.frame(cbind(x.train,y=c.train))
Xy.valid = data.frame(cbind(x.valid,y=c.valid))

#to avoid mangling column names as they are expanded, convert factors
#with only 2 levels to numerics before calling model.matrix()


xm.train = model.matrix(y~.,data=set_2level_numeric(Xy.train))[,-1]
xm.valid = model.matrix(y~.,data=set_2level_numeric(Xy.valid))[,-1]
xm.test = model.matrix(~.,data=set_2level_numeric(x.test))[,-1]

xm.train.damt = xm.train[c.train==1,]
xm.valid.damt = xm.valid[c.valid==1,]

#mean and standard deviation of the trainin set.
#these are used to standardize all 3 chunks (train, valid, test)
x.train.mean <- apply(xm.train, 2, mean)
x.train.sd <- apply(xm.train, 2, sd)

#standardize the X variables
standardizeX = function(x, x.mean=x.train.mean, x.sd = x.train.sd) {
  
  x = as.data.frame(x)  
  x.std <- t((t(x)-x.mean)/x.sd) # standardize to have zero mean and unit sd

  return(x.std)
}

#standardize x variable, then add y column
standardize = function(x,y, x.mean=x.train.mean, x.sd = x.train.sd) {
  
  x = as.data.frame(x)  
  x.std <- standardizeX(x, x.mean, x.sd)
  x.std=cbind(x.std,y)
  
  return(x.std)
}

#make standardized data sets (includes y variable)
x.train.std = standardize(xm.train,c.train)             #logistic training
x.train.std.damt = standardize(xm.train.damt,y.train)   #regression training
x.valid.std = standardize(xm.valid,c.valid)             #logistic validation
x.valid.std.damt = standardize(xm.valid.damt,y.valid)   #regression validation
x.test.std = standardizeX(xm.test)    # standardize using training mean and sd


#save which columns were originally factors
#may want this later
isfactor.charity.t=get_factors(charity.t)

#### make multiple data sets ######

#### X variables as factors #####

xf.train    = data.train[,xrange.charity.t];    dim(xf.train)
xf.train.y  = data.train.y[,xrange.charity.t];  dim(xf.train.y)
xf.valid    = data.valid[,xrange.charity.t];    dim(xf.valid)
xf.valid.y  = data.valid.y[,xrange.charity.t];  dim(xf.valid.y)
xf.test     = data.test[,xrange.charity.t];     dim(xf.test)

#### X variables as numerics (factors levels combined into 1 variable each) ####
xfn.train   = set_all_numeric(xf.train)
xfn.train.y = set_all_numeric(xf.train.y)
xfn.valid   = set_all_numeric(xf.valid)
xfn.valid.y = set_all_numeric(xf.valid.y)
xfn.test    = set_all_numeric(xf.test)

#### standardize xfn sets ######
xfn.mean <- apply(as.matrix( xfn.train ), 2, mean )
xfn.sd <- apply(as.matrix( xfn.train ), 2, sd )

xfn.train.std     = standardizeX( xfn.train, xfn.mean, xfn.sd )
xfn.train.y.std   = standardizeX( xfn.train.y, xfn.mean, xfn.sd )
xfn.valid.std     = standardizeX( xfn.valid, xfn.mean, xfn.sd )
xfn.valid.y.std   = standardizeX( xfn.valid.y, xfn.mean, xfn.sd )
xfn.test.std      = standardizeX( xfn.test, xfn.mean, xfn.sd )

##### X variables with factors expanded ######
xen.train    = model.matrix(~., data = set_2level_numeric(xf.train) )[,-1]
xen.train.y  = model.matrix(~., data = set_2level_numeric(xf.train.y) )[,-1]
xen.valid    = model.matrix(~., data = set_2level_numeric(xf.valid) )[,-1]
xen.valid.y  = model.matrix(~., data = set_2level_numeric(xf.valid.y) )[,-1]
xen.test     = model.matrix(~., data = set_2level_numeric(xf.test) )[,-1]

##### X variables with factors expanded and standardized ######
xen.mean <- apply(as.matrix( xen.train ), 2, mean )
xen.sd <- apply(as.matrix( xen.train ), 2, sd )

xen.train.std     = standardizeX( xen.train, xen.mean, xen.sd )
xen.train.y.std   = standardizeX( xen.train.y, xen.mean, xen.sd )
xen.valid.std     = standardizeX( xen.valid, xen.mean, xen.sd )
xen.valid.y.std   = standardizeX( xen.valid.y, xen.mean, xen.sd )
xen.test.std      = standardizeX( xen.test, xen.mean, xen.sd )


### reproduce the variable names from the original sample code
data.train.std.c <- data.frame(xfn.train.std, donr=c.train)
data.train.std.y <- data.frame(xfn.train.y.std, damt=y.train)
data.valid.std.c <- data.frame(xfn.valid.std, donr=c.valid)
data.valid.std.y <- data.frame(xfn.valid.y.std, damt=y.valid)
data.test.std <- xfn.test.std


#make data matrices for use with functions like glmnet
#this is for use in functions that don't allow the variables to be chosen
#the forumula defines the X variables to include, the resulting matrix
#has only the X variables specified
data_matrix = function(formula=~., expandFactors=TRUE, std=TRUE) {
  
  if( expandFactors == TRUE) {
    
    #start with the X variables that have factors. model.matrix will expand them into individual dummy variables
    xtrain = xf.train
    xtrainy = xf.train.y
    xvalid = xf.valid
    xvalidy = xf.valid.y
    xtest = xf.test
    
  } else {
    #start with the X variables that have numerics.  model.matrix will leave them alone
    xtrain = xfn.train
    xtrainy = xfn.train.y
    xvalid = xfn.valid
    xvalidy = xfn.valid.y
    xtest = xfn.test
  }
  
  #use model.matrix to select the specified columns
  xxtrain   = model.matrix(formula,data=set_2level_numeric(xtrain))[,-1]
  xxtrainy  = model.matrix(formula,data=set_2level_numeric(xtrainy))[,-1]
  xxvalid   = model.matrix(formula,data=set_2level_numeric(xvalid))[,-1]
  xxvalidy  = model.matrix(formula,data=set_2level_numeric(xvalidy))[,-1]
  xxtest    = model.matrix(formula,data=set_2level_numeric(xtest))[,-1]
  
  #standardize them
  if( std == TRUE ) {

    xxmean <- apply(as.matrix( xxtrain ), 2, mean )
    xxsd <- apply(as.matrix( xxtrain ), 2, sd )
    
    xxtrain   = standardizeX( xxtrain, x.mean = xxmean, x.sd = xxsd )
    xxtrainy  = standardizeX( xxtrainy, x.mean = xxmean, x.sd = xxsd )
    xxvalid   = standardizeX( xxvalid, x.mean = xxmean, x.sd = xxsd )
    xxvalidy  = standardizeX( xxvalidy, x.mean = xxmean, x.sd = xxsd )
    xxtest    = standardizeX( xxtest, x.mean = xxmean, x.sd = xxsd )
  }

  #make them global so they can be used
  #considered bad programming practice
  
  assign("xx.train", xxtrain, envir=.GlobalEnv)
  assign("xx.valid", xxvalid, envir=.GlobalEnv)
  assign("xx.test", xxtest, envir=.GlobalEnv)
  assign("xx.train.y", xxtrainy, envir=.GlobalEnv)
  assign("xx.valid.y", xxvalidy, envir=.GlobalEnv)
}

#create a data set suitable for lm()
#if expandFactors = TRUE, factor variables will be replaced with their equivalent dummy variables
data_dframe = function(expandFactors=TRUE, std=TRUE) {
  
  if( expandFactors == TRUE) {
    
    #standardize them
    if( std == TRUE ) {
      
      xtrain = xen.train.std
      xtrainy = xen.train.y.std
      xvalid = xen.valid.std
      xvalidy = xen.valid.y.std
      xtest = xen.test.std
      
    } else {
      
      xtrain = xen.train
      xtrainy = xen.train.y
      xvalid = xen.valid
      xvalidy = xen.valid.y
      xtest = xen.test
    }
    
    
  } else {
    
    #standardize them
    if( std == TRUE ) {
      
      xtrain = xfn.train.std
      xtrainy = xfn.train.y.std
      xvalid = xfn.valid.std
      xvalidy = xfn.valid.y.std
      xtest = xfn.test.std
      
    } else {
      
      xtrain = xfn.train
      xtrainy = xfn.train.y
      xvalid = xfn.valid
      xvalidy = xfn.valid.y
      xtest = xfn.test
      
    }
  }
  
  #attach the y variables
  xtrain = data.frame(xtrain,donr=c.train)
  xtrainy = data.frame(xtrainy,damt=y.train)
  xvalid = data.frame(xvalid,donr=c.valid)
  xvalidy = data.frame(xvalidy,damt=y.valid)
  xtest = as.data.frame(xtest)
  
  #make them global so they can be used
  #considered bad programming practice

  assign("xy.train", xtrain, envir=.GlobalEnv)
  assign("xy.train.y", xtrainy, envir=.GlobalEnv)
  assign("xy.valid", xvalid, envir=.GlobalEnv)
  assign("xy.valid.y", xvalidy, envir=.GlobalEnv)
  assign("xx.test", xtest, envir=.GlobalEnv)
}


#remove values that have log_ equivalents
data_rm_dups = function(dframe,keep) {

  #default to return original data set
  x = dframe
  z = NULL
  
  #find the columns that match
  ii = grep(keep, colnames(dframe))
  if( length(ii) > 0 ) {
    
    #list of names that I want to keep
    kpnames = colnames(subset(dframe,select=ii))

    #candidate list of columns to remove
    rmnames = colnames(subset(dframe,select=-ii))

    rmcol = function(name) {
      i = grep(name,kpnames)
      j = NA
      if( length(i) > 0) {
        j = indexOfColumn(dframe,name)
      }
      return(j)
    }
    
    z = sapply(rmnames, function(r) rmcol(r) )
    z  = z[!is.na(z)]
    x = subset(dframe,select=-z)
  }
  return(x)
}
