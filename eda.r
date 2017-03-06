#This file does EDA for the charity project
source("functions.r")
source("data.r")


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
#image_on = TRUE

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
mbpairs(data.train[numerics.range.charity.t], c.train)
doneimage()

#pairs of numerics vs donr
saveimage("pair_factors")
mbpairs(data.train[factors.range.charity.t], c.train)
doneimage()

#### EDA  #####


## factor variables

#some regions not significant
zz=eda_donr(data.train,"reg")
saveimage("region")
zz=eda_damt(data.train.y,"reg")
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

### do some eda on factor variables, but treat them as numerics
### create dataset with factors as numerics
data_dframe(expandFactors=FALSE,std=FALSE)

saveimage("nregion_donr")
zz=eda_donr(xy.train,"reg")
doneimage()
saveimage("nregion_damt")
zz=eda_damt(xy.train.y,"reg")
doneimage()

saveimage("nhome_donr")
zz=eda_donr(xy.train,"home")
doneimage()
saveimage("nhome_damt")
zz=eda_damt(xy.train.y,"home")
doneimage()

saveimage("nchld_donr")
zz=eda_donr(xy.train,"chld")
doneimage()
saveimage("nchld_damt")
zz=eda_damt(xy.train.y,"chld")
doneimage()

saveimage("nhinc_donr")
zz=eda_donr(xy.train,"hinc")
doneimage()
saveimage("nhinc_damt")
zz=eda_damt(xy.train.y,"hinc")
doneimage()

xy.train$hinc_sqr = (xy.train$hinc)^2
xy.train.y$hinc_sqr = (xy.train.y$hinc)^2
saveimage("shinc_donr")
zz=eda_donr(xy.train,"hinc_sqr")
doneimage()
saveimage("shinc_damt")
zz=eda_damt(xy.train.y,"hinc_sqr")
doneimage()

#not significant
saveimage("ngenf_donr")
zz=eda_donr(xy.train,"genf")
doneimage()
#not signficant
saveimage("ngenf_damt")
zz=eda_damt(xy.train.y,"genf")
doneimage()

saveimage("nwrat_donr")
zz=eda_donr(xy.train,"wrat")
doneimage()
saveimage("nwrat_damt")
zz=eda_damt(xy.train.y,"wrat")
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
zz=eda_donr(data.train,"bin_tdon")
#bins not necessary here?
saveimage("tdon_damt")
zz=eda_damt(data.train.y,"tdon")
doneimage()
saveimage("bin_tdon_damt")
zz=eda_damt(data.train.y,"bin_tdon")
doneimage()

#bin this?
#<8, >= 8?
t(table(data.train$tlag,data.train$donr))
saveimage("tlag")
zz=eda_donr(data.train,"tlag")
doneimage()
zz=eda_donr(data.train,"bin_tlag")
#bins not necessary here?
saveimage("tlag_damt")
zz=eda_damt(data.train.y,"tlag")
doneimage()
saveimage("bin_tlag_damt")
zz=eda_damt(data.train.y,"bin_tlag")
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

#multicolinearity
library(car)

columns = c(xrange.charity.t,donr.charity.t)
#columns = columns[!columns %in% 25:35]
m = glm( donr ~ ., data = data.train[,columns], family=binomial)

#vif is high wit both the the original value and their log transformations
vif(m)

x = data_rm_dups(data.train[,columns],"log_")
m = glm( donr ~ ., data = x, family=binomial)
#fixed it
vif(m)
alias(m)
