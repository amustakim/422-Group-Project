#use this when predictions are probabilities
score_class_valid = function(observedClass, posteriorProb) {
  
  # calculate ordered profit function using average donation = $14.50 and mailing cost = $2
  profit<- cumsum(14.5*observedClass[order(posteriorProb, decreasing=T)]-2)
  plot(profit) # see how profits change as more mailings are made

  n.mail <- which.max(profit) # number of mailings that maximizes profits
  c(n.mail, max(profit)) # report number of mailings and maximum profit
  
  # set cutoff based on n.mail.valid
  cutoff <- sort(posteriorProb, decreasing=T)[n.mail+1]
  
  # mail to everyone above the cutoff
  predictedClass <- ifelse( posteriorProb > cutoff, 1, 0 )
  
  a = table(predictedClass, observedClass) # classification table
  #a
  
  accuracy = (a[1,1]+a[2,2])/sum(a)
  #accuracy #0.8805748
  
  #return some statistics
  x = list()
  x$observedClass = observedClass
  x$predictedClass = predictedClass
  x$posterior = posteriorProb
  x$profit = profit
  x$profit.max = c(n.mail, max(profit))
  x$cutoff = cutoff
  x$matrix = a
  x$accuracy= accuracy
  
  return(x)
}

#use this when the predictions are classes
calc_profit = function(pred,obs) {
  
  cm = table(pred,obs)
 
  p = 12.5*cm[2,2] - 2 * cm[2,1] - 14.5*cm[1,2]
  n = sum(cm[2,])
  return(c(n,p))
}

score_class_test = function( scores.valid, post.test) {
  
  # Oversampling adjustment for calculating number of mailings for test set
  
  n.mail.valid <- x$profit.max[1]
  tr.rate <- .1 # typical response rate is .1
  vr.rate <- .5 # whereas validation response rate is .5
  
  adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
  adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
  adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
  n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set
  
  cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
  chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
  a = table(chat.test)
  
  x = list()
  
  x$chat.test = chat.test
  x$matrix = a
  
  return(x)
}

