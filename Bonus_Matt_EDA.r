
library(dplyr)
library(ggplot2)
scores = read.csv("Bonus422Grp.csv", header = TRUE)
scores = na.omit(scores)
dim(scores)
sum(is.na(scores))
attach(scores)


scores$Select =as.factor(scores$Select)
str(scores)
head(scores)

# Run panel of boxplots
par(mfrow=c(3,3))
boxplot(Leadership~Select, main="Leadership", col=c("gold", "darkgreen"), notch=TRUE)
boxplot(Deployments~Select, main="Deploy")
boxplot(Awards~Select, main="Awards")
boxplot(Ethos~Select, main="Ethos")
boxplot(ExamScore~Select, main="Exam")
boxplot(PerfMkAvg~Select, main="PMA")
boxplot(AFMS~Select, main="AFMS")
par(mfrow=c(1,1))


# Pairs plots colored by Select
cols <- character(nrow(scores))
cols[] <- "black"
cols[scores$Select == 1] <- "red"
cols[scores$Select == 0] <- "black"
pairs(scores[,2:9],col=cols, pch=1, 
      main="SO Scores Select vs Non-Select")

selectees = scores[Select == 1,]
pairs(selectees[,2:9],col="green", pch=1, 
      main="SO Select AFMS Covariance Matrix \n Cycles 218-226")

nonsel = scores[Select==0,]
pairs(nonsel[,2:9],col="lightpink", pch=1, 
      main="SO NonSelect AFMS Covariance Matrix \n Cycles 218-226")

# Correlation Matrix
correlation_Mat = cor(scores[,2:9])
write.csv(correlation_Mat, "Scores correlation_Mat.csv")

# overlaid histograms
par(mfrow=c(3,3))
sl = scores$Leadership[Select == 1]
nsl = scores$Leadership[Select == 0]
hist(nsl, col=rgb(1,1,0,0.5), main="Leadership \nSelect & Non-Select",
     xlab="Leadership")
hist(sl, col=rgb(0,0,1,0.5), add=T)
legend("topright", col = c(rgb(0,0,1,0.5),rgb(1,1,0,0.5)), 
       legend=c("Select", "Non-Select"), pch=15)
box()

sd = scores$Deployments[Select == 1]
nsd = scores$Deployments[Select == 0]
hist(nsd, col=rgb(1,1,0,0.5), main="Deployments \nSelect & Non-Select", 
     xlab="Deployment Score")
hist(sd, col=rgb(0,0,1,0.5), add=T)
box()

sc = scores$Certifications[Select == 1]
nsc = scores$Certifications[Select == 0]
hist(nsc, col=rgb(1,1,0,0.5), main="Certifications \nSelect & Non-Select", 
     xlab="Certifications Score")
hist(sc, col=rgb(0,0,1,0.5), add=T)
box()

sa = scores$Awards[Select == 1]
nsa = scores$Awards[Select == 0]
hist(nsa, col=rgb(1,1,0,0.5), main="Awards \nSelect & Non-Select", 
     xlab="Awards Score")
hist(sa, col=rgb(0,0,1,0.5), add=T)
box()

se= scores$ExamScore[Select == 1]
nse = scores$ExamScore[Select == 0]
hist(nse, col=rgb(1,1,0,0.5), main="ExamScore \nSelect & Non-Select", 
     xlab="Exam Score")
hist(se, col=rgb(0,0,1,0.5), add=T)
box()

sp= scores$PerfMkAvg[Select == 1]
nsp = scores$PerfMkAvg[Select == 0]
hist(nse, col=rgb(1,1,0,0.5), main="Perf. Mark Avg \nSelect & Non-Select", 
     xlab="PMA Score")
hist(se, col=rgb(0,0,1,0.5), add=T)
box()

saf= scores$AFMS[Select == 1]
nsaf = scores$AFMS[Select == 0]
hist(nsaf, col=rgb(1,1,0,0.5), main="AFMS Score \nSelect & Non-Select", 
     xlab="AFMS Score")
hist(saf, col=rgb(0,0,1,0.5), add=T)
box()

par(mfrow=c(1,1))

################################################################################
# Analysis
################################################################################


glm.fit=glm(Select~Leadership+Deployments+Awards+
            Certifications+ExamScore+PerfMkAvg, 
            family = binomial, data= scores)
summary(glm.fit)





