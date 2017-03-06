source("data.r")

#a full data set
head(data.train.std.c)

#make a matrix for ridge/lasso that only uses some of the variables
data_matrix( ~ reg1+reg2 + chld + log_avhv  )

#that was hardcoded to make variables named xx.
head(xx.train)
head(xx.train.y)
head(xx.valid)
head(xx.valid.y)
head(xx.test)

#don't expand them
data_matrix( ~ reg1+reg2 + chld + log_avhv, expandFactors=FALSE  )

#that was hardcoded to make variables named xx.
head(xx.train)
head(xx.train.y)
head(xx.valid)
head(xx.valid.y)
head(xx.test)
