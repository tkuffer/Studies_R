install.packages("readstata13")
install.packages("Matching")
library(readstata13)
library(Matching)
data1=read_dta("R/Ichino et al.dta")
summary(data1)
head(data1)
dim(data1)

###############
#  Ex 1.      #
###############
sum(data1$treat)
#229 individuals are treated (Temporary employed)

#part of male
table(data1$male)
nbmale=table(data1$male)[2]/(table(data1$male)[1]+table(data1$male)[2])
mean(data1$male)
nbmale
#part of female
nbfemale = 1-nbmale
nbfemale

#average age
mean(data1$age)

###############
#  Ex 2.      #
###############
#average age across treatement state
tapply(data1$age, data1$treat, mean)

#part of male across treatement state
tapply(data1$male, data1$treat, mean)

#part of low educated across treatement state
tapply(data1$edu0, data1$treat, mean)

#part of medium educated across treatement state
tapply(data1$edu1, data1$treat, mean)

#part of high educated across treatement state
tapply(data1$edu2, data1$treat, mean)

###############
#  Ex 3.      #
###############

attach(data1)
x <- cbind(male,age,edu1,edu2)
y <- out3
tr <- treat
r1 = Match(Y=y,X=x,Tr=tr, estimand = 'ATT', M=1, BiasAdjust = TRUE, replace=TRUE)
r1$est

###############
#  Ex 4.      #
###############
r2 = Match(Y=y,X=x,Tr=tr, estimand = 'ATT', M=2, BiasAdjust = TRUE, replace=TRUE)
r2$est

###############
#  Ex 5.      #
###############
r5 = Match(Y=y,X=x,Tr=tr, estimand = 'ATT', M=5, BiasAdjust = TRUE, replace=TRUE)
r5$est

summary(r1)
summary(r2)
summary(r5)
###############
#  Ex 6.      #
###############
r5Dist025 = Match(Y=y,X=x,Tr=tr, estimand = 'ATT', M=5, distance.tolerance = 0.25, BiasAdjust = TRUE, replace=TRUE)
r5Dist025$est
summary(r5Dist025)
###############
#  Ex 7.      #
###############
r1Dist005 = Match(Y=y,X=x,Tr=tr, estimand = 'ATT', M=1, distance.tolerance = 0.05, BiasAdjust = TRUE, replace=TRUE)
r1Dist005$est
summary(r1Dist005)




detach(data1)


