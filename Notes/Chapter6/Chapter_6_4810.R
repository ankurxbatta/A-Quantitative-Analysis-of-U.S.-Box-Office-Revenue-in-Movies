EXECSAL2 <- read.delim("~/Desktop/Langara/DANA 4810/Nooshin/Chapters/Ch6/R/EXECSAL2.txt")

attach(EXECSAL2)
head(EXECSAL2)
install.packages("leaps")
library(leaps)
library(MASS)

##Model with All variables
M1=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10)
summary(M1)
# stepwise regression
step(lm(Y~.,data=EXECSAL2),direction="both")

##or using library(MASS)

stepAIC(lm(Y~.,data=EXECSAL2),direction="both")

#Forward selection

mint <- lm(Y~1,data=EXECSAL2)
forwardAIC <- step(mint,scope=list(lower=~1, 
                                   upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10),
                   direction="forward", data=EXECSAL2)

#or

forwardAIC1 <- step(mint,scope=formula(M1),
                   direction="forward", data=EXECSAL2)

forwardAIC1$coefficients
forwardAIC$coefficients
forwardAIC1$anova
forwardAIC$anova

#Backward Elimination

step(lm(Y~.,data=EXECSAL2),direction="backward")
##Best Subset

X <- cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10)


Model=regsubsets(as.matrix(X),Y, nvmax = 10)

#or
Model=regsubsets(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,data=EXECSAL2, nvmax = 10)
summary(Model)
SUM=summary(Model)
names(SUM)
Rsq=SUM$rsq
CP=SUM$cp
AdRsq=SUM$adjr2
BIC=SUM$bic
RSS=SUM$rss
#Calculation of AIC
n <- length(EXECSAL2$Y)
p <- apply(SUM$which, 1, sum)
AIC<- SUM$bic - log(n) * p + 2 * p
#number of independent variables in the models
I=p-1
I
MSE1=RSS/(n-I-1)
MSE1


###Plot
par(mfrow=c(2,2))
plot(p,Rsq,xlab="Subset Size",ylab="Adjusted R-squared", ylim=c(0.6,1), pch=19, col="blue")
plot(p,Rsq,xlab="Subset Size",ylab="R-squared", ylim=c(0.6,1), pch=19, col="blue")
plot(p,CP,xlab="Subset Size",ylab="CP", ylim=c(0, 350), pch=19, col="blue")
lines(y=p+1,x=p, col="red")
plot(p,PRESS,xlab="Subset Size",ylab="PRESS", ylim=c(0.5,2.5), pch=19, col="blue")

##PRESS


m1=lm(Y~X1)
s1=summary(m1)
m2=lm(Y~X1+X3)
s2=summary(m2)
m3=lm(Y~X1+X3+X4)
s3=summary(m3)
m4=lm(Y~X1+X2+X3+X4)
s4=summary(m4)
m5=lm(Y~X1+X2+X3+X4+X5)
s5=summary(m5)
m6=lm(Y~X1+X2+X3+X4+X5+X9)
s6=summary(m6)
m7=lm(Y~X1+X2+X3+X4+X5+X6+X9)
s7=summary(m7)
m8=lm(Y~X1+X2+X3+X4+X5+X6+X8+X9)
s8=summary(m8)
m9=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9)
s9=summary(m9)
m10=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10)
s10=summary(m10)

library(qpcR)
n1=qpcR::PRESS(m1)
a1=n1$stat
b1=s1$sigma
n2=qpcR::PRESS(m2)
a2=n2$stat
b2=s2$sigma
n3=qpcR::PRESS(m3)
a3=n3$stat
b3=s3$sigma
n4=qpcR::PRESS(m4)
a4=n4$stat
b4=s4$sigma
n5=qpcR::PRESS(m5)
a5=n5$stat
b5=s5$sigma
n6=qpcR::PRESS(m6)
a6=n6$stat
b6=s6$sigma
n7=qpcR::PRESS(m7)
a7=n7$stat
b7=s7$sigma
n8=qpcR::PRESS(m8)
a8=n8$stat
b8=s8$sigma
n9=qpcR::PRESS(m9)
a9=n9$stat
b9=s9$sigma
n10=qpcR::PRESS(m10)
a10=n10$stat
b10=s10$sigma
PRESS=c(a1,a2,a3,a4,a5,a6,a7,a8,a9, a10)
MSE=(c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10))^2
##Result
cbind(SUM$which,round(cbind(Rsq,AdRsq,CP,BIC,RSS,AIC, PRESS, MSE,MSE1),4)) 
