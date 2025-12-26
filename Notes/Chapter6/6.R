##Best Subset
X11=X1*X2
X <- cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10, X11)


Model=regsubsets(as.matrix(X),Y, nvmax = 11)


summary(Model)

SUM=summary(Model)
names(SUM)
Rsq=SUM$rsq
CP=SUM$cp
AdRsq=SUM$adjr2
BIC=SUM$bic
RSS=SUM$rss

Model2=regsubsets(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data=EXECSAL2, nvmax = 10)
summary(Model2)
###Plot
par(mfrow=c(2,2))
plot(1:11,Rsq,xlab="Subset Size",ylab="Adjusted R-squared", ylim=c(0.6,1), pch=19, col="blue")
plot(1:11,Rsq,xlab="Subset Size",ylab="R-squared", ylim=c(0.6,1), pch=19, col="blue")
plot(1:11,CP,xlab="Subset Size",ylab="CP", ylim=c(0, 350), pch=19, col="blue")
p=c(1:11)
lines(y=p+1,x=p, col="red")
plot(1:11,PRESS,xlab="Subset Size",ylab="PRESS", ylim=c(0.5,2.5), pch=19, col="blue")

##stepwise 
##Model with All variables
M1=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11)
summary(M1)
# stepwise regression
step(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11),direction="both")
cbind(SUM$which,round(cbind(Rsq,AdRsq,CP,BIC,RSS,AIC),3))