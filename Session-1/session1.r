#####################################################################
################    SESSION  1   ####################################
#####################################################################


######	beta distribution  ########



par(mfrow=c(3,3))

for (a in c(0.5,1, 10)) {
	for (b in c(0.5, 1,10)) {

		plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "")
  		  title(paste("Beta","(","a=",a,",","b=",b,")"))
	}
}




######################## smoking rate from Barcelona  ###########################

par(mfrow=c(1,1))

a <- 4
b <- 6

 
plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "", lwd=2, col="yellow")
 title(paste("Beta","(","a=",a,",","b=",b,")"))




################# probability to get head when you throw a coin  ################

par(mfrow=c(1,1))

a <- 100
b <- 100

 
plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "")
 title(paste("Prior distribution: Beta","(","a=",a,",","b=",b,")"))



#######  DATA

N <- 25
y <- 11


####   likelihood function

k <- integrate(function(th)dbinom(y,N,th), lower=0, upper=1)$value

plot(function(th)dbinom(y,N,th)/k, xlim=c(0,1), xlab=expression(theta), main="likelihood function", add=T, lty=2)
 legend("topright", c("prior","likelihood"), lty=c(1,2))
 abline(v=y/N, lty=3)




#######################  height of adult catalan people   ####################


prior1 <- c(m=190, s=1)



#####  prior distribution

plot(function(x)dnorm(x,prior1[1], prior1[2]),xlim=c(120,220), ylab="", xlab = expression(mu),main=paste("Normal(",prior1[1],",",prior1[2],")",sep=""))













