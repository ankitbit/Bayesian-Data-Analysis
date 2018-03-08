####################################################################
################    SESSIÓ  2   ####################################
####################################################################


#################### 2.2  ASMA   #################################################


# prior distribution

prior <- c(a = 1 , b =  1)


par(mfrow=c(1,1))


plot(function(x)dbeta(x, prior[1],prior[2]), xlim=c(0,1), ylab="", xlab = "theta")
curve(dbeta(x, prior[1],prior[2]), xlim=c(0,1), ylab="", xlab = "theta", n=10000)
 title(paste("Prior: Beta","(","a=",prior[1],",","b=",prior[2],")"))

prior <- c(a = 1.5 , b = 28.5 )
curve(dbeta(x, prior[1],prior[2]), xlim=c(0,1), ylab="", xlab = "theta", n=10000)
title(paste("Prior: Beta","(","a=",prior[1],",","b=",prior[2],")"))
 
# prior predictive distribution

M <- 1000000
simulated_values <- numeric(M)

th.prior <- rbeta(M, prior[1], prior[2])
#sample size of 50
pre.prior <- rbinom(M, 50, th.prior)

th.posterior <- rbeta(M, posterior[1], posterior[2])
pre.posterior <- rbinom(M, 50, th.posterior)
table(pre.prior)

plot(table(pre.prior)/M, type = "h", xlim= c(0,50), col="skyblue")

#pre.prior <- rbeta(M, 1.25, 19)
#plot(table(pre.prior)/M, type = "l")
# data

N <- 200
y <- 11



# likelihood

curve(dbinom(y, N, x),ylab="",xlab=expression(theta), xlim=c(0,1), n=10000)
 abline(v=y/N, lty=2, col="blue")

K <- integrate(function(th)dbinom(y,N,th), lower=0, upper=1)$value

curve(dbeta(x, prior[1], prior[2]), xlim=c(0,1), ylab="", xlab =expression(theta), ylim=c(0,25), n=10000)
curve(dbinom(y, N, x)/K, add=T, lty=2)
 legend("topright", c("prior","likelihood"),lty=c(1,2))
 title("prior & likelihood")

# the straight line corresponds to the uniform prior in the plot


# posterior distribution


posterior <- c(a = prior[1] + y, b = prior[2] +N -y)



# DIBUIX DE LA DISTRIBU DISTRIBUCIO A PRIORI, A POSTERIORI I LA VERSEMBLANÇA

curve(dbeta(x, posterior[1], posterior[2]), xlim=c(0,1), ylab="", xlab =expression(theta), n=10000)
 curve(dbinom(y, N, x)/K, add=T, lty=3, n=10000)
 curve(dbeta(x, prior[1], prior[2]), add=T, lty=2, n=10000)

 legend("topright", c("prior","posterior","likelihood"), lty = c(2,1,3))
 title("prior , posterior & likelihood")



# summnary

sortida <- matrix(nrow = 7, ncol = 2)

colnames(sortida) <- c('prior', 'posterior')
rownames(sortida) <- c('alpha', 'beta', 'mean', 'variance', '2,5%', 'median', '97.5%')

sortida[1:2, 1] <- prior
sortida[3, 1] <- prior[1]/(prior[1] + prior[2])
sortida[4, 1] <- (prior[1]*prior[2])/(((prior[1]+prior[2])^2)*(prior[1]+prior[2]+1))
sortida[5, 1] <- qbeta(0.025, prior[1], prior[2])
sortida[6, 1] <- qbeta(0.5, prior[1], prior[2])
sortida[7, 1] <- qbeta(0.975, prior[1], prior[2])

sortida[1:2, 2] <- posterior
sortida[3, 2] <- posterior[1]/(posterior[1] + posterior[2])
sortida[4, 2] <- (posterior[1]*posterior[2])/(((posterior[1]+posterior[2])^2)*(posterior[1]+posterior[2]+1))
sortida[5, 2] <- qbeta(0.025, posterior[1], posterior[2])
sortida[6, 2] <- qbeta(0.5, posterior[1], posterior[2])
sortida[7, 2] <- qbeta(0.975, posterior[1], posterior[2])


round(sortida, 3)




# prior and posterior predictive distribution

plot(table(pre.posterior)/M, type="h", xlim= c(0,25))


