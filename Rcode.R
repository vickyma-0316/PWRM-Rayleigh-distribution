# -------------------------------------------
# code: functions
# -------------------------------------------

# -------------------------------------------
# weighted median 
# -------------------------------------------
weighted.median <- function(x, w, interpolation=0.5) { 
  # Preparation
  if (missing(w)) w = rep(1L,length(x))
  if (length(w) != length(x)) stop("'x' and 'w' must have the same length")
  
  x = as.double(as.vector(x))
  w = as.double(as.vector(w))
  ok= complete.cases(x,w); x=x[ok]; w=w[ok]
  
  stopifnot(all(w >= 0))
  if(all(w <= 0)) stop("All weights are zero", call.=FALSE)
  
  orderx = order(x)
  x = x[orderx]
  w = w[orderx] / sum(w)
  Fn = cumsum(w)
  tiny = sqrt(.Machine$double.eps)
  
  # Main part
  if ( all( abs(Fn-0.5)>tiny ) ) {  # any values of Fn is not 1/2.
    k = sum( Fn < 0.5 )
    return( x[k+1] )
  } else {
    k = which.min ( signif(abs(Fn-0.5),digits=12) ) # Find k with Fn=0.5
    if (w[k+1] < tiny) {   # check if w[k+1] == 0 
      return( x[k+1] )
    } else {
      return( (1-interpolation)*x[k] + interpolation*x[k+1] )
    }
  }
}


# -----------------------------------------------
# power weighted repeated median
# -----------------------------------------------
Repeated.W.Median <- function (x,y, weight=TRUE, power=p) {
  n = length(x)
  dx = outer(x,x,"-"); dy = outer(y,y,"-");
  slope1 = dy/dx   #(yi-yj)/(xi-xj)
  weight1 = abs(dx)^power 
  slope2  = matrix( t(slope1)[lower.tri(slope1)  |upper.tri(slope1) ], byrow=TRUE, nrow=n)
  weight2 = matrix( t(weight1)[lower.tri(weight1)|upper.tri(weight1)], byrow=TRUE, nrow=n)
  tmp = numeric(n)
  if (weight) { 
    for ( j in 1L:n ) tmp[j] = weighted.median(slope2[j,], w=weight2[j,] )
  } else {
    for ( j in 1L:n ) tmp[j] = median(slope2[j,])
  }
  slope = median(tmp)
  intercept = median(y-slope*x)
  return(c(intercept,slope))
}

# -------------------------------------------
# MSE fn
# -------------------------------------------
#x is lambda, y is mu

MSE.gen = function(x,y, mux=lambda, muy=mu) {
  N = length(x)
  a11 = sum( (x-mux)^2 )
  a22 = sum( (y-muy)^2 )
  a12 = sum( (x-mux)*(y-muy) )
  S = 1/N * matrix( c(a11,a12,a12,a22), nrow=2)
  det(S)
}


# -------------------------------------------
# Pars estimate from LOOCV
# -------------------------------------------
##1-Complete case
##calculate generalized variance with given p value

loocv_params1 <- function(x, lambda=0.038, mu=4.291, p) {
  n <- length(x)
  lambda_est <- numeric(n)
  mu_est <- numeric(n)
  m <- (n-1)
  pi1<-rep(0,m)
  for(j in 1:m){
    if(m<=10){
      pi1[j]<-(j-3/8)/(m+0.25)
    }
    else{
      pi1[j]<-(j-0.5)/m
    }
  }
  pi <- sqrt(-log(1-pi1))
  y1 <- pi[1:m]
  
  for (i in 1:n) {
    # delete the i-th observation
    train_set <- x[-i]
    x1 <- sort(train_set)
    # estimate pars with the train_set
    pars_train <- Repeated.W.Median(x1, y1, weight=TRUE, power=p)
    lambda_est[i] <- pars_train[1]
    mu_est[i] <- pars_train[2]
  }
  ans <- MSE.gen (x=lambda_est, y=mu_est, mux=lambda, muy=mu)
  return(ans)
}

##2-Complete case with extreme large outlier: delta=max{xi}+4*sigma
##calculate generalized variance with given p value
loocv_params2 <- function(x, lambda=0.038, mu=4.291, p) {
  n <- length(x)
  lambda_est <- numeric(n)
  mu_est <- numeric(n)
  order_sample <- sort(x)
  delta <- order_sample[n]+4*(sd(x))
  new_sample <- replace(x, 1, delta)
  m <- (n-1)
  pi1<-rep(0,m)
  for(j in 1:m){
    if(m<=10){
      pi1[j]<-(j-3/8)/(m+0.25)
    }
    else{
      pi1[j]<-(j-0.5)/m
    }
  }
  pi <- sqrt(-log(1-pi1))
  y1 <- pi[1:m]
  
  for (i in 1:n) {
    train_set <- new_sample[-i]
    x1 <- sort(train_set)
    pars_train <- Repeated.W.Median(x1, y1, weight=TRUE, power=p)
    lambda_est[i] <- pars_train[1]
    mu_est[i] <- pars_train[2]
  }
  ans <- MSE.gen (x=lambda_est, y=mu_est, mux=lambda, muy=mu)
  return(ans)
}

##3-Complete case with extreme small outlier01: delta=mu+0.05
##calculate generalized variance with given p value
loocv_params3 <- function(x, lambda=0.038, mu=4.291, p) {
  n <- length(x)
  lambda_est <- numeric(n)
  mu_est <- numeric(n)
  order_sample <- sort(x)
  delta <- mu+0.05
  new_sample <- replace(x, 1, delta)
  m <- (n-1)
  pi1<-rep(0,m)
  for(j in 1:m){
    if(m<=10){
      pi1[j]<-(j-3/8)/(m+0.25)
    }
    else{
      pi1[j]<-(j-0.5)/m
    }
  }
  pi <- sqrt(-log(1-pi1))
  y1 <- pi[1:m]
  
  for (i in 1:n) {
    train_set <- new_sample[-i]
    x1 <- sort(train_set)
    pars_train <- Repeated.W.Median(x1, y1, weight=TRUE, power=p)
    lambda_est[i] <- pars_train[1]
    mu_est[i] <- pars_train[2]
  }
  ans <- MSE.gen (x=lambda_est, y=mu_est, mux=lambda, muy=mu)
  return(ans)
}

##4-Complete case with extreme small outlier02: compare (mu+0.05) and outlier from left shifting 
##calculate generalized variance with given p value
loocv_params4 <- function(x, lambda, mu, p) {
  n <- length(x)
  lambda_est <- numeric(n)
  mu_est <- numeric(n)
  order_sample <- sort(x)
  left_outlier <- order_sample[1]-4*(sd(x))
  if(left_outlier>mu){
    delta<- min(left_outlier, mu+0.05)
  }else{
    delta <- (mu+0.05)
  }
  new_sample <- replace(x, 1, delta)
  m <- (n-1)
  pi1<-rep(0,m)
  for(j in 1:m){
    if(m<=10){
      pi1[j]<-(j-3/8)/(m+0.25)
    }
    else{
      pi1[j]<-(j-0.5)/m
    }
  }
  pi <- sqrt(-log(1-pi1))
  y1 <- pi[1:m]
  
  for (i in 1:n) {
    train_set <- new_sample[-i]
    x1 <- sort(train_set)
    pars_train <- Repeated.W.Median(x1, y1, weight=TRUE, power=p)
    lambda_est[i] <- pars_train[1]
    mu_est[i] <- pars_train[2]
  }
  ans <- MSE.gen (x=lambda_est, y=mu_est, mux=lambda, muy=mu)
  return(ans)
}

##For p=0(0.1)5, calculate generalized variance"Complete Case for Rayleigh"
VAR.gen1 <- function(sample, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1) {
  n <- (b-a)/deltax
  var_est <- numeric(n)
  p <- numeric(n+1)
  for (i in 1:(n+1)) {
    p[i] <- a+deltax*(i-1)
    var_est[i] <- loocv_params1(sample, lambda, mu, p[i])
  }
  ans <- var_est
}


##For p=0(0.1)5, calculate generalized variance"Complete Case with Large Outlier"
VAR.gen2 <- function(sample, lambda, mu, a=0, b=5, deltax=0.1) {
  n <- (b-a)/deltax
  var_est <- numeric(n)
  p <- numeric(n+1)
  for (i in 1:(n+1)) {
    p[i] <- a+deltax*(i-1)
    var_est[i] <- loocv_params2(sample, lambda, mu, p[i])
  }
  ans <- var_est
}


##For p=0(0.1)5, calculate generalized variance"Complete Case with Small Outlier"
VAR.gen3 <- function(sample, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1)  {
  n <- (b-a)/deltax
  var_est <- numeric(n)
  p <- numeric(n+1)
  for (i in 1:(n+1)) {
    p[i] <- a+deltax*(i-1)
    ##for small outlier, choose fn loocv_params3 or loocv_params4
    var_est[i] <- loocv_params3(sample, lambda, mu, p[i])
  }
  ans <- var_est
}

##For p=0(0.1)5, calculate generalized variance"Censored Case for Rayleigh"
VAR.gen4 <- function(sample, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1) {
  n <- (b-a)/deltax
  var_est <- numeric(n)
  p <- numeric(n+1)
  for (i in 1:(n+1)) {
    p[i] <- a+deltax*(i-1)
    var_est[i] <- loocv_params1(sample, lambda, mu, p[i])
  }
  ans <- var_est
}


##For p=0(0.1)5, calculate generalized variance"Censored Case with Large Outlier"
VAR.gen5 <- function(sample, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1) {
  n <- (b-a)/deltax
  var_est <- numeric(n)
  p <- numeric(n+1)
  for (i in 1:(n+1)) {
    p[i] <- a+deltax*(i-1)
    var_est[i] <- loocv_params2(sample, lambda, mu, p[i])
  }
  ans <- var_est
}


##For p=0(0.1)5, calculate generalized variance"Censored Case with Small Outlier"
VAR.gen6 <- function(sample, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1) {
  n <- (b-a)/deltax
  var_est <- numeric(n)
  p <- numeric(n+1)
  for (i in 1:(n+1)) {
    p[i] <- a+deltax*(i-1)
    var_est[i] <- loocv_params3(sample, lambda, mu, p[i])
  }
  ans <- var_est
}

##Lower and upper breakdown point
RM.breakdown.point <- function(x, power) {
  x = sort(x)
  n = length(x)
  dx = outer(x,x,"-");
  weight1 = abs(dx)^power 
  weight2 = matrix( t(weight1)[lower.tri(weight1)|upper.tri(weight1)], byrow=TRUE, nrow=n)
  WU = t( apply(weight2, 1, sort) ) 
  WL = t( apply(weight2, 1, sort, decreasing=TRUE) )
  BU = numeric(n)  # Breakdown (Upper)
  BL = numeric(n)  # Breakdown (Lower)
  for ( i in seq_len(n) ) {   
    BU[i] = sum( cumsum(WU[i,])/sum(WU[i,]) < 0.5 )
    BL[i] = sum( cumsum(WL[i,])/sum(WL[i,]) < 0.5 )
  }
  return(list(lower=min(BL)/(n-1), upper=max(BU)/(n-1)))
}


# --------------------------------------------------------------
# code2 for figures 6-7: G.V. from LOOCV
# --------------------------------------------------------------
# Figs 6(a), 6(c), 7(a) and 7(c)

sample <- c(6.96,9.30,6.96,7.24,9.30,4.9,8.42,6.05,10.18,6.82,8.58,7.77,11.94,11.25,12.94,12.94)
lambda=0.038
mu=4.291

##p=0(0.1)5: "Complete Case for Rayleigh"
##calculate generalized variance and plot
y1 <- VAR.gen1(sample, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1)
plot(x=seq(0,5,by=0.1), y1, type="l", xlab=expression("p"),ylab="Generalized variance")
#plot(x=seq(0,5,by=0.1), y1, type="l", xlab=expression("p"),ylab="Generalized variance", main="Complete data")

##p=0(0.1)5: "Complete Case with Large Outlier"
##calculate generalized variance and plot
y2 <- VAR.gen2(sample, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1)
plot(x=seq(0,5,by=0.1), y2, type="l", xlab=expression("p"),ylab="Generalized variance")
#plot(x=seq(0,5,by=0.1), y2, type="l", xlab=expression("p"),ylab="Generalized variance", main="Complete data with contamination")

##p=0(0.1)5: "Complete Case with Small Outlier"
##calculate generalized variance and plot
y3 <- VAR.gen3(sample, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1)
plot(x=seq(0,5,by=0.1), y3, type="l", xlab="p",ylab="Generalized variance", main="Complete Case with Small Outlier")

##generate Type II censored sample: r=14 out of n=16, (1-14/16)=12.5% observations censored
order_samp <- sort(sample)
cens_samp <- order_samp[1:14]

##p=0(0.1)5: "Censored Case for Rayleigh"
##calculate generalized variance and plot
y4 <- VAR.gen4(cens_samp, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1) 
#plot(x=seq(0,5,by=0.1), y4, type="l", xlab="p",ylab="Generalized variance", main="Censored Case for Rayleigh")
plot(x=seq(0,5,by=0.1), y4, type="l", xlab="p",ylab="Generalized variance")


##p=0(0.1)5: "Censored Case with Large Outlier"
##calculate generalized variance and plot
y5 <- VAR.gen5(cens_samp, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1) 
#plot(x=seq(0,5,by=0.1), y5, type="l", xlab="p",ylab="Generalized variance", main="Censored Case with Large Outlier")
plot(x=seq(0,5,by=0.1), y5, type="l", xlab="p",ylab="Generalized variance")


##p=0(0.1)5: "Censored Case with Small Outlier"
##calculate generalized variance and plot
y6 <- VAR.gen6(cens_samp, lambda=0.038, mu=4.291, a=0, b=5, deltax=0.1) 
plot(x=seq(0,5,by=0.1), y6, type="l", xlab="p",ylab="Generalized variance", main="Censored Case with Small Outlier")


# --------------------------------------------------------------
# code3 for figures 6-7: Lower and upper breakdown pts
# --------------------------------------------------------------
# Figs 6(b), 6(d), 7(b) and 7(d)

##p=0(0.1)5: calculate lower and upper breakdown point
Lower.Upper.bound <- function(sample, a=0, b=5, deltax=0.1) {
  n <- (b-a)/deltax
  bound_est <- list()
  Lowerbound_est <- numeric(n)
  Upperbound_est <- numeric(n)
  p <- numeric(n+1)
  for (i in 1:(n+1)) {
    p[i] <- a+deltax*(i-1)
    bound_est <- RM.breakdown.point(x=sample, power=p[i])
    Lowerbound_est[i] <- bound_est$lower
    Upperbound_est[i] <- bound_est$upper
  }
  ans <- data.frame(Lowerbound_est, Upperbound_est)
  names(ans) <- c("Lowerbound", "Upperbound")
  return(ans)
}

##complete sample
sample <- c(6.96,9.30,6.96,7.24,9.30,4.9,8.42,6.05,10.18,6.82,8.58,7.77,11.94,11.25,12.94,12.94)

##(1) p=0(0.1)5: "Complete Case for Rayleigh"
##calculate lower and upper breakdown points and plot
y01 <- Lower.Upper.bound(sample, a=0, b=5, deltax=0.1)
y11 <- y01$Lowerbound
y21 <- y01$Upperbound
#plot(x=seq(0,5,by=0.1), y11, col="red", type="l", ylim = range(c(y11, y21)), xlab="p",ylab="Lower and upper breakdown points", main="Complete Case for Rayleigh")
#lines(x=seq(0,5,by=0.1), y21, col="blue", type="l")

plot(x=seq(0,5,by=0.1), y11, col="red", type="l", ylim = range(c(y11, y21)), xlab="p",ylab="Lower and upper breakdown points")
lines(x=seq(0,5,by=0.1), y21, col="blue", type="l")

##(2) p=0(0.1)5: "Complete Case with Large Outlier"
## large outlier generated by 4*sigma shifting
sample <- c(6.96,9.30,6.96,7.24,9.30,4.9,8.42,6.05,10.18,6.82,8.58,7.77,11.94,11.25,12.94,12.94)
n <- length(sample)
order_samp <- sort(sample)
delta2 <- order_samp[n]+4*(sd(sample))
sample2 <- replace(sample, 1, delta2)
##calculate lower and upper breakdown points and plot
y02 <- Lower.Upper.bound(sample2, a=0, b=5, deltax=0.1)
y12 <- y02$Lowerbound
y22 <- y02$Upperbound
#plot(x=seq(0,5,by=0.1), y12, col="red", type="l", ylim = range(c(y12, y22)), xlab="p",ylab="Lower and upper breakdown points", main="Complete Case with Large Outlier")
#lines(x=seq(0,5,by=0.1), y22, col="blue", type="l")
plot(x=seq(0,5,by=0.1), y12, col="red", type="l", ylim = range(c(y12, y22)), xlab="p",ylab="Lower and upper breakdown points")
lines(x=seq(0,5,by=0.1), y22, col="blue", type="l")


##(3) p=0(0.1)5: "Complete Case with Small Outlier"
## small outlier replaced by (mu+0.05)
mu <- 4.291
delta3 <- (mu+0.05)
sample3 <- replace(sample, 1, delta3)
##calculate lower and upper breakdown points and plot
y03 <- Lower.Upper.bound(sample3, a=0, b=5, deltax=0.1)
y13 <- y03$Lowerbound
y23 <- y03$Upperbound
plot(x=seq(0,5,by=0.1), y13, col="red", type="l", ylim = range(c(y13, y23)), xlab="p",ylab="Lower and upper breakdown points", main="Complete Case with Small Outlier")
lines(x=seq(0,5,by=0.1), y23, col="blue", type="l")

##generate Type II censored sample: r=14 out of n=16, 20% observations censored
sample <- c(6.96,9.30,6.96,7.24,9.30,4.9,8.42,6.05,10.18,6.82,8.58,7.77,11.94,11.25,12.94,12.94)
n <- 16
r <- 14
order_samp <- sort(sample)
cens_samp <- order_samp[1:r]

##(4) p=0(0.1)5: "Censored Case for Rayleigh"
##calculate lower and upper breakdown points and plot
y04 <- Lower.Upper.bound(cens_samp, a=0, b=5, deltax=0.1)
y14 <- y04$Lowerbound
y24 <- y04$Upperbound
#plot(x=seq(0,5,by=0.1), y14, col="red", type="l", ylim = range(c(y14, y24)), xlab="p",ylab="Lower and upper breakdown points", main="Censored Case for Rayleigh")
#lines(x=seq(0,5,by=0.1), y24, col="blue", type="l")
plot(x=seq(0,5,by=0.1), y14, col="red", type="l", ylim = range(c(y14, y24)), xlab="p",ylab="Lower and upper breakdown points")
lines(x=seq(0,5,by=0.1), y24, col="blue", type="l")


##(5) p=0(0.1)5: "Censored Case with Large Outlier"
## large outlier generated by 4*sigma shifting
delta5 <- order_samp[n]+4*(sd(sample))
sample5 <- replace(cens_samp, 1, delta5)
##calculate lower and upper breakdown points and plot
y05 <- Lower.Upper.bound(sample5, a=0, b=5, deltax=0.1)
y15 <- y05$Lowerbound
y25 <- y05$Upperbound
plot(x=seq(0,5,by=0.1), y15, col="red", type="l", ylim = range(c(y15, y25)), xlab="p",ylab="Lower and upper breakdown points")
lines(x=seq(0,5,by=0.1), y25, col="blue", type="l")
#plot(x=seq(0,5,by=0.1), y15, col="red", type="l", ylim = range(c(y15, y25)), xlab="p",ylab="Lower and upper breakdown points", main="Censored Case with Large Outlier")
#lines(x=seq(0,5,by=0.1), y25, col="blue", type="l")

##(6) p=0(0.1)5: "Censored Case with Small Outlier"
## small outlier replaced by (mu+0.05)
mu <- 4.291
delta6 <- (mu+0.05)
sample6 <- replace(cens_samp, 1, delta6)
##calculate lower and upper breakdown points and plot
y06 <- Lower.Upper.bound(sample6, a=0, b=5, deltax=0.1)
y16 <- y06$Lowerbound
y26 <- y06$Upperbound
plot(x=seq(0,5,by=0.1), y16, col="red", type="l", ylim = range(c(y16, y26)), xlab="p",ylab="Lower and upper breakdown points", main="Censored Case with Small Outlier")
lines(x=seq(0,5,by=0.1), y26, col="blue", type="l")


# -------------------------------------------
# code0: ks.test for real data
# -------------------------------------------
#pdf of two parameter Rayleigh distribution
rayleigh_pdf <- function(x, mu, lambda) {
  2*lambda*(x-mu)*exp(-lambda* (x-mu)^2)
}

# Kolmogorov-Smirnov test
# ks.test_result <- ks.test(data, "p", fun = rayleigh_cdf)
data <- c(6.96, 9.30, 6.96, 7.24, 9.30, 4.9, 8.42, 6.05, 10.18, 6.82, 8.58, 7.77, 11.94, 11.25, 12.94, 12.94)
ks.test_result <- ks.test(data, "rayleigh_pdf", 4.291, 0.038)
print(ks.test_result)

##alternative method to provide data followed by two parameter Rayleigh distribution
rray.twopar <- function(n, lambda, mu)  
{
  set.seed(123)
  u <- runif(n, min=0, max=1)
  x <- mu+sqrt(-1/lambda*log(1-u))
  return(x)
}

x1 <- c(6.96,9.30,6.96,7.24,9.30,4.9,8.42,6.05,10.18,6.82,8.58,7.77,11.94,11.25,12.94,12.94)
n1 <- length(x1)
x2 <- rray.twopar(n1,0.03792208,4.29057239)
ks.test(x1,x2)

# -------------------------------------------
# qqplot for real-data 
# -------------------------------------------
x1 <- c(6.96, 9.30, 6.96, 7.24, 9.30, 4.9, 8.42, 6.05,
        10.18, 6.82, 8.58, 7.77, 11.94, 11.25, 12.94, 12.94)

rayleigh_pdf <- function(x, mu, lambda) {
  y <- ifelse(x >= mu, 2 * lambda * (x - mu) * exp(-lambda * (x - mu)^2), 0)
  y[y <= 0] <- 1e-10  # avoid log(0)
  return(y)
}

rayleigh_cdf <- function(q, mu, lambda) {
  ifelse(q >= mu, 1 - exp(-lambda * (q - mu)^2), 0)
}

rayleigh_qf <- function(p, mu, lambda) {
  mu + sqrt(-log(1 - p) / lambda)
}

negloglik_rayleigh <- function(params) {
  mu <- params[1]
  lambda <- params[2]
  if (lambda <= 0) return(Inf)
  -sum(log(rayleigh_pdf(x1, mu, lambda)))
}


fit <- optim(par = c(mu = (min(x1) - 0.01), lambda = 0.1),
             fn = negloglik_rayleigh,
             method = "L-BFGS-B",
             lower = c(mu = -Inf, lambda = 1e-6))

mu_hat <- fit$par["mu"]
lambda_hat <- fit$par["lambda"]

# QQ plot
par(mar = c(5, 6, 4, 2)) 
qqplot(rayleigh_qf(ppoints(length(x1)), mu_hat, lambda_hat),
       sort(x1), cex.lab = 1.5, cex.axis = 1.8,
       main = "",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch = 19)
abline(0, 1, col = "red", lty = 2, lwd = 3)






