rm(list = ls())
setwd("C:/Users/mym358/Desktop")


# -------------------------------------------
# Figure1: complete sample with n=20
# -------------------------------------------
#read ddata1 for no contamination (blue point) and ddata2 for contamination (green cross)
#take the first 300 pts only
par(mfrow = c(2, 2), mar = c(6, 5.5, 3, 2), cex.lab = 1.3)

ddata1 <- read.csv(file="output01b.csv") 
ddata2 <- read.csv(file="output02.csv")

#(a) OLS
y61 <- ddata1$lambda06[1:300]
x61 <- ddata1$mu06[1:300]
y62 <- ddata2$lambda06[1:300]
x62 <- ddata2$mu06[1:300]

plot(x=x61, y61, type="p", col="blue", xlim = range(c(0.5, 2.0)), ylim = range(c(0, 10)), xlab=expression(hat(mu)[ols]), ylab=expression(hat(lambda)[ols]))
mtext("(a) OLS", side = 1, line = 4.5, cex = 1)
lines(x=x62, y62, type="p", col="green", pch=3)
abline(h=2, lty=1, col="orange")
abline(v=1.5, lty=1, col="orange")


#(b) MLE
y51 <- ddata1$lambda05[1:300]
x51 <- ddata1$mu05[1:300]
y52 <- ddata2$lambda05[1:300]
x52 <- ddata2$mu05[1:300]

plot(x=x51, y51, type="p", col="blue", xlim = range(c(0.5, 2.0)), ylim = range(c(0, 10)), xlab=expression(hat(mu)[mle]), ylab=expression(hat(lambda)[mle]))
mtext("(b) MLE", side = 1, line = 4.5, cex = 1)
lines(x=x52, y52, type="p", col="green", pch=3)
abline(h=2, lty=1, col="orange")
abline(v=1.5, lty=1, col="orange")

#(c) M-estimation
y41 <- ddata1$lambda04[1:300]
x41 <- ddata1$mu04[1:300]
y42 <- ddata2$lambda04[1:300]
x42 <- ddata2$mu04[1:300]

plot(x=x41, y41, type="p", col="blue", xlim = range(c(0.5, 2.0)), ylim = range(c(0, 10)), xlab=expression(hat(mu)[m]), ylab=expression(hat(lambda)[m]))
mtext("(c) M-estimation", side = 1, line = 4.5, cex = 1)
lines(x=x42, y42, type="p", col="green", pch=3)
abline(h=2, lty=1, col="orange")
abline(v=1.5, lty=1, col="orange")

#(d) PWRM (p=3/4)
y31 <- ddata1$lambda03[1:300]
x31 <- ddata1$mu03[1:300]
y32 <- ddata2$lambda03[1:300]
x32 <- ddata2$mu03[1:300]

plot(x=x31, y31, type="p", col="blue", xlim = range(c(0.5, 2.0)),ylim = range(c(0, 10)), xlab=expression(hat(mu)[pwrm]), ylab=expression(hat(lambda)[pwrm]))
mtext(expression("(d) PWRM  (" * p == 3/4 * ")"), side = 1, line = 4.5, cex = 1)
lines(x=x32, y32, type="p", col="green", pch=3)
abline(h=2, lty=1, col="orange")
abline(v=1.5, lty=1, col="orange")


# ----------------------------------------------------------
# Figure2 Type II censogreen sample with r=16 out of n=20
# ----------------------------------------------------------
#read ddata3 for no contamination (blue point) and ddata4 for contamination (green cross)
#take the first 300 pts only

ddata3 <- read.csv(file="output07_0.8.csv") 
ddata4 <- read.csv(file="output08_0.8.csv")


#(a) OLS
y61 <- ddata3$lambda06[1:300]
x61 <- ddata3$mu06[1:300]
y62 <- ddata4$lambda06[1:300]
x62 <- ddata4$mu06[1:300]

plot(x=x61, y61, type="p", col="blue", xlim = range(c(0.5, 2.0)), ylim = range(c(0, 10)), xlab=expression(hat(mu)[ols]), ylab=expression(hat(lambda)[ols]))
mtext("(a) OLS", side = 1, line = 4.5, cex = 1)
lines(x=x62, y62, type="p", col="green", pch=3)
abline(h=2, lty=1, col="orange")
abline(v=1.5, lty=1, col="orange")

#(b) MLE
y51 <- ddata3$lambda05[1:300]
x51 <- ddata3$mu05[1:300]
y52 <- ddata4$lambda05[1:300]
x52 <- ddata4$mu05[1:300]

plot(x=x51, y51, type="p", col="blue", xlim = range(c(0.5, 2.0)), ylim = range(c(0, 10)), xlab=expression(hat(mu)[mle]), ylab=expression(hat(lambda)[mle]))
mtext("(b) MLE", side = 1, line = 4.5, cex = 1)
lines(x=x52, y52, type="p", col="green", pch=3)
abline(h=2, lty=1, col="orange")
abline(v=1.5, lty=1, col="orange")

#(c) M-estimation
y41 <- ddata3$lambda04[1:300]
x41 <- ddata3$mu04[1:300]
y42 <- ddata4$lambda04[1:300]
x42 <- ddata4$mu04[1:300]

plot(x=x41, y41, type="p", col="blue", xlim = range(c(0.5, 2.0)), ylim = range(c(0, 10)), xlab=expression(hat(mu)[m]), ylab=expression(hat(lambda)[m]))
mtext("(c) M-estimation", side = 1, line = 4.5, cex = 1)
lines(x=x42, y42, type="p", col="green", pch=3)
abline(h=2, lty=1, col="orange")
abline(v=1.5, lty=1, col="orange")

#(d) PWRM (p=3/4)
y31 <- ddata3$lambda03[1:300]
x31 <- ddata3$mu03[1:300]
y32 <- ddata4$lambda03[1:300]
x32 <- ddata4$mu03[1:300]

plot(x=x31, y31, type="p", col="blue", xlim = range(c(0.5, 2.0)), ylim = range(c(0, 10)), xlab=expression(hat(mu)[pwrm]), ylab=expression(hat(lambda)[pwrm]))
mtext(expression("(d) PWRM  (" * p == 3/4 * ")"), side = 1, line = 4.5, cex = 1)
lines(x=x32, y32, type="p", col="green", pch=3)
abline(h=2, lty=1, col="orange")
abline(v=1.5, lty=1, col="orange")


# ------------------------------------------------------
# Figure3 G.V. and RE for complete sample with n=20
# ------------------------------------------------------
##Comment: Type II censogreen sample tried, but not good
par(mfrow = c(3, 2),mar = c(7.5, 5.5, 3, 2), cex.lab = 1.5, cex.axis = 1.3)

ddata5 <- read.csv(file = "mse_n20.csv")

y1 <- ddata5$mse1
y2 <- ddata5$mse2
y3 <- ddata5$mse3
y4 <- ddata5$mse4
y5 <- ddata5$mse5
y6 <- ddata5$mse6

# Plot 1
plot(x = seq(0, 100, by = 1), y1, col = "blue", type = "l", lwd = 2,
     ylim = range(c(0, 40)), xlab = expression(delta), ylab = "Generalized MSE")
mtext(expression("(a)"), side = 1, line = 6, cex = 1.1)
lines(x = seq(0, 100, by = 1), y3, type = "l", col = "green", lwd = 2)
lines(x = seq(0, 100, by = 1), y4, type = "l", col = "orange", lwd = 2)
lines(x = seq(0, 100, by = 1), y6, type = "l", col = "purple", lwd = 2)

legend("topleft", legend = c("M-estimation", "MLE", "OLS", "PWRM (p=1/4)"),
       col = c("blue", "green", "orange", "purple"), lty = 1:4, cex = 1.1, bty = "n")

# Plot 2
plot(x = seq(0, 100, by = 1), y1, col = "blue", type = "l", lwd = 2,
     ylim = range(c(0, 0.6)), xlab = expression(delta), ylab = "Generalized MSE")
mtext(expression("(b)"), side = 1, line = 6, cex = 1.1)
lines(x = seq(0, 100, by = 1), y4, type = "l", col = "orange", lwd = 2)
lines(x = seq(0, 100, by = 1), y6, type = "l", col = "purple", lwd = 2)
legend(x = 40, y = 0.3, legend = c("M-estimation", "OLS", "PWRM (p=1/4)"),
       col = c("blue", "orange", "purple"), lty = c(1, 3, 4), cex = 1.1, bty = "n")

# Plot 3
ddata6 <- read.csv(file = "RE_n20.csv")
y1 <- ddata6$RE1
y2 <- ddata6$RE2
y3 <- ddata6$RE3
y4 <- ddata6$RE4
y5 <- ddata6$RE5
y6 <- ddata6$RE6

y11 <- y1 * 100
y22 <- y2 * 100
y33 <- y3 * 100
y44 <- y4 * 100
y55 <- y5 * 100
y66 <- y6 * 100


plot(x = seq(0, 100, by = 1), y11, col = "blue", type = "l", lwd = 2,
     ylim = range(0, 100), xlab = expression(delta), ylab = "RE(%)")
mtext(expression("(c)"), side = 1, line = 6, cex = 1.1)
lines(x = seq(0, 100, by = 1), y33, type = "l", col = "green", lwd = 2)
lines(x = seq(0, 100, by = 1), y44, type = "l", col = "orange", lwd = 2)
lines(x = seq(0, 100, by = 1), y66, type = "l", col = "purple", lwd = 2)

legend(x = 40, y = 60,  legend = c("M-estimation", "MLE", "OLS", "PWRM (p=1/4)"),
       col = c("blue", "green", "orange", "purple"), lty = 1:4, cex = 1.1, bty = "n")





# -----------------------------------------------------
# Figure4 G.V. and RE for complete sample with n=40
# -----------------------------------------------------
# ##Comment: Type II censogreen sample tried, but not good

ddata7 <- read.csv(file="mse_n40.csv")

y1 <- ddata7$mse1
y2 <- ddata7$mse2
y3 <- ddata7$mse3
y4 <- ddata7$mse4
y5 <- ddata7$mse5
y6 <- ddata7$mse6

# Plot 1
plot(x = seq(0, 100, by = 1), y1, col = "blue", type = "l", lwd = 2,
     ylim = range(c(0, 40)), xlab = expression(delta), ylab = "Generalized MSE")
mtext(expression("(d)"), side = 1, line = 6, cex = 1.1)
lines(x = seq(0, 100, by = 1), y3, col = "green", type = "l", lwd = 2)
lines(x = seq(0, 100, by = 1), y4, col = "orange", type = "l", lwd = 2)
lines(x = seq(0, 100, by = 1), y6, col = "purple", type = "l", lwd = 2)

legend("topleft",
       legend = c("M-estimation", "MLE", "OLS", "PWRM (p=1/4)"),
       col = c("blue", "green", "orange", "purple"),
       lty = 1:4, lwd = 2, cex = 1.1, bty = "n")

# Plot 2
plot(x = seq(0, 100, by = 1), y1, col = "blue", type = "l", lwd = 2,
     ylim = range(c(0, 0.6)), xlab = expression(delta), ylab = "Generalized MSE")
mtext(expression("(e)"), side = 1, line = 6, cex = 1.1)
lines(x = seq(0, 100, by = 1), y4, col = "orange", type = "l", lwd = 2)
lines(x = seq(0, 100, by = 1), y6, col = "purple", type = "l", lwd = 2)

legend(x = 35, y = 0.25,
       legend = c("M-estimation", "OLS", "PWRM (p=1/4)"),
       col = c("blue", "orange", "purple"),
       lty = c(1, 2, 3), lwd = 2, cex = 1.1, bty = "n")

# Plot 3
ddata8 <- read.csv(file = "RE_n40.csv")

y1 <- ddata8$RE1 * 100
y2 <- ddata8$RE2 * 100
y3 <- ddata8$RE3 * 100
y4 <- ddata8$RE4 * 100
y5 <- ddata8$RE5 * 100
y6 <- ddata8$RE6 * 100

plot(x = seq(0, 100, by = 1), y1, col = "blue", type = "l", lwd = 2,
     ylim = c(0, 100), xlab = expression(delta), ylab = "RE(%)")
mtext(expression("(f)"), side = 1, line = 6, cex = 1.1)
lines(x = seq(0, 100, by = 1), y3, col = "green", type = "l", lwd = 2)
lines(x = seq(0, 100, by = 1), y4, col = "orange", type = "l", lwd = 2)
lines(x = seq(0, 100, by = 1), y5, col = "black", type = "l", lwd = 2)
lines(x = seq(0, 100, by = 1), y6, col = "purple", type = "l", lwd = 2)

legend(x = 35, y = 55,
       legend = c("M-estimation", "MLE", "OLS",  "PWRM (p=1/4)", "PWRM (p=1/2)"),
       col = c("blue", "green", "orange", "purple", "black"),
       lty = 1:5, lwd = 2, cex = 1.1, bty = "n")


# -----------------plotEnd------------------

# -------------------------------------------
# code1: functions
# -------------------------------------------
library(survival)
library(msme)
library(MASS)

#library(pacman)
#pacman::p_load(MASS, survival, msme)

# -------------------------------------------
# Calculate pars by robust regression method
# -------------------------------------------
Robust.R <- function(x, y, weight=FALSE, power=1)   
{
  data1<-data.frame(x,y) 
  fit <- rlm(y~x, data=data1, init = "ls", 
             psi = psi.bisquare, scale.est = "MAD",
             method =  "M", wt.method = "inv.var", 
             maxit = 200, acc = 1e-4, test.vec = "resid", lqs.control = NULL)
  ans <- fit$coefficients   
  names(ans) <- NULL
  return(ans)  
}


# -------------------------------------------
# OLS
# -------------------------------------------
ols.R <- function(x, y)   
{
  data1<-data.frame(x,y) 
  fit <- lm(y~x, data=data1)
  ans <- fit$coefficients       
  names(ans) <- NULL
  return(ans)  
}


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
# power weighted repeated median: power1, 2 and 3
# -----------------------------------------------
Repeated.W.Median1 <- function (x,y, weight=TRUE, power=power1) {
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

Repeated.W.Median2 <- function (x,y, weight=TRUE, power=power2) {
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

Repeated.W.Median3 <- function (x,y, weight=TRUE, power=power3) {
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


# ---------------------------------------------------------
# Generate sample from two parameter Rayleigh distribution 
# ---------------------------------------------------------
rray.twopar <- function(n, lambda, mu)  
{
  u <- runif(n, min=0, max=1)
  x <- mu+sqrt(-1/lambda*log(1-u))
  return(x)
}


# ---------------------------------------------------------------------------
# Generating TYPE II censogreen sample from two-parameter Rayleigh distribution
# ---------------------------------------------------------------------------
rray.type2cens.twopar <- function(n, r, lambda, mu)
{
  x <- rray.twopar(n, lambda, mu)
  z <- sort(x)
  samp <- z[1:r]
  event<-(x<=samp[r])
  ans <- list(x=x, samp=samp, t=samp[r], event=event, r=r, n=n)
  
  return(ans)
}


# ---------------------------------------------------------
# MLE for complete Rayleigh sample: method 1
# ---------------------------------------------------------
#followed Park's code: MLE of Weibull
#error occurgreen from lower and upper bounds settings

ray.MLE <- function (x, tol=.Machine$double.eps^0.25, maxiter=1000, trace=0) {
  # Setup for interval 
  n <- length(x)
  lower = abs(min(x)-0.2)
  upper = min(x)
  interval = c(lower, upper)
  
  # EE equation 
  EEray= function(mu, x) {
    xmu = sum((x-mu)^2)
    sum(-1/(x-mu))+2*n*sum(x-mu)/xmu
  }
  tmp = uniroot(EEray, interval = interval, x = x, tol = .Machine$double.eps^0.25,
                maxiter = 1000, trace = 0)
  muhat = tmp$root
  lambdahat = n/(sum((x-muhat)^2))
  return( c(lambdahat , muhat) )
} # END of MLE of Rayleigh


# ---------------------------------------------------------
# MLE for complete Rayleigh sample: method 2
# ---------------------------------------------------------
#theta[1]=lambda, theta[2]=mu
##log likelihood function for complete sample
flog2 <- function(theta, x)
{
  n <- length(x)
  lambda <- theta[1]
  mu <- theta[2]
  ln <- n*log(lambda)+sum(log(abs(x-mu)))-lambda*sum((x-mu)^2)
  return(ln)
}

ray.mle.twopar <- function(pars, x)
{
  ans <- optim(par=pars, fn=flog2, x=x, control=list(fnscale=-1), method="Nelder-Mead")
  return(ans$par)
}

# ---------------------------------------------------------
# MLE for complete Rayleigh sample: method 3
# ---------------------------------------------------------

ray.mle2.twopar <- function(mu, x)
{ 
  n <- length(x)
  lower <- min(x)
  upper <- 5*max(x)
  fmu <- function(mu,x){
    lmu <- n*log(n)-n*log(sum((x-mu)^2))+sum(log(abs(x-mu)))-n
  }
  ans <- optim(par=mu, fn=fmu, x=x, lower=lower, upper=upper, control=list(fnscale=-1), method="Brent")
  muhat <- ans$par
  lambdahat = n/(sum((x-muhat)^2))
  return(c(lambdahat, muhat))  
}

# -----------------------------------------------------
# log likelihood function for Type II censogreen samples
# -----------------------------------------------------
## z is a vector consists of two parameters, z[1] is lambda=2 and z[2] is mu=1.5
floglike.twopar <- function(z, x) 
{
  if (z[1] <= 0 || z[2] <= 0)
  {
    return (-Inf)
  }
  
  s1 <- 0
  s2 <- 0
  if (x$r > 0)
  {
    for (j in 1:x$r)
    {
      s1 <- s1+log(abs(x$samp[j]-z[2]))
      s2 <- s2+(x$samp[j]-z[2])^2
    }
  }
  else
  {
    stop("floglike: Log Likelihood function cannot be evaluated since r=0.")
  }
  ans <- x$r*log(z[1])+s1-z[1]*s2- z[1]*(x$n-x$r)*(x$t-z[2])^2
  return(ans)
}


# ----------------------------------------------------
# MLE for Type II censogreen samples
# ----------------------------------------------------
#par[1] is lambda and par[2] is mu
ray.type2mle.twopar <- function(pars, x)
{
  ans <- optim(par=pars, fn=floglike.twopar, x=x, control=list(fnscale=-1), method="Nelder-Mead")
  return(ans$par)
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
  ans = det(S)
  return(ans)
}


# --------------------------------------------------------------
# code2 for figures 1: parameter estimates under complete sample
# --------------------------------------------------------------
# 4 methods together for no contamination
# 1-PWRM(p=0.25); 2-PWRM(p=0.5); 3-PWRM(p=0.75); 4-RR; 5-MLE; 6-OLS  

All.Estimators1b <- function(n, r, lambda, mu, weight, B)
{
  pars01 <- matrix(nrow=B, ncol=2)
  pars02 <- matrix(nrow=B, ncol=2)
  pars03 <- matrix(nrow=B, ncol=2)
  pars04 <- matrix(nrow=B, ncol=2)
  pars05 <- matrix(nrow=B, ncol=2)
  pars06 <- matrix(nrow=B, ncol=2)
  a <- vector()
  b <- vector()
  cc <- vector()
  d <- vector()
  e <- vector()
  m <- n
  pi1<-rep(0,m)
  for(j in 1:m){
    if(m<=10){
      pi1[j]<-(j-3/8)/(m+0.25)
    }
    else{
      pi1[j]<-(j-0.5)/m
    }
  }
  y1 <- sqrt(-log(1-pi1))
  
  for (i in 1:B)
  {
    T <- rray.twopar(n, lambda, mu)
    #xi=ti for Rayleigh
    x <- sort(T)
    y <- sort(y1)
    ##parameters-lambda??mu
    a <- Repeated.W.Median1(x,y, weight=TRUE, power=1/4)
    pars01[i, ] <- c((a[2])^2, -a[1]/a[2])
    b <- Repeated.W.Median2(x,y, weight=TRUE, power=1/2)
    pars02[i, ] <- c((b[2])^2, -b[1]/b[2])
    cc <- Repeated.W.Median3(x,y, weight=TRUE, power=3/4)
    pars03[i, ] <- c((cc[2])^2, -cc[1]/cc[2])
    d <- Robust.R(x, y, weight=FALSE, power=1)
    pars04[i, ] <- c((d[2])^2, -d[1]/d[2])
    pars05[i, ] <- ray.mle.twopar(c(lambda, mu),x) #method 2
    e <- ols.R(x, y)
    pars06[i, ] <- c((e[2])^2, -e[1]/e[2])
  }
  ans <- data.frame(n, r, pars01[, 1], pars01[, 2], pars02[, 1], pars02[, 2], pars03[, 1], pars03[, 2], pars04[, 1], pars04[, 2], pars05[, 1], pars05[, 2], pars06[, 1], pars06[, 2])
  names(ans) <- c("n","r","lambda01","mu01","lambda02","mu02","lambda03","mu03","lambda04","mu04", "lambda05","mu05", "lambda06","mu06")
  return(ans)
}

result1b <- All.Estimators1b(n=20, r=20, lambda=2, mu=1.5, weight, B=10000)
#write.csv(result1b, file="D://Research//Output//output01b.csv")
##comment: code for complete sample only: r=n; can't apply to Type II censogreen sample

#-------------------------------------------------------------------------------
# 4 methods together for contamination
# # 1-PWRM(p=0.25); 2-PWRM(p=0.5); 3-PWRM(p=0.75); 4-RR; 5-MLE; 6-OLS

  All.Estimators2 <- function(n, r, lambda, mu, weight, B)
  {
    pars01 <- matrix(nrow=B, ncol=2)
    pars02 <- matrix(nrow=B, ncol=2)
    pars03 <- matrix(nrow=B, ncol=2)
    pars04 <- matrix(nrow=B, ncol=2)
    pars05 <- matrix(nrow=B, ncol=2)
    pars06 <- matrix(nrow=B, ncol=2)
    a <- vector()
    b <- vector()
    cc <- vector()
    d <- vector()
    e <- vector()
    m <- n
    pi1<-rep(0,m)
    for(j in 1:m){
      if(m<=10){
        pi1[j]<-(j-3/8)/(m+0.25)
      }
      else{
        pi1[j]<-(j-0.5)/m
      }
    }
    y1 <- sqrt(-log(1-pi1))
    
    for (i in 1:B)
    {
      T <- rray.type2cens.twopar(n, r, lambda, mu)
      #xi=ti for Rayleigh    
      n <- T$n
      order_sample <- sort(T$x)
      delta <- order_sample[n]+4*(sd(T$x))
      #delta=10 can be tried
      new_sample <- replace(order_sample, 1, delta)
      x <- sort(new_sample)
      y <- sort(y1)
      ##parameters-lambda and mu
      a <- Repeated.W.Median1(x,y, weight=TRUE, power=1/4)
      pars01[i, ] <- c((a[2])^2, -a[1]/a[2])
      b <- Repeated.W.Median2(x,y, weight=TRUE, power=1/2)
      pars02[i, ] <- c((b[2])^2, -b[1]/b[2])
      cc <- Repeated.W.Median3(x,y, weight=TRUE, power=3/4)
      pars03[i, ] <- c((cc[2])^2, -cc[1]/cc[2])
      d <- Robust.R(x, y, weight=FALSE, power=1)
      pars04[i, ] <- c((d[2])^2, -d[1]/d[2])
      pars05[i, ] <- ray.mle.twopar(c(lambda, mu), x=x) #method 2
      e <- ols.R(x, y)
      pars06[i, ] <- c((e[2])^2, -e[1]/e[2])
    }
    ans <- data.frame(n, r, pars01[, 1], pars01[, 2], pars02[, 1], pars02[, 2], pars03[, 1], pars03[, 2], pars04[, 1], pars04[, 2], pars05[, 1], pars05[, 2], pars06[, 1], pars06[, 2])
    names(ans) <- c("n","r","lambda01","mu01","lambda02","mu02","lambda03","mu03","lambda04","mu04", "lambda05","mu05", "lambda06","mu06")
    return(ans)
  }
result2 <- All.Estimators2(n=20, r=20, lambda=2, mu=1.5, weight, B=10000)
write.csv(result2, file="D://Research//Output//output02.csv")
##comment: code for complete sample only: r=n(20 or 40); can't apply to Type II censogreen sample


# ---------------------------------------------------------------------------------------
# code3 for figures 2: parameter estimates under Type II censogreen sample:r=16 out of n=20
# ---------------------------------------------------------------------------------------

# 4 methods together for no contamination
# 1-PWRM(p=0.25); 2-PWRM(p=0.5); 3-PWRM(p=0.75); 4-RR; 5-MLE; 6-OLS 

All.Estimators3 <- function(n, r, lambda, mu, weight, B)
{
  pars01 <- matrix(nrow=B, ncol=2)
  pars02 <- matrix(nrow=B, ncol=2)
  pars03 <- matrix(nrow=B, ncol=2)
  pars04 <- matrix(nrow=B, ncol=2)
  pars05 <- matrix(nrow=B, ncol=2)
  pars06 <- matrix(nrow=B, ncol=2)
  a <- vector()
  b <- vector()
  cc <- vector()
  d <- vector()
  e <- vector()
  for (i in 1:B)
  {
    T <- rray.type2cens.twopar(n, r, lambda, mu)
    #xi=ti for Rayleigh
    time.t <- T$samp   
    #T$samp: type II censogreen sample
    ##complete sample x0 and indicator event0
    x0<-T$x
    event0 <- T$event
    survobject <- Surv(time = x0, event=event0)
    data0<-data.frame(x0,event0)
    ##K-M method applied to calculate survival function S(x) under censogreen sample
    fit1 <- survfit(survobject ~ 1, data = data0)
    si <- summary(fit1)$surv
    # CDF F(x)=1-S(x) and the corresponding variable y0 calculated
    y0 <- sqrt(-log(si))
    y <- sort(y0)
    x <- sort(time.t)   
    ##parameters-lambda and mu
    a <- Repeated.W.Median1(x,y, weight=TRUE, power=1/4)
    pars01[i, ] <- c((a[2])^2, -a[1]/a[2])
    b <- Repeated.W.Median2(x,y, weight=TRUE, power=1/2)
    pars02[i, ] <- c((b[2])^2, -b[1]/b[2])
    cc <- Repeated.W.Median3(x,y, weight=TRUE, power=3/4)
    pars03[i, ] <- c((cc[2])^2, -cc[1]/cc[2])
    d <- Robust.R(x, y, weight=FALSE, power=1)
    pars04[i, ] <- c((d[2])^2, -d[1]/d[2])
    pars05[i, ] <- ray.type2mle.twopar(c(lambda, mu), x=T)
    e <- ols.R(x, y)
    pars06[i, ] <- c((e[2])^2, -e[1]/e[2])
  }
  ans <- data.frame(n, r, pars01[, 1], pars01[, 2], pars02[, 1], pars02[, 2], pars03[, 1], pars03[, 2], pars04[, 1], pars04[, 2], pars05[, 1], pars05[, 2], pars06[, 1], pars06[, 2])
  names(ans) <- c("n","r","lambda01","mu01","lambda02","mu02","lambda03","mu03","lambda04","mu04", "lambda05","mu05", "lambda06","mu06")
  return(ans)
}
result3 <- All.Estimators3(n=20, r=16, lambda=2, mu=1.5, weight, B=10000)
write.csv(result3, file="D://Research//Output//output07_0.8.csv")

#-------------------------------------------------------------------------------
# 4 methods together for contamination
# # 1-PWRM(p=0.25); 2-PWRM(p=0.5); 3-PWRM(p=0.75); 4-RR; 5-MLE; 6-OLS

  All.Estimators4 <- function(n, r, lambda, mu, weight, B)
  {
    pars01 <- matrix(nrow=B, ncol=2)
    pars02 <- matrix(nrow=B, ncol=2)
    pars03 <- matrix(nrow=B, ncol=2)
    pars04 <- matrix(nrow=B, ncol=2)
    pars05 <- matrix(nrow=B, ncol=2)
    pars06 <- matrix(nrow=B, ncol=2)
    a <- vector()
    b <- vector()
    cc <- vector()
    d <- vector()
    e <- vector()
    for (i in 1:B)
    {
      T <- rray.type2cens.twopar(n, r, lambda, mu)
      #xi=ti for Rayleigh
      time.t <- T$samp   
      #T$samp: type II censogreen sample
      ##complete sample x0 and indicator event0
      x0<-T$x
      event0 <- T$event
      survobject <- Surv(time = x0, event=event0)
      data0<-data.frame(x0,event0)
      ##K-M method applied to calculate survival function S(x) under censogreen sample
      fit1 <- survfit(survobject ~ 1, data = data0)
      si <- summary(fit1)$surv
      # CDF F(x)=1-S(x) and the corresponding variable y0 calculated
      y0 <- sqrt(-log(si))
      y <- sort(y0)
      ##contaminated sample:time.t[1] replaced by delta
      #delta=30 can be tried
      delta <- x0[n]+4*(sd(x0)) #4*sigma shifting
      x1 <- replace(time.t, 1, delta)
      x <- sort(x1)
      ## x: contaminated Type II censogreen sample is sorted
      #T1: a list needed to calculated MLE
      T1 <- list(x=x0, samp=x, t=x[r], event=event0, r=T$r, n=T$n)
      ##parameters-intercept, slope
      a <- Repeated.W.Median1(x,y, weight=TRUE, power=1/4)
      pars01[i, ] <- c((a[2])^2, -a[1]/a[2])
      b <- Repeated.W.Median2(x,y, weight=TRUE, power=1/2)
      pars02[i, ] <- c((b[2])^2, -b[1]/b[2])
      cc <- Repeated.W.Median3(x,y, weight=TRUE, power=3/4)
      pars03[i, ] <- c((cc[2])^2, -cc[1]/cc[2])
      d <- Robust.R(x, y, weight=FALSE, power=1)
      pars04[i, ] <- c((d[2])^2, -d[1]/d[2])
      pars05[i, ] <- ray.type2mle.twopar(c(lambda, mu), x=T1)
      e <- ols.R(x, y)
      pars06[i, ] <- c((e[2])^2, -e[1]/e[2])
    }
    ans <- data.frame(n, r, pars01[, 1], pars01[, 2], pars02[, 1], pars02[, 2], pars03[, 1], pars03[, 2], pars04[, 1], pars04[, 2], pars05[, 1], pars05[, 2], pars06[, 1], pars06[, 2])
    names(ans) <- c("n","r","lambda01","mu01","lambda02","mu02","lambda03","mu03","lambda04","mu04", "lambda05","mu05", "lambda06","mu06")
    return(ans)
  }
result4 <- All.Estimators4(n=20, r=16, lambda=2, mu=1.5, weight, B=10000)
write.csv(result4, file="D://Research//Output//output08_0.8.csv")


# -------------------------------------------------------
# code4 for figures 3 and 4: calculation for G.V. and RE 
# -------------------------------------------------------

# 4 methods together for contamination (delta given): applied to complete sample only
# 1-RR; 2-PWRM(p=power1); 3-MLE; 4-OLS; 5-PWRM(p=power2); 6-PWRM(p=power3)

All4.Estimators <- function(n, r, lambda, mu, weight, B, delta, power1, power2, power3)
{
  pars01 <- matrix(nrow=B, ncol=2)
  pars02 <- matrix(nrow=B, ncol=2)
  pars03 <- matrix(nrow=B, ncol=2)
  pars04 <- matrix(nrow=B, ncol=2)
  pars05 <- matrix(nrow=B, ncol=2)
  pars06 <- matrix(nrow=B, ncol=2)
  a <- vector()
  b <- vector()
  cc <- vector()
  d <- vector()
  e <- vector()
  m <- n
  pi1<-rep(0,m)
  for(j in 1:m){
    if(m<=10){
      pi1[j]<-(j-3/8)/(m+0.25)
    }
    else{
      pi1[j]<-(j-0.5)/m
    }
  }
  y1 <- sqrt(-log(1-pi1))
  
  for (i in 1:B)
  {
    T <- rray.type2cens.twopar(n, r, lambda, mu)
    #xi=ti for Rayleigh    
    order_sample <- sort(T$x)
    #delta is added to the 1st observation and is given
    new_sample <- replace(order_sample, 1, (order_sample[1]+delta))
    x <- sort(new_sample)
    y <- sort(y1)
    T1 <- list(x=new_sample, samp=x[1:r],t=x[r], r=r, n=n)
    ##parameters-lambda and mu
    a <- Robust.R(x, y, weight=FALSE, power=1)
    pars01[i, ] <- c((a[2])^2, -a[1]/a[2])
    b <- Repeated.W.Median1(x,y, weight=TRUE, power=power1)
    pars02[i, ] <- c((b[2])^2, -b[1]/b[2])
    pars03[i, ] <- ray.type2mle.twopar(pars=c(lambda, mu), x=T1) 
    cc <- ols.R(x, y)
    pars04[i, ] <- c((cc[2])^2, -cc[1]/cc[2])
    d <- Repeated.W.Median2(x,y, weight=TRUE, power=power2)
    pars05[i, ] <- c((d[2])^2, -d[1]/d[2])
    e <- Repeated.W.Median3(x,y, weight=TRUE, power=power3)
    pars06[i, ] <- c((e[2])^2, -e[1]/e[2])
    
  }
  ans <- data.frame(pars01[, 1], pars01[, 2], pars02[, 1], pars02[, 2], pars03[, 1], pars03[, 2], pars04[, 1], pars04[, 2], pars05[, 1], pars05[, 2], pars06[, 1], pars06[, 2])
  names(ans) <- c("lambda01","mu01","lambda02","mu02","lambda03","mu03","lambda04","mu04","lambda05","mu05","lambda06","mu06")
  return(ans)
}

# 4 methods together for contamination (delta given): applied to Type II censogreen sample
# 1-RR; 2-PWRM(p=power1); 3-MLE; 4-OLS; 5-PWRM(p=power2); 6-PWRM(p=power3)

All4type2.Estimators <- function(n, r, lambda, mu, weight, B, delta, power1, power2, power3)
{
  pars01 <- matrix(nrow=B, ncol=2)
  pars02 <- matrix(nrow=B, ncol=2)
  pars03 <- matrix(nrow=B, ncol=2)
  pars04 <- matrix(nrow=B, ncol=2)
  pars05 <- matrix(nrow=B, ncol=2)
  pars06 <- matrix(nrow=B, ncol=2)
  a <- vector()
  b <- vector()
  cc <- vector()
  d <- vector()
  e <- vector()
  
  for (i in 1:B)
  {
    T <- rray.type2cens.twopar(n, r, lambda, mu)
    #xi=ti for Rayleigh
    time.t <- T$samp   
    #T$samp: type II censogreen sample
    ##complete sample x0 and indicator event0
    x0<-T$x
    event0 <- T$event
    survobject <- Surv(time = x0, event=event0)
    data0<-data.frame(x0,event0)
    fit1 <- survfit(survobject ~ 1, data = data0)
    si <- summary(fit1)$surv
    y0 <- sqrt(-log(si))
    y <- sort(y0)
    order_sample <- time.t
    #delta is added to the 1st observation and is given
    new_sample <- replace(order_sample, 1, (order_sample[1]+delta))
    x <- sort(new_sample)
    T1 <- list(x=x0, samp=x[1:r],t=x[r], r=r, n=n)
    ##parameters-lambda and mu
    a <- Robust.R(x, y, weight=FALSE, power=1)
    pars01[i, ] <- c((a[2])^2, -a[1]/a[2])
    b <- Repeated.W.Median1(x,y, weight=TRUE, power=power1)
    pars02[i, ] <- c((b[2])^2, -b[1]/b[2])
    pars03[i, ] <- ray.type2mle.twopar(pars=c(lambda, mu), x=T1) 
    cc <- ols.R(x, y)
    pars04[i, ] <- c((cc[2])^2, -cc[1]/cc[2])
    d <- Repeated.W.Median2(x,y, weight=TRUE, power=power2)
    pars05[i, ] <- c((d[2])^2, -d[1]/d[2])
    e <- Repeated.W.Median3(x,y, weight=TRUE, power=power3)
    pars06[i, ] <- c((e[2])^2, -e[1]/e[2])
    
  }
  ans <- data.frame(pars01[, 1], pars01[, 2], pars02[, 1], pars02[, 2], pars03[, 1], pars03[, 2], pars04[, 1], pars04[, 2], pars05[, 1], pars05[, 2], pars06[, 1], pars06[, 2])
  names(ans) <- c("lambda01","mu01","lambda02","mu02","lambda03","mu03","lambda04","mu04","lambda05","mu05","lambda06","mu06")
  return(ans)
}

## function for G.V. repeated
MSE.gen = function(x,y, mux=lambda, muy=mu) {
  N = length(x)
  a11 = sum( (x-mux)^2 )
  a22 = sum( (y-muy)^2 )
  a12 = sum( (x-mux)*(y-muy) )
  S = 1/N * matrix( c(a11,a12,a12,a22), nrow=2)
  det(S)
}

##calculation for G.V. under either complete or Type II censogreen sample when delta=0(1)100
MSE.delta <- function(n, r, lambda, mu, weight, B, a, b, deltax, power1, power2, power3){
  mse1 <- vector()
  mse2 <- vector()
  mse3 <- vector()
  mse4 <- vector()
  mse5 <- vector()
  mse6 <- vector()
  delta <- vector()
  ddata <- list()
  m <- (b-a)/deltax
  for (i in 1:(m+1)){
    delta[i] <- (a+(i-1)*deltax) 
    ddata[[i]] <- All4.Estimators(n, r, lambda, mu, weight, B, delta[i], power1, power2, power3)
    ##Type II censogreen sample can be tried, but not good
    #ddata[[i]] <- All4type2.Estimators(n, r, lambda, mu, weight, B, delta[i], power1, power2, power3)    
    ##1-RR; 2-PWRM; 3-MLE; 4-OLS; 5-PWRM-pwer2; 6-PWRM-pwer3
    mse1[i] <- MSE.gen(x=ddata[[i]]$lambda01,y=ddata[[i]]$mu01, mux=lambda, muy=mu)
    mse2[i] <- MSE.gen(x=ddata[[i]]$lambda02,y=ddata[[i]]$mu02, mux=lambda, muy=mu)
    mse3[i] <- MSE.gen(x=ddata[[i]]$lambda03,y=ddata[[i]]$mu03, mux=lambda, muy=mu)
    mse4[i] <- MSE.gen(x=ddata[[i]]$lambda04,y=ddata[[i]]$mu04, mux=lambda, muy=mu)
    mse5[i] <- MSE.gen(x=ddata[[i]]$lambda05,y=ddata[[i]]$mu05, mux=lambda, muy=mu)
    mse6[i] <- MSE.gen(x=ddata[[i]]$lambda06,y=ddata[[i]]$mu06, mux=lambda, muy=mu)
  }
  ans <- data.frame(mse1, mse2, mse3, mse4, mse5, mse6)
  names(ans) <- c("mse1","mse2","mse3","mse4","mse5","mse6")
  return(ans)
}

result5 <- MSE.delta(n=20, r=20, lambda=2, mu=1.5, weight, B=1000, a=0, b=100, deltax=1, power1=0.75, power2=0.5, power3=0.25)
write.csv(result5, file="D://Research//Output//mse_n20.csv")
result6 <- MSE.delta(n=40, r=40, lambda=2, mu=1.5, weight, B=1000, a=0, b=100, deltax=1, power1=0.75, power2=0.5, power3=0.25)
write.csv(result6, file="D://Research//Output//mse_n40.csv")


# 4 methods together for contamination (delta given): applied to complete sample only
#1-M-estimation; 2-PWRM(p=power1=0.75); 3-MLE; 4-OLS; 
#5-PWRM(p=power2=0.5); 6-PWRM(p=power2=0.25); 7-OLS(baseline&No contamination)

All5.Estimators <- function(n, r, lambda, mu, weight, B, delta, power1, power2, power3)
{
  pars01 <- matrix(nrow=B, ncol=2)
  pars02 <- matrix(nrow=B, ncol=2)
  pars03 <- matrix(nrow=B, ncol=2)
  pars04 <- matrix(nrow=B, ncol=2)
  pars05 <- matrix(nrow=B, ncol=2)
  pars06 <- matrix(nrow=B, ncol=2)
  pars07 <- matrix(nrow=B, ncol=2)
  a <- vector()
  b <- vector()
  cc <- vector()
  d <- vector()
  e <- vector()
  f <- vector()
  m <- n
  pi1<-rep(0,m)
  for(j in 1:m){
    if(m<=10){
      pi1[j]<-(j-3/8)/(m+0.25)
    }
    else{
      pi1[j]<-(j-0.5)/m
    }
  }
  y1 <- sqrt(-log(1-pi1))
  
  for (i in 1:B)
  {
    T <- rray.type2cens.twopar(n, r, lambda, mu)
    #xi=ti for Rayleigh    
    order_sample <- sort(T$x)
    #delta is added to the 1st observation and is given
    new_sample <- replace(order_sample, 1, (order_sample[1]+delta))
    x <- sort(new_sample)
    y <- sort(y1)
    T1 <- list(x=new_sample, samp=x[1:r],t=x[r], r=r, n=n)
    ##parameters-lambda and mu
    a <- Robust.R(x, y, weight=FALSE, power=1)
    pars01[i, ] <- c((a[2])^2, -a[1]/a[2])
    b <- Repeated.W.Median1(x,y, weight=TRUE, power=power1)
    pars02[i, ] <- c((b[2])^2, -b[1]/b[2])
    pars03[i, ] <- ray.type2mle.twopar(pars=c(lambda, mu), x=T1) 
    cc <- ols.R(x, y)
    pars04[i, ] <- c((cc[2])^2, -cc[1]/cc[2])
    d <- Repeated.W.Median2(x,y, weight=TRUE, power=power2)
    pars05[i, ] <- c((d[2])^2, -d[1]/d[2])
    e <- Repeated.W.Median3(x,y, weight=TRUE, power=power3)
    pars06[i, ] <- c((e[2])^2, -e[1]/e[2])
    f <- ols.R(order_sample, y)
    pars07[i, ] <- c((f[2])^2, -f[1]/f[2])
    
  }
  ans <- data.frame(pars01[, 1], pars01[, 2], pars02[, 1], pars02[, 2], pars03[, 1], pars03[, 2], pars04[, 1], pars04[, 2], pars05[, 1], pars05[, 2], pars06[, 1], pars06[, 2], pars07[, 1], pars07[, 2])
  names(ans) <- c("lambda01","mu01","lambda02","mu02","lambda03","mu03","lambda04","mu04","lambda05","mu05","lambda06","mu06", "lambda07","mu07")
  return(ans)
}

# All5.Estimators <- function(n, r, lambda, mu, weight, B, delta, power1, power2, power3)

#1-M-estimation; 2-PWRM(p=power1=0.75); 3-MLE; 4-OLS; 
#5-PWRM(p=power2=0.5); 6-PWRM(p=power2=0.25); 7-OLS(baseline&No contamination)
#re1=mse7/mse1; re2=mse7/mse2; re3=mse7/mse3
#re4=mse7/mse4; re5=mse7/mse5; re6=mse7/mse6
##calculation for RE under complete sample when delta=0(1)100
RE.delta <- function(n, r, lambda, mu, weight, B, a, b, deltax, power1, power2, power3){
  mse1 <- vector()
  mse2 <- vector()
  mse3 <- vector()
  mse4 <- vector()
  mse5 <- vector()
  mse6 <- vector()
  mse7 <- vector()
  re1 <- vector()
  re2 <- vector()
  re3 <- vector()
  re4 <- vector()
  re5 <- vector()
  re6 <- vector()
  delta <- vector()
  ddata <- list()
  m <- (b-a)/deltax
  for (i in 1:(m+1)){
    delta[i] <- (a+(i-1)*deltax) 
    ddata[[i]] <- All5.Estimators(n, r, lambda, mu, weight, B, delta[i], power1, power2, power3)
    mse1[i] <- MSE.gen(x=ddata[[i]]$lambda01,y=ddata[[i]]$mu01, mux=lambda, muy=mu)
    mse2[i] <- MSE.gen(x=ddata[[i]]$lambda02,y=ddata[[i]]$mu02, mux=lambda, muy=mu)
    mse3[i] <- MSE.gen(x=ddata[[i]]$lambda03,y=ddata[[i]]$mu03, mux=lambda, muy=mu)
    mse4[i] <- MSE.gen(x=ddata[[i]]$lambda04,y=ddata[[i]]$mu04, mux=lambda, muy=mu)
    mse5[i] <- MSE.gen(x=ddata[[i]]$lambda05,y=ddata[[i]]$mu05, mux=lambda, muy=mu)
    mse6[i] <- MSE.gen(x=ddata[[i]]$lambda06,y=ddata[[i]]$mu06, mux=lambda, muy=mu)
    mse7[i] <- MSE.gen(x=ddata[[i]]$lambda07,y=ddata[[i]]$mu07, mux=lambda, muy=mu)
    re1[i] <- mse7[i]/mse1[i]
    re2[i] <- mse7[i]/mse2[i]
    re3[i] <- mse7[i]/mse3[i]
    re4[i] <- mse7[i]/mse4[i]
    re5[i] <- mse7[i]/mse5[i]
    re6[i] <- mse7[i]/mse6[i]
  }
  ans <- data.frame(re1, re2, re3, re4, re5, re6)
  names(ans) <- c("RE1","RE2","RE3","RE4","RE5","RE6")
  return(ans)
}


result7 <- RE.delta(n=20, r=20, lambda=2, mu=1.5, weight, B=1000, a=0, b=100, deltax=1,power1=0.75, power2=0.5, power3=0.25)
#write.csv(result7, file="D://Research//Output//RE_n20.csv")

result8 <- RE.delta(n=40, r=40, lambda=2, mu=1.5, weight, B=1000, a=0, b=100, deltax=1,power1=0.75, power2=0.5, power3=0.25)
#write.csv(result8, file="D://Research//Output//RE_n40.csv")

