# USL model of NUMA data in GCaP book Appendix B.
# Created by NJG on Tue May 19 09:34:01 PDT 2009
# Updated by NJG on Thu Dec  8 07:53:04 2011

df.numa <- read.table(text="p	Xp
1   20
4   78
8   130
12  170
16  190
20  200
24  210
28  230
32  260
48  280
64  310", 
header=TRUE)

# Check the dataframe
print(df.numa)

# Normalize data and check efficiency
df.numa$Cp <- df.numa$Xp / df.numa$Xp[1]
df.numa$Ep <- df.numa$Cp / df.numa$p
print(df.numa)

# Standard non-linear least squares (NLS) fit using USL model
usl <- nls(Cp ~ p / (1 + sigma*(p-1) + kappa*p*(p-1)), 
           data=df.numa, 
           start=c(sigma=0.1, kappa=0.01)
           )

# Look at the fitted USL parameters (sigma,kappa)
print(coef(usl))
# Check the blah blah statistical parameters
print(summary(usl))

# Get sigma & kappa parameters for use in plot legend
x.coef <- coef(usl)
print(x.coef)

# Determine sum-of-squares for R-squared coeff from NLS fit
sse <- sum((df.numa$Cp - predict(usl))^2)
sst <- sum((df.numa$Cp - mean(df.numa$Cp))^2)

# Calculate pmax and X(pmax)
pmax <- sqrt((1 - x.coef['sigma']) / x.coef['kappa'])
xmax <- df.numa$Xp[1]*pmax / (1 + x.coef['sigma']*(pmax - 1) + x.coef['kappa']*pmax*(pmax - 1))


# Plot all the results
plot(x <- c(0:64), df.numa$Xp[1]*x/(1 + x.coef['sigma'] * (x-1) + x.coef['kappa'] *x*(x-1)),
     type="l",lty="dashed",lwd=1, ylab="Throughput X(p)", xlab="6-way processors (p)")
points(df.numa$p, df.numa$Xp)
lines(df.numa$p, predict(usl)*df.numa$Xp[1], col='blue')
legend("bottom", legend=eval(parse(text=sprintf(
   "expression(sigma == %.4f, kappa == %.6f, R^2 == %.4f, pmax==%.2f, xmax==%.2f, Xroof==%.2f)",
   x.coef['sigma'], x.coef['kappa'], 1-sse/sst, pmax, xmax, df.numa$Xp[1]/x.coef['sigma'] ))), ncol=2)
abline(v=pmax)
abline(h=df.numa$Xp[1] / x.coef['sigma'])
title(main="USL Analysis of SGI Origin")

# multiple plots
op <- par(mfrow = c(2, 2)) # save old plot

plot(x <- c(0:64), df.numa$Xp[1]*x/(1 + x.coef['sigma'] * (x-1) + x.coef['kappa'] *x*(x-1)),
     type="l",lty="dashed",lwd=1, ylab="Throughput X(p)", xlab="N-way processors (p)")
points(df.numa$p, df.numa$Xp)
lines(df.numa$p, predict(usl)*df.numa$Xp[1], col='blue')

plot(x <- c(0:200), df.numa$Xp[1]*x/(1 + x.coef['sigma'] * (x-1) + x.coef['kappa'] *x*(x-1)),
     type="l",lty="dashed",lwd=1, ylab="Throughput X(p)", xlab="N-way processors (p)")
points(df.numa$p, df.numa$Xp)
lines(df.numa$p, predict(usl)*df.numa$Xp[1], col='blue')

plot(x <- c(0:500), df.numa$Xp[1]*x/(1 + x.coef['sigma'] * (x-1) + x.coef['kappa'] *x*(x-1)),
     type="l",lty="dashed",lwd=1, ylab="Throughput X(p)", xlab="N-way processors (p)")
points(df.numa$p, df.numa$Xp)
lines(df.numa$p, predict(usl)*df.numa$Xp[1], col='blue')

plot(x <- c(0:1000), df.numa$Xp[1]*x/(1 + x.coef['sigma'] * (x-1) + x.coef['kappa'] *x*(x-1)),
     type="l",lty="dashed",lwd=1, ylab="Throughput X(p)", xlab="N-way processors (p)")
points(df.numa$p, df.numa$Xp)
lines(df.numa$p, predict(usl)*df.numa$Xp[1], col='blue')

par(op) # reset

