# Compare Moeding usl() function with new 3-param USL model
# No X(1) value required
# No C(N) normalization required
# Gamma param (G) == X(1)
# Then compute C(N) and E(N) as for 2-param USL
#
# Created by NJG on Sat Nov 17 21:00:04 2018
# Updated by NJG on Sun Nov 18 07:48:10 2018

library(usl)

RUN_FAIL <- FALSE  # global flag

##############################
# From vignette 
##############################
data(raytracer)
raytracer
plot(throughput ~ processors, data = raytracer)
usl.moeding <- usl(throughput ~ processors, data = raytracer)
summary(usl.moeding)
plot(usl.moeding, add = TRUE)

# Coefficients:
# Estimate  Std. Error  t value  Pr(>|t|)     
# sigma  5.002e-02   3.209e-03   15.587  8.08e-08  ***
# kappa  4.708e-06   6.923e-05    0.068     0.947     


# cf. NLS fit to 2-param USL 
usl2.njg <- nls(throughput ~ throughput[1] * processors / 
                 (1 + alpha * (processors - 1) + 
                    beta * processors * (processors - 1)), 
               data=raytracer, algorithm="port", 
               start=list(alpha=5e-2, beta=2e-6),
               lower=c(0, 0), upper=c(10e-2, 5e-6)
)
summary(usl2.njg)

# Parameters:
# Estimate Std. Error t value Pr(>|t|)    
# alpha 5.007e-02  3.213e-03  15.585 8.09e-08 ***
# beta  5.000e-06  6.932e-05   0.072    0.944    


##############################
# No X(1) data 
##############################
df.njg <- read.csv(text=" N,X
                  4,78
                  8,130
                  12,170
                  16,190
                  20,210
                  24,220
                  28,245
                  32,255
                  48,280
                  64,310",
                  header=TRUE
)
print(df.njg)

plot(X ~ N, data=df.njg)

if (RUN_FAIL) {
  usl.njg <- usl(X ~ N, data=df.njg)  # Fail
}

# Fit to 3-param USL 
usl3.njg <- nls(X ~ gamma * N / (1 + alpha * (N - 1) + beta * N * (N - 1)), 
               data=df.njg, algorithm="port", 
               start=list(gamma=78/4, alpha=5e-2, beta=2e-6),
               lower=c(10, 0, 0), upper=c(30, 10e-2, 5e-6)
)
summary(usl3.njg)

# Regression stats differ from vignette due to extra 
# degree of freedom introduced by gamma coefficient.
# Also true if X(1) is included for 3-param USL.
# Higher value for estimated X(1) => zero beta coeff.

if (RUN_FAIL) {
  plot(usl3.njg, add = TRUE)  # Fail
}

A <- coef(usl3.njg)['alpha']   # Contention coefficient
B <- coef(usl3.njg)['beta']    # Coherency coefficient 
G <- coef(usl3.njg)['gamma']   # Estimated X(1) value

plot(df.njg$N, df.njg$X, type="p",
     xlim=c(0, 64), ylim=c(0, 400),
     main="USL3 model with missing X(1) value",
     xlab="Processors", ylab="Throughput"
)

# overlay the USL curve
curve(G * x / (1 + A * (x - 1) +  B * x * (x - 1)), 
      from=0, to=100, add=TRUE, col="blue", lwd=2
)

# compute USL scaling metrics
Nmax  <- sqrt((1 - A) / B)
Xmax  <- G * Nmax / (1 + A * (Nmax - 1) + B * Nmax * (Nmax - 1))
Nopt  <- abs(1 / A)
Xroof <- G * Nopt 

abline(h=Xroof, col="red")
abline(a=0, b=G, col="red")
abline(v=Nopt, col="gray", lty="dashed")

legend("bottom", legend=eval(parse(text=sprintf("expression(alpha == %.6f, beta == %.6f, gamma == %.6f, Nopt==%.2f, Xmax==%.2f,  Xroof==%.2f)", A, B, G, Nopt, Xmax, Xroof))), ncol=2, inset=0.05, cex=0.75)

