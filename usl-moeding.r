# Compare Moeding usl() function with new 3-pram USL model
# No X(1) value required
# No C(N) normalization required
# Gamma param (G) = X(1)
# Then, can computer C(N) and E(N) as for 2-param USL
# Created by NJG on Sat Nov 17 21:00:04 2018

library(usl)

RUN_FAIL <- FALSE  # global flag

# From vignette
data(raytracer)
raytracer
plot(throughput ~ processors, data = raytracer)
usl.moeding <- usl(throughput ~ processors, data = raytracer)
summary(usl.moeding)
plot(usl.moeding, add = TRUE)



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
head(df.njg, 10)

plot(X ~ N, data=df.njg)

if (RUN_FAIL) {
  usl.njg <- usl(X ~ N, data=df.njg)  # Fail
}

# Fit to 3-param USL 
usl.njg <- nls(X ~ gamma * N / (1 + alpha * (N - 1) + beta * N * (N - 1)), 
               data=df.njg, algorithm="port", 
               start=list(gamma=78/4, alpha=5e-2, beta=2e-6),
               lower=c(10, 0, 0), upper=c(30, 10e-2, 5e-6)
)
summary(usl.njg)

# Regression results differ from known X(1) case

if (RUN_FAIL) {
  plot(usl.njg, add = TRUE)  # Fail
}

A <- coef(usl.njg)['alpha']   # Contention parameter
B <- coef(usl.njg)['beta']    # Coherency parameter 
G <- coef(usl.njg)['gamma']   # Estimated X(1) value

plot(df.njg$N, df.njg$X, type="p",
     xlim=c(0, 64), ylim=c(0, 350),
     main="USL3 model",
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

legend("bottom",
       legend=eval(parse(text=sprintf("expression(alpha == %.6f, 
   beta == %.6f, gamma == %.6f, Nmax==%.2f, Xmax==%.2f,  Xroof==%.2f)", 
                                      A, B, G, Nmax, Xmax, Xroof))), 
       ncol=2, inset=0.05, cex=0.75
       )

