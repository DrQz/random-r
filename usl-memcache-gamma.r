# Created by NJG on Mon Oct 22 18:39:24 2018

# N is configured number of memcache worker threads 
df.mc <- read.csv(text=" N,X
                  2,152.22
                  4,242.49
                  6,278.63
                  8,269.67
                  10,250.36
                  12,241.69", 
                  header=TRUE
                  )
head(df.mc)

plot(df.mc$N,df.mc$X, type="p",
     xlim=c(0,12), ylim=c(0,300),
     main="USL3 model of Memcache using NLS",
     xlab="Worker threads (N)",
     ylab="Throughput in KOPS X(N)"
     )

# Fit the nonlinear USL model
usl.fit <- nls(X ~ gamma * N / 
                 (1 + alpha * (N - 1) + beta * N * (N - 1)), 
               data=df.mc, algorithm="port", 
               start=list(gamma=100, alpha=1e-2, beta=1e-2),
               lower=c(50,0,0), upper=c(200,5e-1,5e-1)
               )
summary(usl.fit)

A <- coef(usl.fit)['alpha']
B <- coef(usl.fit)['beta']
G <- coef(usl.fit)['gamma']

# overlay the USL curve
curve(G * x / (1 + A * (x - 1) +  B * x * (x - 1)), 
from=0, to=12, add=TRUE, col="blue", lwd=2
)

# compute USL scaling metrics
Nmax  <- sqrt((1 - A) / B)
Xmax  <- G * Nmax / (1 + A * (Nmax - 1) + B * Nmax * (Nmax - 1))
Nopt  <- abs(1 / A)
Xroof <- G * Nopt 

abline(v=Nmax,col="gray",lty="dashed")
abline(h=Xmax,col="gray",lty="dashed")
abline(b=coef(usl.fit)['gamma'], a=0, col="gray")

legend("bottom",
       legend=eval(parse(text=sprintf("expression(alpha == %.4f, 
      beta == %.6f, gamma == %.2f, Nmax==%.2f, Xmax==%.2f,  Xroof==%.2f)", 
       A, B, G, Nmax, Xmax, Xroof))), 
       ncol=2, inset=0.05, cex=0.75
       )
