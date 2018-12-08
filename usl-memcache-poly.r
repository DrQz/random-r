# Created by NJG on Tue Oct 23 07:42:37 2018
# Check 2nd degree polynomial with a,b,c in Excel
# when X(1) data is missing

# N is configured number of memcache worker threads 
# First, reproduce Excel results using lm()
df.mc.poly <- read.csv(text=" N,X
                  2,152.22
                  4,242.49
                  6,278.63
                  8,269.67
                  10,250.36
                  12,241.69", 
                  header=TRUE
)
print(df.mc.poly)

df.mc.poly$X.user   <- df.mc.poly$X / df.mc.poly$N
df.mc.poly$Xu.inv   <- 1 / df.mc.poly$X.user
df.mc.poly$N.minus1 <- df.mc.poly$N - 1 # must do this way
print(df.mc.poly)

usl.mc.poly <- lm(Xu.inv ~ N.minus1 + I(N.minus1^2), data=df.mc.poly)
#usl.mc.poly <- lm(Xu.inv ~ N + I(N^2), data=df.mc.poly)
summary(usl.mc.poly)

c <- coef(usl.mc.poly)[1] # intercept
b <- coef(usl.mc.poly)[2] # N coeff
a <- coef(usl.mc.poly)[3] # N^2 coeff

curve(a * x^2 + b* x + c, from=0, to=11, col="blue")

A <- (b - a) / c
B <- a /c
G <- 1 / c

plot(df.mc.poly$N, df.mc.poly$X, 
     xlim=c(0,12), ylim=c(0,300),
     main="USL3 Polynomial model of Memcache",
     xlab="Worker threads (N)",ylab="Throughput in KOPS X(N)"
     )
curve(G * x / ( 1 + A * (x-1) + B * x * (x-1) ), 
      from=0, to=12, 
      col="blue", add=TRUE
      )

# compute USL scaling metrics
Nmax  <- sqrt((1 - A) / B)
Xmax  <- G * Nmax / (1 + A * (Nmax - 1) + B * Nmax * (Nmax - 1))
Nopt  <- abs(1 / A)
Xroof <- G * Nopt 

abline(v=Nmax,col="gray",lty="dashed")
abline(h=Xmax,col="gray",lty="dashed")
abline(b=G, a=0, col="gray")

legend("bottom",
  legend=eval(parse(text=sprintf("expression(alpha == %.4f, beta == %.6f, gamma == %.2f, Nmax==%.2f, Xmax==%.2f,  R^2==%.4f)", 
A, B, G, Nmax, Xmax, summary(usl.mc.poly)$r.squared))), 
ncol=2, inset=0.05, cex=0.75
)

