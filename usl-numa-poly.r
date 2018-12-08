# Created by NJG on Tue Oct 23 17:06:01 2018
# Emulate USL polynomial fit in Excel per GCAP book p.80 ff

# SGI Origin running ray-tracing workload
df.numa.poly <- read.table(text="p	X
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
  header=TRUE
)
print(df.numa.poly)

# Normalize data and check efficiency
df.numa.poly$Cp <- df.numa.poly$X / df.numa.poly$X[1]
df.numa.poly$Ep <- df.numa.poly$Cp / df.numa.poly$p
df.numa.poly$Ep.inv1 <- (1 / df.numa.poly$Ep) - 1 # must do this way
df.numa.poly$p.minus1 <- (df.numa.poly$p - 1) # must do this way
print(df.numa.poly)

# Inverse poly fit for USL model
usl.poly <- lm(Ep.inv1 ~ 0 + p.minus1 + I(p.minus1^2), data=df.numa.poly)
summary(usl.poly)

b <- coef(usl.poly)[1] # N coeff
a <- coef(usl.poly)[2] # N^2 coeff

# From GCAP book p.80
A <- (b - a)
B <- a 

pmax <- sqrt((1 - A) / B)
xmax <- df.numa.poly$X[1] * pmax / 
  (1 + A * (pmax - 1) + B * pmax * (pmax - 1))

plot(df.numa.poly$p, df.numa.poly$X, type="p",
     xlim=c(0, 100), ylim=c(0, 350),
     main="USL using Excel Inverted Polynomial",
     xlab="Processors (p)", ylab="Throughput X(p)"
     )

curve(df.numa.poly$X[1] * x / (1 + A * (x - 1) + B * x * (x - 1)), 
      from=0, to=100,
      add=TRUE, col="blue"
      )

legend("bottom", legend=eval(parse(text=sprintf(
   "expression(alpha == %.4f, beta == %.6f, R^2 == %.4f, pmax==%.2f, xmax==%.2f, p_opt==%.2f)",
   A, B, summary(usl.poly)$r.squared, pmax, xmax, 1 / A ))), 
   ncol=2, inset=0.05, cex=0.75
   )


