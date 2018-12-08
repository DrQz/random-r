
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

# NLS fit for USL model
usl <- nls(Cp ~ p / (1 + alpha*(p-1) + beta*p*(p-1)), 
           data=df.numa, 
           start=c(alpha=0.1, beta=0.01)
           )
summary(usl)
x.coef <- coef(usl)

# Determine sum-of-squares for R-squared coeff from NLS fit
sse <- sum((df.numa$Cp - predict(usl))^2)
sst <- sum((df.numa$Cp - mean(df.numa$Cp))^2)

pmax <- sqrt((1 - x.coef['alpha']) / x.coef['beta'])
xmax <- df.numa$Xp[1]*pmax / (1 + x.coef['alpha']*(pmax - 1) + x.coef['beta']*pmax*(pmax - 1))


# Plot all the results
plot(df.numa$p, df.numa$Xp, stype="p",
     xlim=c(0,100), ylim=c(0,350),
     main="USL Using NLS",
     xlab="Processors (p)", ylab="Throughput X(p)"
     )

curve(df.numa$Xp[1] * x / (1 + x.coef['alpha'] * (x-1) + x.coef['beta'] * x * (x-1)), 
      from=0, to=100,
      add=TRUE, col="blue"
      )

legend("bottom", legend=eval(parse(text=sprintf(
   "expression(alpha == %.4f, beta == %.6f, R^2 == %.4f, pmax==%.2f, xmax==%.2f, Xroof==%.2f)",
   x.coef['alpha'], x.coef['beta'], 1-sse/sst, pmax, xmax, 
   df.numa$Xp[1]/x.coef['alpha'] ))), ncol=2, inset=0.05, cex=0.75
   )


