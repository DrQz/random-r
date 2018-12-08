# Track the convergence of the Bayesian posterior peak
# starting with a Jeffreys prior and coin bias p = 1/4.
# Created by NJG on Tuesday, March 29, 2016

SimPeakPath<- function(p, N) {
  a <- 1/2
  b <- 1/2
  heads  <- 0
  path.x <- 0
  path.y <- 0
  y.max  <- 20
  
  curve(dbeta(x, a, b), xlim=c(0,1), ylim=c(0, y.max), 
        xlab="H", ylab="Updated posterior", 
        col="green", lwd=2, lty=3)
  abline(v=p)
  path.x[1] <- (a - 1) / (a + b - 2)
  path.y[1] <- 0
  
  prand <- runif(N)
  for(i in 1:N) {
    if(prand[i] <= p) { heads <- heads + 1 }
    a <- heads + 1
    b <- (i - heads) + 1
    curve(dbeta(x, a, b), col="lightgrey", add=TRUE)
    path.x[i+1] <- (a - 1) / (a + b - 2)
    path.y[i+1] <- i / (N / y.max)
  }

  curve(dbeta(x, a, b), col="red", lwd=2, add=TRUE)
  path.x[N+1] <- (a - 1) / (a + b - 2)
  path.y[N+1] <- i / (N / y.max)
  lines(path.x, path.y, lwd=1, col="blue")
}


# Test function
SimPeakPath(p=0.25, N=64)


# Plot array
rows <- 2
cols <- 3
toss <- 256
sims <- rows * cols

op <- par(no.readonly=TRUE)
par(mfrow=c(rows, cols))
par(mar=c(2.5, 3.5, 1, 0.5))
par(mgp=c(1.5, 0.5, 0))
par(oma=c(0, 0, 3, 0))
for(i in 1:sims) { 
  SimPeakPath(p=0.25, N=toss)
  abline(v=0.25)
  text(0.75, 14, paste(toss, "Trials"), cex=0.75)
}
mtext(text="Convergence of Bayesian posteriors for p = 1/4", 
      outer=TRUE, side=3, cex=1.2, line=1)
par(op)


