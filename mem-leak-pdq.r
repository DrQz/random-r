# PDQ model of memory leakage (from GCAP 2018 student)
# Created by NJG on Sat Oct 20 12:50:01 2018

library(pdq)

### Globals ###
memWrites  <- 1000
writeTime  <- 0.002 # seconds
swpWrites  <- 0
swapTime   <- 0.004 # seconds
swapStart  <- 400   # pages
memUtil    <- NULL  # array for PDQ output
swpUtil    <- NULL  # array for PDQ output
diffUtil   <- NULL  # diff b/w utilizations
clipPlot   <- 10    # drop last few data points

####################################
# Memory consumption phase
####################################
for(i in 1:memWrites) {
  Init("Memory Consumption")
  
  # Memory page model
  CreateClosed("pageWrites", TERM, as.numeric(i), 1.0)
  CreateNode("Memory", CEN, FCFS)
  SetDemand("Memory", "pageWrites", writeTime)
  
  Solve(EXACT)
  memUtil[i] <- GetUtilization("Memory", "pageWrites", TERM)
}

plot(x=1:swapStart+50, y=memUtil[1:swapStart+50] * 100, 
     main="PDQ Model of Memory Leak",
     xlab="Memory pages", ylab="Resource consumption (%)",
     xli=c(0,memWrites), ylim=c(0,105),
     type="l", col="red", lwd=2
     )
lines(x=swapStart+50:memWrites, y=memUtil[swapStart+50:memWrites] * 100,  
      col="red", lwd=2, lty="dotted"
      )


####################################
# Swap out phase
####################################
for(i in 1:memWrites) {
  Init("Page swapping")
  
  # Swap rate
  swpWrites <- 0.8 * i # determines slope of delta curve (blue)
  
  CreateClosed("swpWrites", TERM, as.numeric(swpWrites), 1.0)
  CreateNode("SwapDev", CEN, FCFS)
  SetDemand("SwapDev", "swpWrites", swapTime)
  
  Solve(APPROX)
  swpUtil[i]  <- GetUtilization("SwapDev", "swpWrites", TERM)
}


# Phase shift by 450 pages on x-axis
diffUtil <- c(rep(NA, swapStart), swpUtil[1:(memWrites - swapStart)])

lines(x=1:(memWrites - clipPlot), 
      y=diffUtil[1:(memWrites - clipPlot)] * 100, 
      col="darkgreen", lwd=2, lty="dotted"
      )

lines(x=1:(swapStart + 50), y=rep(0,(swapStart + 50)), 
      col="darkgreen", lwd=2
      )

lines(x=(swapStart + 50):(memWrites - clipPlot), 
      y=diffUtil[(swapStart + 50):(memWrites - clipPlot)] * 100, 
      col="darkgreen", lwd=2
      )

lines(x=1:(memWrites-clipPlot), 
      y=(memUtil - diffUtil)[1:(memWrites - clipPlot)] * 100, 
      col="red", lwd=2, lty="dotted"
      )

lines(x=(swapStart + 50):(memWrites - clipPlot), 
      y=(memUtil - diffUtil)[(swapStart + 50):(memWrites - clipPlot)] 
      * 100, col="red", lwd=2
      )

abline(v=450, col="gray") # discontinuity line
arrows(x0=450,y0=88,x1=450,y1=73,col="red",lwd=2,angle=20,length=0.1)
arrows(x0=450,y0=0,x1=450,y1=15,col="darkgreen",lwd=2,angle=20,length=0.1)

text(250,50, "Accumulating RAM pages", cex=0.75)
text(730,35, "Active RAM pages", cex=0.75)
text(770,80, "Reclaimed\n(inactive) pages\non swap device", cex=0.75)
text(550,7, "Page swapping\ncommences", cex=0.75)

text(200,90, "(c) 2018 Performance Dynamics",col="gray",cex=0.75)
