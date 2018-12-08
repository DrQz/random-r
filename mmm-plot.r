# Plot M/M/m response time curves for increasing m servers
# Created by NJG on Thursday, July 23, 2009

library(pdq)

# PDQ globals
servers <- c(1, 2, 4, 16, 64, 128)
stime   <- 0.499
node    <- "qnode"
work    <- "qwork"

for (m in servers) {
	# reset these variables
	arate <- 0
	xvals <- 0
	yvals <- 0
		
	for (i in 1:200) {
		pdq::Init("")
		
		arate   <- arate + 0.01
		aggrate <- m * arate 		# remember Erlang!
		
		pdq::CreateOpen(work, aggrate)
		pdq::CreateMultiNode(m, node, CEN, FCFS)
		pdq::SetDemand(node, work, stime)
		pdq::Solve(CANON)
		xvals[i] <- as.double(aggrate * stime / m)                  # rho
		yvals[i] <- pdq::GetResidenceTime(node,work,TRANS) / stime  # stretch
	}
		
	if (m == 1) {
		# draw plot frame with first curve
		plot(xvals, yvals, type="l", ylim=c(0,10), lwd=2, col="blue", 
			xlab=expression(paste("Server utilization ",rho)), ylab="Stretch factor R/S")
		title("M/M/m Response Time Profiles")
		today <- paste("Created on", date())
		text(0.25, 7, today, cex=0.75)	
		text(0, 9, "www.perfdynamics.com/Tools/PDQ-R.html", adj=c(0,0), col="blue", cex=0.75)
		
		# left-justified legend
		text(0.1, 6, paste("m =", paste(servers[1:6], collapse=',')), adj=c(0,0))
		text(0.1, 5.5, "PDQ exact solution", col="blue", adj=c(0,0))
		text(0.1, 5, "Morphing approximation", col="pink3", adj=c(0,0))
		
		abline(h=c(1), col = "black",lwd=2)
		abline(h=c(2, 4, 10), col = "gray")
		abline(v=c(0.50, 0.75, 0.90), col = "gray")
		
		points(c(0.5, 0.75, 0.90), c(2, 4, 10), cex=1.5)
		
		# morphing approximation of PPDQ book
		points(xvals, 1 / (1 - xvals^m), lty="dotted", col="pink3", cex=0.5) 
		
	} else {
	  
		# overlay other curves
		points(xvals, yvals, type="l", lwd=2, col="blue") 
		points(xvals, 1 / (1 - xvals^m), lty="dotted", col="pink3", cex=0.5) 
		
	}
}
