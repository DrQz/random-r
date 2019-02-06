# Created by NJG on Tue Feb  5 15:15:35 2019
# 
# Blog post: What happens when you add a new teller?  21 October 2008
# https://www.johndcook.com/blog/2008/10/21/what-happens-when-you-add-a-new-teller/
#
# See my remarks in the Comments section of that blog post.
# Here, I've reproduced my original R code in PDQ.
# Download the current PDQ software (6.2.0) library from  
# http://www.perfdynamics.com/Tools/PDQcode.html

# A picture is worth a thousand blog words

library(pdq)


# Global queueing parameters
arrival.rate <- seq(0, 5.8, 0.25)  # cust per hr
service.time <- 10 / 60  # 10 minutes as fraction of hr
x.utiliz <- NULL
y.wtime1 <- NULL
y.rtime1 <- NULL
y.wtime2 <- NULL
y.rtime2 <- NULL

#########################################
### Fisrt case: single teller
#########################################
tellers <- 1

for(i in 1:length(arrival.rate)) {
  Init("Single Teller Plot")
  CreateOpen("deposit", arrival.rate[i])
  SetWUnit("Cust")
  SetTUnit("Hour")
  CreateMultiNode(tellers,"teller", CEN, FCFS)
  SetDemand("teller", "deposit", service.time)
  Solve(CANON)
  
  x.utiliz[i] <- GetUtilization("teller", "deposit", pdq::TRANS)
  y.rtime1[i] <- GetResidenceTime("teller", "deposit", pdq::TRANS)
  y.wtime1[i] <- GetResidenceTime("teller", "deposit", pdq::TRANS) - service.time
}

plot(x.utiliz, y.wtime1, 
     main="Constant Traffic Waiting Times", 
     xlab="Server utilization (%)",
     ylab="Waiting time (hrs)",
     type="b", col="blue", cex=0.75
     )


#########################################
### Second case: additional teller
#########################################
tellers <- 2

for(i in 1:length(arrival.rate)) {
  Init("Single Teller Plot")
  CreateOpen("deposit", arrival.rate[i])
  SetWUnit("Cust")
  SetTUnit("Hour")
  CreateMultiNode(tellers,"teller", CEN, FCFS)
  SetDemand("teller", "deposit", service.time)
  Solve(CANON)
  
  x.utiliz[i] <- GetUtilization("teller", "deposit", pdq::TRANS) 
  y.rtime2[i] <- GetResidenceTime("teller", "deposit", pdq::TRANS) 
  y.wtime2[i] <- GetResidenceTime("teller", "deposit", pdq::TRANS) - service.time
}

lines(x.utiliz, y.wtime2, type="b", col="blue3", pch=2, cex=0.75)

# Theoretical dual server curve
curve(service.time / (1 - x^2) - service.time, 
      from=0, to=1, col="gray", add=TRUE)

legend("topleft", legend=c("Single PDQ teller", "Dual PDQ tellers"),
       col=c("blue", "blue3"), lwd=c(1,1), pch=c(1,2),
       inset=0.05, cex=0.75
       )

text(0.5,2, "NJG on Tue Feb  5, 2019", col="lightgray")

