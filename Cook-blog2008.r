# Created by NJG on 4 May 2009 at 15:47
# Updated by NJG on Tue Feb  5 15:15:35 2019
# 
# Blog post: What happens when you add a new teller?  21 October 2008
# https://www.johndcook.com/blog/2008/10/21/what-happens-when-you-add-a-new-teller/
#
# See my remarks in the Comments section of that blog post.
# Here, I've reproduced my original R code in PDQ.
# Download the current PDQ software (6.2.0) library from  
# http://www.perfdynamics.com/Tools/PDQcode.html

library(pdq)

# Neil Gunther
# 4 May 2009 at 15:47
# This example is dramatic because the lone teller is already near saturation
# (arrival rate ~= service rate) and the queue is growing unbounded. So naturally,
# adding another teller will halve the utilization of each and reduce the queue
# size significantly.


# Global queueing parameters
arrival.rate <- 5.8      # customers per hr
service.time <- 10 / 60  # 10 minutes

#########################################
### Fisrt case: single teller
#########################################
tellers <- 1

Init("Single Bank Teller")

#define the workflow
CreateOpen("deposit", arrival.rate)
SetWUnit("Cust")
SetTUnit("Hour")

#define an m-server queue (here m = 2)
CreateMultiNode(tellers,"teller", CEN, FCFS)

#define the service demand on the server
SetDemand("teller", "deposit", service.time)
Solve(CANON)

Report()  # single teller is busy more than 96% of the time

# Get the waiting time metric from PDQ
wait.time1 <- GetResidenceTime("teller", "deposit", pdq::TRANS) - service.time
cat(sprintf("W1 = %.2f hrs or %.2f mins\n", wait.time1, wait.time1 * 60))


#########################################
### Second case: additional teller
#########################################
tellers <- 2

Init("Two Bank Tellers")

#define the workflow
CreateOpen("deposit", arrival.rate)
SetWUnit("Cust")
SetTUnit("Hour")

#define an m-server queue (here m = 2)
CreateMultiNode(tellers,"teller", CEN, FCFS)

#define the service demand on the server
SetDemand("teller", "deposit", service.time)
Solve(CANON)

Report() # each teller is only busy 48% of the time

# Get the waiting time metric from PDQ
wait.time2 <- GetResidenceTime("teller", "deposit", pdq::TRANS) - service.time
cat(sprintf("W2 = %.2f hrs or %.2f mins\n", wait.time2, wait.time2 * 60))

