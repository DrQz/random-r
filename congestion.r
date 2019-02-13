# PDQ model of "Congestion charging… in IT?"
# in Call Center (standard M/M/m queue)
# https://evolvingitsm.com/2013/03/20/congestion-charging-in-it/
#
# Created by NJG on Wednesday, February 13, 2019

library(pdq)

arate  <- 1.90 / 2  # Can't have 2 calls / 2 minutes !!! 
stime  <- 4         # minutes 
agents <- 4

# check bounds
as.logical(arate * stime / agents <  1) 

Init("Call Congestion")
CreateOpen("Calls", arate)
CreateMultiNode(agents, "Agent", FCFS, CEN)
SetDemand("Agent", "Calls", stime)
SetWUnit("Calls")
SetTUnit("Min.")
Solve(CANON)
Report()

