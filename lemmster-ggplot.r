# Created by NJG on Wed Oct 26 09:30:28 2016

library(ggplot2)
library(scales)

##############
# No lock data
##############
df.nlk <- read.csv("lockfree.csv")
df.nlk <- transform(df.nlk, Throughput = Insertions / Duration)
df.nlk <- aggregate(Throughput ~ Processors, data=df.nlk, FUN=function(x) c(M=mean(x)))
names(df.nlk) <- c("p", "Xp")
df.nlk$Cp <- df.nlk$Xp / df.nlk$Xp[1]
df.nlk$Ep <- df.nlk$Cp / df.nlk$p

usl.nlk <- nls(Cp ~ p/(1 + sigma * (p-1) + kappa * p * (p-1)), 
               data=df.nlk, start=c(sigma=0.1, kappa=0.01)
               )
print(coef(usl.nlk))
print("Superlinear: negative contention coeff. Measurement error?")

# double-log ggolot 
nlk.curves     <- with(df.nlk, expand.grid(p=seq(min(p), max(p), length=150)))
nlk.curves$fit <- df.nlk$Xp[1] * predict(usl.nlk, newdata=nlk.curves)

p.nlk <- qplot(p, Xp, data=df.nlk) + 
  geom_point(colour="blue") +
  geom_smooth(aes(y=fit), data=nlk.curves, stat="identity") +
  scale_x_log10() + 
  scale_y_log10() +
  annotation_logticks() + 
  labs(x="Log p", y="Log X(p)") + 
  annotate('text', 9, 1e5, parse=TRUE, 
           label="nLocks:~sigma==0.0003275634~and~kappa==0.0000109649")


##############
# Locking data
##############
df.lok <- read.csv("locks.csv")
df.lok <- transform(df.lok, Throughput = Insertions / Duration)
df.lok <- aggregate(Throughput ~ Processors, data=df.lok, FUN=function(x) c(M=mean(x)))
names(df.lok) <- c("p", "Xp")
df.lok$Cp <- df.lok$Xp / df.lok$Xp[1]
df.lok$Ep <- df.lok$Cp / df.lok$p

usl.lok <- nls(Cp ~ p/(1 + sigma * (p-1) + kappa * p * (p-1)), 
               data=df.lok, start=c(sigma=0.1, kappa=0.01)
               )
print(coef(usl.lok))

# add locks data and USL curve to existing plot
lok.curves     <- with(df.lok, expand.grid(p=seq(min(p), max(p), length=150)))
lok.curves$fit <- df.lok$Xp[1] * predict(usl.lok, newdata=lok.curves)
p.both <- p.nlk + geom_point(data=df.lok, colour="red") + 
  geom_smooth(aes(y=fit), data=lok.curves, colour="red", stat="identity") +
  annotate('text', 9, 4e3, parse=TRUE, 
           label="Locks:~sigma==0.1028739534~and~kappa==0.0005902645") + 
  ggtitle("USL Analysis of Locks vs. No locks") + 
  theme(plot.title=element_text(lineheight=.8, face="bold"))
print(p.both)

