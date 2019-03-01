# Created by NJG on Thu Feb 28 20:25:53 PST 2019
#
# Exponential functions
# 	Anything raised to the power 0 = 1
# 	Anything raised to the power 1 = itself

plot(0, 0, pch='.', las=1, 
     xlim=c(-1, 2), ylim=c(0, 10), 
     xlab="x", ylab="f(x)",
     main="Exponential Functions in R"
     )

# The Exponential function, exp() in R, 
# shows where the magic number e^1 = 2.71828 comes from
curve(exp(x), from=-1, to=2, col="blue",  add=TRUE)

abline(v=0)
abline(v=1, col="green", lwd=1)
abline(h=1, col="gray")

segments(x0=0, y0=2, x1=1, y1=2, col="red", lwd=2)
segments(x0=0, y0=exp(1), x1=1, y1=exp(1), col="red", lwd=2)
segments(x0=0, y0=10, x1=1, y1=10, col="red", lwd=2)

# Binary exponential function (for comparison)
curve(2^x, from=-1, to=2, col="blue", add=TRUE)

# Decenary exponential function  (for comparison)
curve(10^x, from=-1, to=2, col="blue", add=TRUE)

text(-0.05, 9.9, "10.0", adj=c(1, 0), cex=0.75)
text(-0.05, 3, "2.718282", adj=c(1, 0), cex=0.75)
text(-0.05, 2, "2.0", adj=c(1, 0), cex=0.75)

text(0.75, 8, bquote(10^x))
text(1.5, 5, bquote(e^x))
text(2, 3.5, bquote(2^x))

