# Based on JD Long's (https://twitter.com/cmastication) blog post 
# of 2012 about solving a number puzzle by the novel application 
# of linear regression.
# The key idea is to use a frequency table of the digits.
# It can be thought of as a kind of crypto cipher 
#
# Modified by NJG on November 24, 2020

# The puzzle comprises 20 4-digit numbers that are 
# associated via an "=" sign with another single digit. Thusly, 
#
#   1   8809 = 6
#   2   7111 = 0
#   3   2172 = 0
#   4   6666 = 4
#       .... 
#  19   5531 = 0
#  20   2581 = ?
#
# Question: what number is represented by "?" in the last row?


# JD Long uses the following dataframe format: 
#   + no '=' character
#   + no '?' character
#   + pattern digit dx appears in the last column
df.pattern <- read.csv(text="d1,d2,d3,d4,dx
8,8,0,9,6
7,1,1,1,0
2,1,7,2,0
6,6,6,6,4
1,1,1,1,0
3,2,1,3,0
7,6,6,2,2
9,3,1,3,1
0,0,0,0,4
2,2,2,2,0
3,3,3,3,0
5,5,5,5,0
8,1,9,3,3
8,0,9,6,5
7,7,7,7,0
9,9,9,9,4
7,7,5,6,1
6,8,5,5,3
9,8,8,1,5
5,5,3,1,0",
header=TRUE)
dim(df.pattern) # should be 20 rows

## DJL: "Munge the data to create a frequency table"
freqTable <- as.data.frame( 
  t(apply(df.pattern[ , 1:4], 1, function(x) table(c(x, 0:9))-1)) 
  )
# NJG: name the columns in the freq table
names(freqTable) <- 
  c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9")

# NJG: Append the pattern digits from col 5 in the dataframe
# to the frequency table as column called "dep".
# This becomes the response variale while the digits {d1,d,2..,d9} 
# are the independent regressors.
# JDL uses "dep" to refer to the dependent variable.
# freqTable$dep <- df.pattern[ , 5]
# I prefer to call it d.hat: the estimator.
freqTable$d.hat <- df.pattern[ , 5]

## DJL: "Now a simple OLS regression with no intercept"
# He uses the regression coefficients to reveal how much of each 
# digit appears in the d.hat response variable.
model.fit <- 
  lm(d.hat ~ 0 + d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9, 
      data=freqTable
     )

# NJG: If we look at the summary stats, it WARNS that it's something
# of ab abuse of LSQ: "essentially a perfect fit", since the coeffs 
# are INTEGERS, 0, 1, 2. 
# summary(model.fit) 

print(round(model.fit$coefficients))

# CONCLUSION as stated by JD Long:
#   "We can see that d0, d6, and d9 all get mapped to 1 
#   and d8 gets mapped to 2. Everything else is zero. 
#   And d4 is NA because there were no fours in the training data."


