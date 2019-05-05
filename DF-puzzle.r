#  Created by NJG on Sat May  4 15:32:08 PDT 2019
# 
# See the background story in this Twitter thread 
# https://twitter.com/DrQz/status/1124726941158789120

### Create a test data frame (DF)
col1 <- c(10, 20, 30)
col2 <- c("a", "b", "c")
col3 <- c(0, 2877, 0)
col4 <- c(300, 200, 100)

df.puzz <- data.frame(col1, col2, col3, col4)
df.puzz  # check that it's correct

### PROBLEM
# Test 1: why does this statement NOT fail with an error conditon
# rather than execute? (correctly, btw)
df.puzz[2, 3, 4 ]

# A student wrote that code.
# Otherwise, I would never have known about this behavior.
# Reminder: a DF is a 2-dimensional object (like a matrix) in R


# Test 2: this statement DOES fail with an error (as expected)
df.puzz[2, 3, 4, ]


# It turns out the 3rd index is a BOOLEAN for dropping the dimensional
# representation in a matrix or a DF.
#
# See e.g., 9.2.1 Dropping matrix dimensions  of
# https://bookdown.org/rdpeng/rprogdatascience/subsetting-r-objects.html
#
# The official R documentation is not easy to find, either
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/Extract.data.frame.html
# In the Help tab of RStudio, you have to type "Extract" (note the upper case 'E')
# in order to arrive at the correct page. That doesn't exactly spring to mind in
# this contenxt.


# Test 3: 3rd "index" is equivalent to drop=FALSE
df.puzz[2, 3, 0]   # NOTE the row and col IDs

### CONCLUSION
# Any non-zero numeric value after the 2nd comma is treated as drop=TRUE.
# Who knew a BOOEAN was a DF index!??
# Ploymorphisms are cool but mixing metphors is a bad idea.
# At least it fails SAFELY.

