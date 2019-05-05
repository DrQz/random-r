#  Created by NJG on Sat May  4 15:32:08 PDT 2019
# 
# See Twitter thread for the background 
# https://twitter.com/DrQz/status/1124726941158789120

### Create a test DF
col1 <- c(10,20,30)
col2 <- c("a","b","c")
col3 <- c(0,2877,0)
col4 <- c(300,200,100)

df.puzz <- data.frame(col1, col2, col3, col4)
df.puzz  # check it's there

# Test 1: why doesn't this fail?
# A student wrote this code. 
# Otherwise, I would never known about it.
df.puzz[2,3,4] 

# Test 2: this DOES fail
df.puzz[2,3,4,] 

# The 3rd index is a BOOLEAN for dropping the matrix or the DF 
# dimensional representation. 

# See 9.2.1 Dropping matrix dimensions  of 
# https://bookdown.org/rdpeng/rprogdatascience/subsetting-r-objects.html

# Test 2: 3rd "index" is equivalent to drop=FALSE
df.puzz[2,3,0]   # NOTE the row and col IDs

### CONCLUSION
# Any non-zero is treated as drop=TRUE.
# Who knew a BOOEAN was a DF index!??


