# Created by NJG on Fri Dec  7 15:13:38 2018

# W3C time formats https://www.w3schools.com/tags/att_time_datetime.asp
ttag <- function() {
  ts <- Sys.time()
  tt <- paste("Published on <time datetime=", 
              sprintf("\"%s\"", ts), 
              ">", ts, "</time>",  
              sep="")
  return(tt)
}

lines <- readLines("timetag.html", -1) # read to EOF
linum <- grep("</html>", lines, ignore.case=TRUE)

# We don't know what immediatey precedes </html> 
# Overwrite it with TS and rewrite </html> on next line
lines[linum]     <- ttag()  
lines[linum + 1] <- "</html>"
writeLines(lines, "timetag-out.html") # don't overwrite read file
