# Word cloud constructoin in R
# Example from 
# https://datascienceplus.com/building-wordclouds-in-r
#
# Word cloud image will appear in the RStudio plot window
# BEWARE: it may take a minute or more to render
#
# Created by NJG on Thu May  2 10:45:46 PDT 2019

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Create a DF
jeopQ <- read.csv("https://datascienceplus.com/wp-content/uploads/2015/08/JEOPARDY_CSV.csv", stringsAsFactors=FALSE)


# Create a List object from Question column in CSV data
# BUG:
#   There's a problem using Corpus() in the original example.
#   Causes "transformation drops documents" WARNINGS in tm-map() 
#   and the following error when wordcloud() is called:
#   "Error in simple_triplet_matrix(i, j, v, ... 'i, j' invalid"
#   Solution is to call VCorpus() instead
#   See Comment by Halake on Jan 19, 2018
#   https://github.com/OhLookCake/xkcd-Topics/issues/1
jeopCorpus <- VCorpus(VectorSource(jeopQ$Question))

# Converst list object to lc, remove stop-word, etc...
jeopCorpus <- tm_map(jeopCorpus, content_transformer(tolower))
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords("english"))

# Perform word stemming
jeopCorpus <- tm_map(jeopCorpus, stemDocument)

# Plot the B&W wordcloud
wordcloud(jeopCorpus, max.words=100, random.order=FALSE)

# Now add some coloe
wordcloud(jeopCorpus, 
          max.words=100, 
          random.order=FALSE,
          colors=brewer.pal(8, "Dark2")
          )







