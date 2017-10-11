# Script 7 3 starter for students
# Introduction to Analytics Class
#Lakshman Arunachalam
#Thomas Alen
# Script 7.3 K Means Algorithm
# R data script for Assignment 7 3 - K Means on State data
# This contains some starter code to get the data set loaded
# It also contains some (possibly) helpful demonstrations of K-Means
#
# Your group should copy Part 1 of this script verbatim
# Your group may reuse any section of Part 2 it likes, or you may choose
# to write your own fresh code
#
# Part 1 starts here --------------------------------------------
# Set our working directory
getwd()
# Load the functions we will need

# dar2ed.scale.many thanks to Viswanathan
dar2ed.scale.many <- function (dat, column_nos)
{
  nms = names(dat)
  for (col in column_nos) {
    name = paste(nms[col], "_z", sep = "")
    dat[name] = scale(dat[, col])
  }
  cat(paste("Scaled", length(column_nos), "attribute(s)\n"))
  dat
}


# dar2ed.kmeans.plot - thanks to Viswanathan
dar2ed.kmeans.plot <- function (data, cols, num_clust = 10, seed = 9876) {
  n = length(names(data))
  dat = dar2ed.scale.many(data, cols)[, (n + 1):(n + length(cols))]
  # dat.scale = scale(dat)
  wss <- (nrow(dat) - 1) * sum(apply(dat, 2, var))
  for (i in 2:num_clust) {
    set.seed(seed)
    wss[i] <- sum(kmeans(dat, centers = i)$withinss)
  }
  plot(1:num_clust, wss, type = "b", pch = 5, xlab = "# Clusters",
       ylab = "Total within_ss across clusters")
}
# Use the state.x77 data set which comes with R
# http://www.inside-r.org/r-doc/datasets/state
# These numbers are all from 1977
# Make it a data frame so we can work with it easily
state.frame <- data.frame(state.x77)
print("Here is the head of the state.x77 data which comes with R")
print(head(state.frame))
# Note we are using row names here, so
# row.names(state.frame[1:5,])
# will give you the data for Alabama, Alaska, Arizona, Arkansas, California
# and
# state.frame["Alaska",] will give you the relevant information for Alaska
#
# You can also still use
# state.frame[1, ] to get the first row's data as well
print("Now we will scale the data")
state.scaled <- dar2ed.scale.many(state.frame, 1:8)
print(head(state.scaled))

#Part 2 Starts here -------------------

#K - means Clustring is been performed on (Murder, HS.Grad)
dare2ed.kmeans.plot(state.scaled, 13:14)
#From the plot, cluster 4 seems to be a feasible one.
#run K Means for our 4 clusters. and append the result back to our state.sclaed
fit <- kmeans(state.scaled[, 13:14], 4)
#what are the results of our k-means here?
print(fit)

#Append our clusters to our data frame
state.scaled$cluster <- fit$cluster
#let's see what we now have
#want to print only columns 13, 14, and 17
#because columns 13 and 14 are the standardized Murder/H.S Grade variables
#and column 17 is our new clustering variable
print("Here are columns 13, 14, and 17 from our state.scaled data")
print(head(state.scaled[,c(9, 10, 17)]))

#Extracting each cluster to see the statistical overview
state.1 <- state.scaled[state.scaled$cluster == 1,]
state.2 <- state.scaled[state.scaled$cluster == 2,]
state.3 <- state.scaled[state.scaled$cluster == 3,]
state.4 <- state.scaled[state.scaled$cluster == 4,]

print("Cluster 1 has the following result")
print(state.1)
print("Cluster 2 has the following result")
print(state.2)
print("Cluster 3 has the following result")
print(state.3)
print("Cluster 4 has the following result")
print(state.4)

#Calculating average population for each clusters

print("Cluster 1 average popualtion")
print(mean(state.1$Population))
print("Cluster 2 average popualtion")
print(mean(state.2$Population))
print("Cluster 3 average popualtion")
print(mean(state.3$Population))
print("Cluster 4 average popualtion")
print(mean(state.4$Population))


#To Print the centers
print(fit$centers)

#Plotting
plot(state.scaled$Murder, state.scaled$HS.Grad)

#Adding abbrevation
state.scaled$abb <- state.abb

#Makes plot more visible with text
text(state.scaled$Murder, state.scaled$HS.Grad, state.scaled$abb, col=state.scaled$cluster)

#Aggregating all the columns

print("Here's an aggregate of the states by cluster")
print(aggregate(state.scaled[,1:8], by = list(state.scaled$cluster), FUN="mean"))



