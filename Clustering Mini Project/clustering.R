# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))
library(cluster)
library(rattle)
library(NbClust)


# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df_wine <- scale(wine[-1])
summary(df_wine)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df_wine)

# Exercise 2:
#   * How many clusters does this method suggest?
#      ==> Method suggest cluster count as 3 i.e k=3.
#   * Why does this method work? What's the intuition behind it?
#      ==> sum of square error (SSE) plot is used to determine
#         appropriate k value. Plot indicates that there is distinct drop
#         when moving from 1 to 3 clusters. After 3 we can observe decrease in
#         drop off, this suggest a 3-cluster as solution
#         If we look at the original data it also contains 3 classes
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df_wine, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# ==> By Looking into graph we can say best number of cluster is 3 i.e k=3        

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans( df_wine, 3 )
str(fit.km)
# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster)
table(wine$Type)
table_clust_wine <- table(wine$Type, fit.km$cluster) # fit.km$cluster is the row entries
table_clust_wine # its a confusion matrix
# To find "fit.km" is a good cluster or not, using an adjusted Rank index 
# provided by the flexclust package.
library(flexclust)
randIndex(table_clust_wine)
# The adjusted Rand index provides a measure of the agreement between two  
# partitions, adjusted for chance. It ranges from -1 (no agreement) to 
# 1 (perfect agreement). Agreement between the wine varietal type and 
# the cluster solution is 0.9. So its a good clustering

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot( df_wine, fit.km$cluster, color = T, shade = T, 
          main='2D representation of the Cluster solution')
