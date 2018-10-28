# Eva-Marie Costello - K-Means clustering mini-project
## Before starting the exercise - loaded necessary packages and downloaded the dataset
install.packages(c("cluster", "rattle.data","NbClust"))
data(wine, package="rattle.data")
head(wine)
## Exercise 1 - Remove the first colum from the data and scale it
wine <- wine[-grep('Type', colnames(wine))]
View(wine)
scale(wine)
## Exercise 2:
###   * How many clusters does this method suggest?
###   * Why does this method work? What's the intuition behind it?
###   * Look at the code for wssplot() and figure out how it works
### Method 1 - clustering
library(NbClust)
set.seed(1234)
nc <- NbClust(wine, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
          + xlab="Number of Clusters", ylab="Number of Criteria",
          + main="Number of Clusters Chosen by 26 Criteria")
## Exercise 3 and 4  - How many clusters does this method suggest? = 14
fit.km <- kmeans(wine, 3, nstart=25)  
fit.km$size
fit.km$centers
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)
# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
ct.km <- table(wine$Type, fit.km$cluster)
ct.km 
# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
cluspol