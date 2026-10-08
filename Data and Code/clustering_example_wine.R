
## The library rattle is loaded in order to use the data set wines.

# install.packages('rattle')
data(wine, package='rattle')
head(wine)

# To standarize the variables
wine.stand <- scale(wine[-1]) 


# K-Means
k.means.fit <- kmeans(wine.stand, 3) # k = 3


attributes(k.means.fit)


# Centroids:
k.means.fit$centers


# Clusters:
k.means.fit$cluster




# Cluster size:
k.means.fit$size



## Library clusters allow us to represent (with the aid of PCA) the cluster 
## solution into 2 dimensions:

library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)



## In order to evaluate the clustering performance we build a confusion matrix:

print(table(wine[,1],k.means.fit$cluster))









#############################  Hierarchical clustering:  #####################

## Hierarchical methods use a distance matrix as an input for the clustering algorithm. 
## The choice of an appropriate metric will influence the shape of the clusters, as some 
## elements may be close to one another according to one distance and farther away 
## according to another.

# Euclidean distance matrix.
d <- dist(wine.stand, method = "euclidean") 

## We use the Euclidean distance as an input for the clustering algorithm 
## (Ward's minimum variance criterion minimizes the total within-cluster variance):

H.fit <- hclust(d, method="ward")


## The clustering output can be displayed in a dendrogram

# display dendogram
plot(H.fit)

# cut tree into 5 clusters
groups <- cutree(H.fit, k=3) 

# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 


## evaluated with the aid of a confusion matrix as follows:


print(table(wine[,1],groups))




