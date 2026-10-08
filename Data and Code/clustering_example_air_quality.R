require("datasets")

# load airquality Dataset
data("airquality")


#view structure of dataset
str(airquality) 

#view statistical summary of dataset
summary(airquality)
print(summary(airquality)) 


#view top  rows of dataset
head(airquality)
print(head(airquality))


## Preprocess the dataset ##

## Let's begin by finding which attributes have missing values. 
## We then need to impute those missing values(NA), which we will 
## be doing simply by replacing NA with monthly average. 


# apply function anyNA() on all columns of airquality dataset

col1<- mapply(anyNA,airquality) 
print(col1)


## The output shows that only Ozone and Solar.R attributes have NA i.e. some missing value.


# Impute monthly mean in Ozone
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Ozone"],na.rm = TRUE)
  }
  
  # Impute monthly mean in Solar.R
  if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
  
}

#Normalize the dataset so that no particular attribute has more impact on clustering algorithm than others.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# replace contents of dataset with normalized values
airquality<- normalize(airquality) 


## We have removed missing values from our dataset. 
## We can now perform k-means clustering on our dataset



##############   Apply k-means clustering algorithm   ####################

# apply k-means algorithmusing first 4 attributes and with k=3(no. of required clusters)

result<- kmeans(airquality[c(1,2,3,4)],3) 

# gives no. of records in each cluster
result$size 
print(result$size)

# gives value of cluster center datapoint value(3 centers for k=3)
result$centers 
print(result$centers)


#gives cluster vector showing the custer where each record falls
result$cluster 
print(result$cluster)




#########     Visualize clustering results          ####################

par(mfrow=c(1,2), mar=c(5,4,2,2))

# Plot to see how Ozone and Solar.R data points have been distributed in clusters
plot(airquality[,1:2], col=result$cluster) 



## Graph shows that we have got 3 clearly distinguishable clusters for Ozone and Solar.R data points. 
## Let's see how clustering has performed on Wind and Temp attributes.


# Plot to see how Wind and Temp data points have been distributed in clusters

plot(airquality[,3:4], col=result$cluster) 


## This graph shows that Wind and Temp data points have not been clustered properly. 
## Let us find out which attributes have been taken into consideration more by k-means algorithm. 
## For this, we will plot all possible combinations of attributes!


# Plot to see all attribute combinations
plot(airquality[,], col=result$cluster) 


## From the above plot, it can be seen that k-means algorithm has successfully clustered Ozone:Solar.R, 
## Wind:Solar.R, Temp:Solar.R, Month:Solar.R, Day:Solar.R. By observing the pattern, it can be seen 
## that Solar.R is common amongst all the groups and therefore, it could be concluded that the clustering 
## has been most affected by values in Solar.R attribute.



############################
# 
# 
# ## Library clusters allow us to represent (with the aid of PCA) the cluster 
# ## solution into 2 dimensions:
# 
# library(cluster)
# clusplot(airquality, result$cluster, main='2D representation of the Cluster solution',
#          color=TRUE, shade=TRUE,
#          labels=2, lines=0)

