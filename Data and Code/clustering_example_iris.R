############### k-means clustering example #########################

require("datasets")

# load Iris Dataset
data("iris") 

#view structure of dataset
str(iris) 

#view statistical summary of dataset
summary(iris)
print(summary(iris)) 


#view top  rows of dataset
head(iris)
print(head(iris))


## Preprocess the dataset ##

## Since clustering is a type of Unsupervised Learning, we would not 
## require Class Label(output) during execution of our algorithm. 
## We will, therefore, remove Class Attribute "Species" and store it in 
## another variable. We would then normalize the attributes 
## between 0 and 1 using our own function.

iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new)
print(head(iris.new))

## check class label of iris data points 

head(iris.class)
print(head(iris.class))


#### normalization function ####


normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)
print(head(iris.new))


##############   Apply k-means clustering algorithm   ####################

#aplly k-means algorithm with no. of centroids(k)=3
result<- kmeans(iris.new,3) 
## print overall results 
print(result)



# gives no. of records in each cluster
result$size 
print(result$size)

# gives value of cluster center datapoint value(3 centers for k=3)
result$centers 
print(result$centers)


#gives cluster vector showing the custer where each record falls
result$cluster 
print(result$cluster)


#########    Verify results of clustering          ####################


par(mfrow=c(2,2), mar=c(5,4,2,2))

# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=result$cluster)


# Plot to see how Sepal.Length and Sepal.Width data points have been distributed 
# originally as per "class" attribute in dataset
plot(iris.new[c(1,2)], col=iris.class)


# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=result$cluster)


# Plot to see how Petal.Length and Petal.Width data points have been distributed 
# originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=iris.class)


#############  calculate class accuracy ##########

table(result$cluster,iris.class)
print(table(result$cluster,iris.class))


##################################

# Total number of correctly classified instances are: 36 + 47 + 50= 133
# Total number of incorrectly classified instances are: 3 + 14= 17
# Accuracy = 133/(133+17) = 0.88 i.e our model has achieved 88% accuracy!
  


##################### plot elbow method  ###########################

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(iris.new, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

wss_values=c()

for (i in 1:max(k.values))
{
  wss_values[i]=wss(i)
}

print(wss_values)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
