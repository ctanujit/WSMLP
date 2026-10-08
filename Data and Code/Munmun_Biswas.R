#regression tree
library(rsample)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(ipred)
library(Metrics) #rmse function

body_dimension_data_interest <- read.csv("~/Desktop/Workshop_stat_ml/My_talk/body_dimension_data_interest.csv", row.names=NULL)
View(body_dimension_data_interest)

set.seed(123)
bodydim_split<-initial_split(body_dimension_data_interest, propo=.7)
bodydim_train<-training(bodydim_split)
bodydim_test<-testing(bodydim_split)

#implementation of CART
m1<-rpart(formula=weight~., data=bodydim_train, method="anova")
rpart.plot(m1) #to view the tree
plotcp(m1) #To check for the prunning

m2 <- rpart(formula=weight ~ .,data= bodydim_train, method= "anova",control= list(cp = 0, xval = 10))
rpart.plot(m2)
plotcp(m2)

m3 <- rpart(formula=weight ~ .,data= bodydim_train, method= "anova",control= list(minsplit = 10, maxdepth = 12, xval = 10))
rpart.plot(m3)

pred <- predict(m1, newdata = bodydim_test)
obs<-bodydim_test$weight
rmse(pred, obs)

#Use of rattle() function
library(rattle)
rattle()
library(RGtk2)
library(gWidgets)
library(gWidgetsRGtk2)
library(cairoDevice)

#implementation of MARS
library(plotrix) #loading required for earth
library(TeachingDemos) #loading required for earth
library(plotmo) #loading required for earth
library(earth)
library(lattice) #loading required for caret
library(caret)
library(vip)
library(pdp)

mars1 <- earth(weight ~ ., data = bodydim_train)
print(mars1) #for model summary
summary(mars1)
plot(mars1, which = 1)

mars2 <- earth(weight ~., data = bodydim_train, degree = 2)
summary(mars2)
plot(mars2,which=1,legend.pos=0)

# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)
# for reproducibiity
set.seed(123)

# cross validated model
tuned_mars<-train(x=subset(bodydim_train, select=-weight),y=bodydim_train$weight,
                    method="earth",metric="RMSE",trControl=trainControl(method="cv", number=10),
                    tuneGrid=hyper_grid)
tuned_mars$bestTune
ggplot(tuned_mars)

#performance of the model in test dataset
summary(mars1,newdata=bodydim_test)
yhat=predict(mars1,newdata=bodydim_test)
yobs=bodydim_test$weight
rmse(yhat, yobs)

