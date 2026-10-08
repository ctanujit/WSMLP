

# simulation of a dataset with no dependence---------------------------------------------
y<-rnorm(500, mean = 0, sd = 1)
x<-rnorm(500, mean = 0, sd = 1)
cor(x,y)
plot(x,y)
abline(lm(y ~ x),col ="red")



# demonstration of simple linear regression (descriptve) using cars data-----------------
library("datasets", lib.loc="/usr/lib/R/library")
View(cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",las = 1)
cor(cars$speed, cars$dist)
title(main = "cars data")
#lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")
abline(lm(cars$dist ~ cars$speed), col = "red")
fm1<-lm(cars$dist ~ cars$speed)
fm1
fm1$fitted.values
fm1$residuals
#fitting a line without intercept
#abline(lm(cars$dist ~ -1 +cars$speed), col = "red")



#demonstration of how to deal with missing data in regression-----------------------------
abline(lm(cars$dist ~ cars$speed,na.action = na.exclude), col = "red")



#outliers and problem with least squares line---------------------------------------------
y<-cars$dist
x<-cars$speed
x[51]=48
y[51]=2
plot(cars$speed,cars$dist,xlim=c(0,50))
abline(lm(cars$dist ~ cars$speed), col = "red")
points(x[51],y[51],col="blue")
abline(lm(y ~ x),col ="red")
title(main = "cars data with artificial outlier")
library("MASS", lib.loc="/usr/lib/R/library")

#lms regression
plot(cars$speed,cars$dist,xlim=c(0,50))
title(main = "LMS regression with artificial outlier")
fmlms<-lqs(cars$dist ~ cars$speed,method = "lms",nsamp = "exact")
abline(fmlms,col ="blue")
points(x[51],y[51],col="blue")
fmlms<-lqs(y ~ x,method = "lms",nsamp = "exact")
abline(fmlms,col ="blue")
fmlms
#summary(fmlms)
fmlms$fitted.values
fmlms$residuals

#lts regression
# plot(cars$speed,cars$dist,xlim=c(0,50))
# fmlts<-lqs(cars$dist ~ cars$speed,method = "lts",quantile = 25,nsamp = "exact")
# abline(fmlts,col ="blue")
# points(x[51],y[51],col="blue")
# fmlts<-lqs(y ~ x,method = "lts",quantile = 25,nsamp = "exact")
# abline(fmlts,col ="blue")


# use of nontransformed positive data and modeling-------------------------------------- 
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",las = 1)
cor(cars$speed, cars$dist)
title(main = "cars data")
#lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")
abline(lm(cars$dist ~ cars$speed), col = "red")
summary(fm1 <- lm(dist ~ speed, data = cars))
#mle of sigma^2_{\epsilon}
n=50
(t(fm1$residuals) %*% fm1$residuals)/(n)
#variance of residuals as an estimator
(t(fm1$residuals) %*% fm1$residuals)/(n - 1)
#unbiased estimator of sigma^2_{\epsilon}
(t(fm1$residuals) %*% fm1$residuals)/(n - 2)
fitted1<-fm1$coefficients[1] + (fm1$coefficients[2])*(cars$speed)
points(cars$speed,fitted1, col = "blue")
confint(fm1)

#model diagnostic checking
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
plot(fm1,4)
plot(fm1,6)
#test for homoscedasticity
#ncvTest(fm1)
#ols_test_breusch_pagan(fm1)
# test for uncorrelatedness
acf(fm1$residuals, type = "correlation")
#dwtest(fm1$residuals)
Box.test(fm1$residuals)
#test of normality
shapiro.test(fm1$residuals)
#ks.test(fm1$residuals,"pnorm",mean = mean(fm1$residuals),sd=sd(fm1$residuals))

#further diagnostic measures
influence.measures(fm1)

# different goodness of fit measures
summary(fm1)
AIC(fm1)
BIC(fm1)
#--------------------------------------------------------------------------------
#transform the data to log(y),log(x) and then fitting simple linear regression to that.
# variance stabilization and also positive data
#plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",las = 1, log = "xy")
#title(main = "cars data (logarithmic axis)")
#lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")
plot(log(cars$speed),log(cars$dist), xlab = "Speed (mph)", ylab = "Stopping distance (ft)",las = 1)
cor(log(cars$speed), log(cars$dist))
abline(lm(log(cars$dist) ~ log(cars$speed)))
title(main = "log transformed cars data")
#lines(lowess(log(cars$speed), log(cars$dist), f = 2/3, iter = 3), col = "red")
summary(fm1 <- lm(log(dist) ~ log(speed), data = cars))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
# speedmod <- cars$speed
# speedmod<- speedmod[-(23)]
# speedmod<- speedmod[-(1:3)]
# distmod <- cars$dist
# distmod<- distmod[-(23)]
# distmod<- distmod[-(1:3)]
# plot(log(speedmod),log(distmod), xlab = "Speed (mph)", ylab = "Stopping distance (ft)",las = 1)
# cor(log(speedmod), log(distmod))
# abline(lm(log(distmod) ~ log(speedmod)))
# title(main = "outlier deleted log transformed cars data")
# summary(fm1 <- lm(log(distmod) ~ log(speedmod)))
# opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),mar = c(4.1, 4.1, 2.1, 1.1))
# plot(fm1)
# par(opar)
#------------------------------------------------------------------------------------
#polynomial regression
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)", las = 1, xlim = c(0, 30))
d <- seq(0, 30, length.out = 200)
for(degree in 1:4) 
{
  fm <- lm(dist ~ poly(speed, degree), data = cars)
  assign(paste("cars", degree, sep="."), fm)
  lines(d, predict(fm, data.frame(speed=d)), col = degree)
}
fm2 <- lm(dist ~ poly(speed, 2), data = cars)
summary(fm2)
fm3 <- lm(dist ~ poly(speed, 3), data = cars)
summary(fm3)


#logistic regression------------------------------------------------------------------------


Classification1training <- read.delim("~/Desktop/Current Laptop All Files/Academic_Files/Talks_Given_Conference_Workshops_Seminars/Regression_BKC_Workshop_2020/R_Files_Slides/Classification1training.csv")
View(Classification1training)
Classification1test <- read.delim("~/Desktop/Current Laptop All Files/Academic_Files/Talks_Given_Conference_Workshops_Seminars/Regression_BKC_Workshop_2020/R_Files_Slides/Classification1test.csv")
View(Classification1test)
library("MASS", lib.loc="/usr/lib/R/library")


#logistic regression with single predictor------------------------------------------------------------
plot(Classification1training$Percent_Word_Money_Freq,Classification1training$Spam)
logitfit1<-glm(factor(Spam) ~ Percent_Word_Money_Freq,data=Classification1training,family=binomial(link="logit"))  
summary(logitfit1)
confint(logitfit1)


influence.measures(logitfit1)
plot(logitfit1)
plot(logitfit1,4)
plot(logitfit1,6)

#predictive performance of the logit model
logittest1<-predict(logitfit1,Classification1test)
confusionmat_logit<-table(logittest1>0.5,Classification1test$Spam,dnn=c('Predicted Group','Actual Group'))
confusionmat_logit

#exam dataset----------------------------------------------------------------------------------
examtraining <- read.csv("~/Desktop/Current Laptop All Files/Academic_Files/Talks_Given_Conference_Workshops_Seminars/Regression_BKC_Workshop_2020/R_Files_Slides/examtraining.csv")
View(examtraining)
examtest <- read.csv("~/Desktop/Current Laptop All Files/Academic_Files/Talks_Given_Conference_Workshops_Seminars/Regression_BKC_Workshop_2020/R_Files_Slides/examtest.csv")
View(examtest)
logitfit1<-glm(factor(Pass) ~ Hour,data=examtraining,family=binomial(link="logit"))  
summary(logitfit1)
confint(logitfit1)


influence.measures(logitfit1)
plot(logitfit1)
plot(logitfit1,4)
plot(logitfit1,6)

#predictive performance of the logit model
logittest1<-predict(logitfit1,examtest)
logittest1
confusionmat_logit<-table(logittest1>0.5,examtest$Pass,dnn=c('Predicted Group','Actual Group'))
confusionmat_logit


#multiple logistic regression-----------------------------------------------------------------
logitfit1<-glm(factor(Spam) ~ Total_Capital_Letters+Percent_Word_Money_Freq+Percent_Character_._Freq,data=Classification1training,family=binomial(link="logit"))  
summary(logitfit1)
confint(logitfit1)


influence.measures(logitfit1)
plot(logitfit1)
plot(logitfit1,4)
plot(logitfit1,6)
#checking multicollinearity
library("fmsb", lib.loc="~/R/x86_64-pc-linux-gnu-library/2.14")
VIFT_Cap_Let<-VIF(lm(formula = Classification1training$Total_Capital_Letters ~ Classification1training$Percent_Word_Money_Freq+Classification1training$Percent_Character_._Freq))
VIFT_Cap_Let
VIFT_Word_Money<-VIF(lm(formula = Classification1training$Percent_Word_Money_Freq ~ Classification1training$Total_Capital_Letters + Classification1training$Percent_Character_._Freq))
VIFT_Word_Money
VIFT_Char_Freq<-VIF(lm(formula = Classification1training$Percent_Character_._Freq ~ Classification1training$Percent_Word_Money_Freq + Classification1training$Total_Capital_Letters))
VIFT_Char_Freq

#predictive performance of the logit model
logittest1<-predict(logitfit1,Classification1test)
confusionmat_logit<-table(logittest1>0.5,Classification1test$Spam,dnn=c('Predicted Group','Actual Group'))
confusionmat_logit


#Demonstration of how to include interaction terms
logitfit2<-glm(factor(Spam) ~ Total_Capital_Letters+Percent_Word_Money_Freq+Percent_Character_._Freq+Percent_Word_Money_Freq:Percent_Character_._Freq,data=Classification1training,family=binomial(link="logit"))
summary(logitfit2)
confint(logitfit2)