## Introduction to R


## Vector Input 
c(157, 162, 166, 175, 182, 177, 164, 159, 174, 179)
Heights=c(157, 162, 166, 175, 182, 177, 164, 159, 174, 179)

X1=seq(1,100,2) ## Inserting a finite sequence of numbers
X2=rep(1,50) ## Inserting a vector with all 1's
X1;X2

## Matrix Input
matrix(c(157, 162, 166, 175, 182, 177, 164, 159, 174, 179,62, 65, 73, 63, 82, 85, 72, 55, 64, 65, 1, 0,0,0,0,0 ,1,1,1, 0),nrow=10)

## Combining vectors to form matrix

Weights=c(62, 65, 73, 63, 82, 85, 72, 55, 64, 65)
Sex=c(1, 0,0,0,0,0 ,1,1,1, 0)
rbind(Heights,Weights,Sex)
cbind(Heights,Weights,Sex)

matrix1= rbind(Heights,Weights,Sex)
## Data Reading

## Text file
read.table("DATA_SIM.txt",header=FALSE)
data1=read.table("DATA_SIM.txt",header=FALSE)
data_useful=data1$V2

## CSV file
read.csv("cerealsugar.csv")
data2=read.csv("cerealsugar.csv")

## Matrix Operations

length(Heights)

dim(matrix1)

t(matrix1)

matrix1+ matrix(rep(1,30),nrow=3)

matrix2=matrix1 %*% t(matrix1);matrix2

det(matrix2)

solve(matrix2)

qr(matrix2)$rank

eigen(matrix2)$values

## Measures of Location

sum(Heights);mean(Heights)

gm=prod(Heights)^(1/length(Heights));gm

median(Heights)

## Measures of dispersion

var(Heights)

sd(Heights)

max(Heights)-min(Heights)

quantile(Heights,0.25)

quantile(Heights,0.75)

summary(Heights)

## Sampling and simulation

Space=c(1:50)
probv=rep(1/50,50)
prob1=c(1,rep(0,49))
sample1=sample(Space,10,prob=probv,replace=TRUE);sample1
sample2=sample(Space,10,prob=prob1,replace=TRUE);sample2

rbinom(10,10,.5)

rpois(10,5)

rnorm(10,2,4)

rexp(10,2)

dbinom(1,10,0.5);dpois(1,5);dnorm(0,2,4);dexp(2,2)
pbinom(1,10,0.5);ppois(1,5);pnorm(0,2,4);pexp(2,2)
qbinom(0.9,10,0.5,lower.tail=TRUE)
qbinom(0.9,10,0.5,lower.tail=FALSE)

qnorm(0.9,2,4,lower.tail=TRUE)
qnorm(0.9,2,4,lower.tail=FALSE)

## Plots


plot(Weights~Heights,main="Plotting Height against Weight",xlab="Heights",ylab="Weights",type="p",col=2,pch=7)

x=seq(-10,10,0.01)
y=exp(x)
plot(y~x,main="Exponential Function",xlab="x",ylab="e^x",col=3,type="l",lty=2,xlim=c(0,10),ylim=c(0,10000))

