rm(list=ls())
getwd()
setwd('R')
###################################################################3
# part 1
# loading and inspecting data
data <- read.csv('Housing_data.csv',header=TRUE,sep = ',')
str(data)


# Checking if na's
any(is.na(data))
# Check for dublicates
any(duplicated(data))
# Check for NaN
any(is.nan('data'))
# check for null
any(is.null(data))
# 1. answer: no

# ggplot for nice visualization
library(ggplot2)
# Overview of the data
boxplot(data,col = 'red')
# checking for variable AVR and dis outliers
boxplot(data$AVR)
boxplot(data$DIS)


summary(data)
# analysis and Visualization for PCCR 

plot(data$PCCR)
summary(data$PCCR)
ggplot(data,aes(PCCR))+
  geom_histogram(bins = 15, col = 'blue')

# PRLZ

ggplot(data,aes(PRLZ))+
  geom_histogram(bins = 15,col='blue')
summary(data$PRLZ)

# Indus

ggplot(data,aes(INDUS))+
  geom_histogram(bins =15,col='blue')
summary(data$INDUS)

# NOX
ggplot(data,aes(NOX))+
  geom_histogram(bins = 15,col='blue')
summary(data$NOX)

# AVR

ggplot(data,aes(AVR))+
  geom_histogram(bins = 15,col='blue')
summary(data$AVR)

#AGE
ggplot(data,aes(AGE))+
  geom_histogram(bins = 15,col='blue')
summary(data$AGE)

# DIS
ggplot(data,aes(DIS))+
  geom_histogram(bins = 15,col='blue')
summary(data$DIS)

# RAD
ggplot(data,aes(RAD))+
  geom_histogram(bins = 15,col='blue')
summary(data$RAD)

# TAX
ggplot(data,aes(TAX))+
  geom_histogram(bins = 15,col='blue')
summary(data$TAX)

# MEDV
ggplot(data,aes(MEDV))+
  geom_histogram(bins = 15,col='blue')
summary(data$MEDV)


# Correlation analysis
library(dplyr)
library(corrplot)

# correlations 
cor(data)
# Plotting correlation matrix
corrplot(cor(data),method = 'number')

# Linear model
linmodel <- lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX, data=data)
summary(linmodel)
# Mean squared error
round(mean(linmodel$residuals^2),2)
# without intercept
linmodel <- lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX+0, data=data)
summary(linmodel)
# without INDUS
linmodel <- lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX, data=data)
summary(linmodel)
# without intercept and INDUS, final model
linmodel <- lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX+0, data=data)
summary(linmodel)
# new variables




linmodel
coef(linmodel)


     #       PCCR     PRLZ             NOX     AVR     AGE     DIS     RAD    TAX
newV1 <- c(0.03221,    4,            0.432,  6.121,  64.2,   4.1300,   3,    254)
newV2 <- c(0.06733,    5,            0.443,  6.783,  67.8,   4.9832,   2,    267)
newV3 <- c(0.04211,    0,            0.454,  6.345,  62.5,   5.0322,   2,    231)
newV4 <- c(0.05328,    8,            0.476,  6.754,  59.8,   6.0412,   1,    255)

# calculating MEDV with these new values

NewForecast1 <- round(sum(newV1*coef(linmodel)),2)
NewForecast2 <- round(sum(newV2*coef(linmodel)),2)
NewForecast3 <- round(sum(newV3*coef(linmodel)),2)
NewForecast4 <- round(sum(newV4*coef(linmodel)),2)



# original vs new model

original_linmodel <- lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX, data=data)
summary(original_linmodel)
optimal_linmodel <- lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX+0, data=data)
summary(optimal_linmodel)
plot(fitted.values(optimal_linmodel))

plot(fitted.values(optimal_linmodel))

observations <- length(fitted.values(optimal_linmodel))
ggplot(data,aes(1:observations))+
  geom_line(aes(1:observations, data$MEDV), colour='blue')+
  geom_line(aes(1:observations, fitted.values(optimal_linmodel)),colour = 'red')+
  xlab('Observations')+
  ylab('Median Value')+
  ggtitle('Original ("blue") vs Final ("red") Values')
  
  

# residuals and density
residual_linmodel<-residuals(optimal_linmodel)
plot(1:observations,residual_linmodel)
plot(density(residual_linmodel),xlab = 'Observations',ylab = 'Residual density',main='Residual Analysis')
# Residuals are quite normally distributed
