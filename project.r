#Full Model
library(readxl)
Graduate_Data <- read_excel("C:/Users/Aravind Reddy/Desktop/Regression Analysis/Graduate_Data.xlsx")
#X<-as.data.frame(Delivery_Data)
X<-as.matrix(Graduate_Data)

y<-X[,8]
x1<-X[,1]
x2<-X[,2]
x3<-X[,3]
x4<-X[,4]
x5<-X[,5]
x6<-X[,6]
x7<-X[,7]

colnames(X) <- c("x1","x2","x3","x4","x5","x6","x7","y")
mod1<-lm(y~x1+x2+x3+x4+x5+x6+x7)
summary(mod1)

qqnorm(y)




#Model Diagnostics and Data Cleaning
library(readxl)
Graduate_Data <- read_excel("C:/Users/Aravind Reddy/Desktop/Regression Analysis/Graduate_Data.xlsx")
#X<-as.data.frame(Delivery_Data)
X<-as.matrix(Graduate_Data)

y<-X[,8]
x1<-X[,1]
x2<-X[,2]
x3<-X[,3]
x4<-X[,4]
x5<-X[,5]
x6<-X[,6]
x7<-X[,7]

colnames(X) <- c("x1","x2","x3","x4","x5","x6","x7","y")
mod.lm<-lm(y~x1+x2+x3+x4+x5+x6+x7)
summary(mod.lm)

#Leverage
lev<-lm.influence(mod.lm)$hat

# COOK'S Distance
cd<-cooks.distance(mod.lm)


#DFBETA
dfb<-dfbeta(mod.lm)

#DFFITS
dfft<-dffits(mod.lm)

#COVRATIO
covr<-covratio(mod.lm)

All<-as.matrix(cbind(X, lev, cd, dfb, dfft, covr))

# Data cleaning 
Allm <- All[All[, 9]<0.08 & All[, 9]<1 & All[, 18]<0.4 & All[,19]<1.1206 & All[,19]>-0.8794, ]


#Box-cox transformation
library(MASS)
library(readxl)
Graduate_Data <- read_excel("C:/Users/Aravind Reddy/Desktop/Regression Analysis/Graduate_Data_clean.xlsx")
X<-as.matrix(Graduate_Data)
y<-X[,8]
x1<-X[,1]
x2<-X[,2]
x3<-X[,3]
x4<-X[,4]
x5<-X[,5]
x6<-X[,6]
x7<-X[,7]
mod1<-lm(y~x1+x2+x3+x4+x5+x6+x7)
summary(mod1)
#plot(mod1)
#required package car
# Box-Cox transformation
library(car)
lambda<-boxCox(y~x1+x2+x3+x4+x5+x6+x7, family="yjPower", plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
y.tr<-bcPower(y, lambda = lambda.max)
mod2<-lm(y.tr~x1+x2+x3+x4+x5+x6+x7)
summary(mod2)
#plot(mod2)
res2<-residuals(mod2)
py2<-predict(mod2)
plot(py2,res2)

#Multi-Collinearity
vif(mod2)

#Model Selection
library(readxl)
library(MASS)

Graduate_Data <- read_excel("C:/Users/Aravind Reddy/Desktop/Regression Analysis/Graduate_Data_Clean.xlsx")
Graduate_Data<-as.data.frame(Graduate_Data)
colnames(Graduate_Data) <- c("x1","x2","x3","x4","x5","x6","x7","y")
mod1<-lm(y~x1+x2+x3+x4+x5+x6+x7,data = Graduate_Data)

summary(mod1)


library(olsrr) 

ols_step_forward_p(mod1,prem = 0.05)

ols_step_backward_p(mod1,prem = 0.05)

ols_step_both_p(mod1, details = TRUE)

ols_step_backward_aic(mod1,details = TRUE)

ols_step_forward_aic(mod1,details = TRUE)

#Testing
library(readxl)
Graduate_Data <- read_excel("C:/Users/Aravind Reddy/Desktop/Regression Analysis/Testing.xlsx")
#X<-as.data.frame(Delivery_Data)
X<-as.matrix(Graduate_Data)

y<-X[,7]
x1<-X[,1]
x2<-X[,2]
x3<-X[,3]
x5<-X[,4]
x6<-X[,5]
x7<-X[,6]

colnames(X) <- c("x1","x2","x3","x5","x6","x7","y")
mod.lm<-lm(y~x1+x2+x3+x5+x6+x7)
summary(mod.lm)

#Validation
library(readxl)
Graduate_Data <- read_excel("C:/Users/Aravind Reddy/Desktop/Regression Analysis/Validation.xlsx")
#X<-as.data.frame(Delivery_Data)
X<-as.matrix(Graduate_Data)

y<-X[,7]
x1<-X[,1]
x2<-X[,2]
x3<-X[,3]
x5<-X[,4]
x6<-X[,5]
x7<-X[,6]

colnames(X) <- c("x1","x2","x3","x5","x6","x7","y")
mod.lm<-lm(y~x1+x2+x3+x5+x6+x7)
summary(mod.lm)

#Final Model
Graduate_Data <- read_excel("C:/Users/Aravind Reddy/Desktop/Regression Analysis/Graduate_Data_clean_Final.xlsx")
#X<-as.data.frame(Delivery_Data)
X<-as.matrix(Graduate_Data)

y<-X[,7]
x1<-X[,1]
x2<-X[,2]
x3<-X[,3]
x5<-X[,4]
x6<-X[,5]
x7<-X[,6]

colnames(X) <- c("x1","x2","x3","x5","x6","x7","y")
mod.lm<-lm(y~x1+x2+x3+x5+x6+x7)
summary(mod.lm)

