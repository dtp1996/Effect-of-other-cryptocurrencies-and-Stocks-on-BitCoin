setwd("C:/Users/DTP/Desktop/MS Analytics/Quarter-2/Intermediate Analytics/Bitcoin Prediction")

bitcoin<-read.csv("bitcoin_price.csv",header = T)
bitcoin_cash<-read.csv("bitcoin_cash_price.csv",header = T)
ripple<-read.csv("ripple_price.csv",header = T)
dash<-read.csv("dash_price.csv",header = T)
ethereum<-read.csv("ethereum_price.csv",header = T)
neo<-read.csv("neo_price.csv",header = T)
nem<-read.csv("nem_price.csv",header = T)
monero<-read.csv("monero_price.csv",header = T)
lite_coin<-read.csv("litecoin_price.csv",header = T)

bitcoinP<-data.frame(date=bitcoin$Date,bitcoin$Close)
bitcoin_cashP<-data.frame(date=bitcoin_cash$Date,bitcoin_cash$Close)
rippleP<-data.frame(date=ripple$Date,ripple$Close)
dashP<-data.frame(date=dash$Date,dash$Close)
ethereumP<-data.frame(date=ethereum$Date,ethereum$Close)
neoP<-data.frame(date=neo$Date,neo$Close)
nemP<-data.frame(date=nem$Date,nem$Close)
moneroP<-data.frame(date=monero$Date,monero$Close)
litecoinP<-data.frame(date=lite_coin$Date,lite_coin$Close)


library(dplyr)
joins <- left_join(bitcoinP,bitcoin_cashP, by="date")
joins <- left_join(joins,rippleP, by="date")
joins <- left_join(joins,dashP, by="date")
joins <- left_join(joins,ethereumP, by="date")
joins <- left_join(joins,neoP, by="date")
joins <- left_join(joins,moneroP, by="date")
joins <- left_join(joins,litecoinP, by="date")
joins <- left_join(joins,nemP, by="date")

colnames(joins)<-c("Date","Bitcoin","Bitcoin_cash","Ripple","Dash","Ethereum","NEO","Monero","Litecoin","NEM")
class(joins$Bitcoin)
attach(joins)
joins$Date<-as.Date(joins$Date,format = "%B %d, %Y")
joined<-joins

plot(joined$Date,joined$Bitcoin,type = "l",col="red",ylim = range(0,1500),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Bitcoin_cash,type = "l",col="blue",ylim = range(0,1500),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Ripple,type = "l",col="green",ylim = range(0,1500),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Dash,type = "l",col="yellow",ylim = range(0,1500),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Ethereum,type = "l",col="orange",ylim = range(0,1500),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$NEO,type = "l",col="darkgreen",ylim = range(0,1500),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$NEM,type = "l",col="black",ylim = range(0,1500),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Litecoin,type = "l",col="pink",ylim = range(0,1500),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Monero,type = "l",col="grey",ylim = range(0,1500),xlab = "Years",ylab = "$")

pairs(joins$Bitcoin~Bitcoin_cash+Ripple+Dash+Ethereum+NEO+Monero+Litecoin+NEM,data=joins)

summary(lm(Bitcoin~Bitcoin_cash+Ripple+Dash+Ethereum+NEO+Monero+Litecoin+NEM,data=joins))


joins$Bitcoin_cash[is.na(joins$Bitcoin_cash)] <- round(mean(joins$Bitcoin_cash, na.rm = TRUE),digits = 0)
joins$Ripple[is.na(joins$Ripple)] <- round(mean(joins$Ripple, na.rm = TRUE),digits = 0)
joins$Dash[is.na(joins$Dash)] <- round(mean(joins$Dash, na.rm = TRUE),digits = 0)
joins$Ethereum[is.na(joins$Ethereum)] <- round(mean(joins$Ethereum, na.rm = TRUE),digits = 0)
joins$NEO[is.na(joins$NEO)] <- round(mean(joins$NEO, na.rm = TRUE),digits = 0)
joins$Monero[is.na(joins$Monero)] <- round(mean(joins$Monero, na.rm = TRUE),digits = 0)
joins$Litecoin[is.na(joins$Litecoin)] <- round(mean(joins$Litecoin, na.rm = TRUE),digits = 0)
joins$NEM[is.na(joins$NEM)] <- round(mean(joins$NEM, na.rm = TRUE),digits = 0)


attach(joins)
joins$Date<-as.Date(joins$Date,format = "%B %d, %Y")

pairs(joins$Bitcoin~Bitcoin_cash+Ripple+Dash+Ethereum+NEO+Monero+Litecoin+NEM,data=joins)


#k<-c("Date","Bitcoin","Bitcoin_cash","Ripple","Dash","Ethereum","NEO","Monero","Litecoin","NEM")
# par(mfrow=c(2,5))
# for(i in 2:10){
#   plot(joins$Date, joins[i])
#   abline(lm(Bitcoin~joins[i],data=joins))
# }

#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(joins$Bitcoin, SplitRatio = 0.75)
training_set = subset(joins, split == TRUE)
test_set = subset(joins, split == FALSE)


# Feature Scaling
training_set[3:10] = scale(training_set[3:10])
test_set[3:10] = scale(test_set[3:10])
training_set$Bitcoin<-round(training_set$Bitcoin,digits = 0)
# test_set$Bitcoin<-round(test_set$Bitcoin,digits = 0)
# training_set[is.na(training_set)] <- 0
# test_set[is.na(test_set)] <- 0
class(Bitcoin)

# Fitting Simple Linear Regression to the Training set
regressor1 = lm(formula = Bitcoin ~ Ripple+Ethereum+Litecoin+NEM,
               data = training_set)

# Predicting the Test set results
lr_pred = predict(regressor1, newdata = test_set)

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Bitcoin ~ Ripple+Ethereum+Litecoin+NEM,
                 data = training_set,
                 kernel = 'linear')

# Predicting the Test set results
svm_pred = predict(classifier, newdata = test_set)
#data.frame(svm_pred)

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(formula = Bitcoin ~ Ripple+Ethereum+Litecoin+NEM,
                  data = training_set,
                  control = rpart.control(minsplit = 4))

# Predicting a new result with Decision Tree Regression
dt_pred = predict(regressor, test_set)
#data.frame(dt_pred)
#install.packages("e1071")
library(e1071)
classifier2 = naiveBayes(Bitcoin ~ ., data = training_set,cross=3)

# Predicting the Test set results
nb_pred = predict(classifier2, test_set)
data.frame(nb_pred)


# Fitting Kernel-SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier3 = svm(formula = Bitcoin ~ Ripple+Ethereum+Litecoin+NEM,
                 data = training_set,
                 kernel = 'radial')

# Predicting the Test set results
ksvm_pred = predict(classifier3, newdata = test_set)
#data.frame(ksvm_pred)


# Encoding the target feature as factor
Bitcoin_LR = factor(Bitcoin, levels = c(0, 1))
classifier = glm(formula = Bitcoin_LR ~ Ripple+Ethereum+Litecoin+NEM,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

Predictions<-data.frame(lr_pred,svm_pred,ksvm_pred,dt_pred)

Comparision<-data.frame(test_set$Bitcoin)

Comparision<-cbind(Comparision,Predictions)



#install.packages('caret')
library(caret)
#install.packages('e1071')
library(e1071)
pca = preProcess(x = training_set[3:10], method = 'pca', pcaComp = 2)
data_set = predict(pca, training_set[3:10])
data_set = data.frame(data_set)


#plotting PC1 and PC2
#plot(data_set$PC1,data_set$PC2,type="p")
data_sample=data_set
data_sample["Bitcoin"]=scale(training_set[2])
#Plotting all scalled values of dependent variable(mpg) and PC1,PC2 in 3D plane and fixing the axis.
cols=c("red")
library(rgl)
plot3d(data_sample,col=cols)
ranges <- c(min(data_sample), max(data_sample))
plot3d(data_sample, type = "s", col = cols,xlim=ranges,ylim = ranges,zlim = ranges)

