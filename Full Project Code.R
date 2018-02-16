#Bitcoin vs other Crypto currencies

setwd("C:/Users/DTP/Desktop/MS Analytics/Quarter-2/Intermediate Analytics/Bitcoin Prediction")
#reading data into R
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

#Joining the tabels
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

#plotting Time series graph
legnames<-c("Bitcoin","Bitcoin_cash","Ripple","Dash","Ethereum","NEO","Monero","Litecoin","NEM")
plot(joined$Date,joined$Bitcoin,type = "l",col="red",ylim = range(0,2000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Bitcoin_cash,type = "l",col="blue",ylim = range(0,2000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Ripple,type = "l",col="green",ylim = range(0,2000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Dash,type = "l",col="yellow",ylim = range(0,2000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Ethereum,type = "l",col="orange",ylim = range(0,2000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$NEO,type = "l",col="darkgreen",ylim = range(0,2000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$NEM,type = "l",col="black",ylim = range(0,2000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Litecoin,type = "l",col="pink",ylim = range(0,2000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(joined$Date,joined$Monero,type = "l",col="grey",ylim = range(0,2000),xlab = "Years",ylab = "$")
legend("topleft", legnames, col=c("red","blue","green","yellow","orange","darkgreen","black","pink","grey"), lty=1, cex=.45)

#correlation graph
pairs(joins$Bitcoin~Bitcoin_cash+Ripple+Dash+Ethereum+NEO+Monero+Litecoin+NEM,data=joins)

summary(lm(Bitcoin~Bitcoin_cash+Ripple+Dash+Ethereum+NEO+Monero+Litecoin+NEM,data=joins))

#Replacing null values with mean of the collumn
joins$Bitcoin_cash[is.na(joins$Bitcoin_cash)] <- round(mean(joins$Bitcoin_cash, na.rm = TRUE),digits = 0)
joins$Ripple[is.na(joins$Ripple)] <- round(mean(joins$Ripple, na.rm = TRUE),digits = 0)
joins$Dash[is.na(joins$Dash)] <- round(mean(joins$Dash, na.rm = TRUE),digits = 0)
joins$Ethereum[is.na(joins$Ethereum)] <- round(mean(joins$Ethereum, na.rm = TRUE),digits = 0)
joins$NEO[is.na(joins$NEO)] <- round(mean(joins$NEO, na.rm = TRUE),digits = 0)
joins$Monero[is.na(joins$Monero)] <- round(mean(joins$Monero, na.rm = TRUE),digits = 0)
joins$Litecoin[is.na(joins$Litecoin)] <- round(mean(joins$Litecoin, na.rm = TRUE),digits = 0)
joins$NEM[is.na(joins$NEM)] <- round(mean(joins$NEM, na.rm = TRUE),digits = 0)

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

mse_lr <-mean(regressor1$residuals^2)
mse_lr

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Bitcoin ~ Ripple+Ethereum+Litecoin+NEM,
                 data = training_set,
                 kernel = 'linear')

# Predicting the Test set results
svm_pred = predict(classifier, newdata = test_set)
mse_svm <-mean(classifier$residuals^2)
mse_svm
#data.frame(svm_pred)

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(formula = Bitcoin ~ Ripple+Ethereum+Litecoin+NEM,
                  data = training_set,
                  control = rpart.control(minsplit = 4))

# Predicting a new result with Decision Tree Regression
dt_pred = predict(regressor, test_set)
mse_dt <-112619.9
mse_dt

summary(regressor)


# Fitting Kernel-SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier3 = svm(formula = Bitcoin ~ Ripple+Ethereum+Litecoin+NEM,
                  data = training_set,
                  kernel = 'radial')

# Predicting the Test set results
ksvm_pred = predict(classifier3, newdata = test_set)
mse_ksvm <-mean(classifier3$residuals^2)
mse_ksvm

#data.frame(ksvm_pred)

#Mean square error table
mse<-c(mse_lr,mse_svm,mse_dt,mse_ksvm)
MSE<-data.frame(c("Linear Regression","SVM","KSVM","Decision Tree"),mse)
colnames(MSE)<-c("Model","MSE")


#Projected vs Actual values
Predictions<-data.frame(lr_pred,svm_pred,ksvm_pred,dt_pred)

Comparision<-data.frame(test_set$Bitcoin)

Comparision<-cbind(Comparision,Predictions)
attach(Comparision)

#scatter plot with trend line for predicted vs actual
plot(Comparision$test_set.Bitcoin,Comparision$lr_pred,type = "p",col="red",ylim = range(0,8000),xlab="Actual BitCoin Price",ylab = "Predicted Value");par(new=T)

plot(Comparision$test_set.Bitcoin,Comparision$svm_pred,type = "p",col="darkgreen",ylim = range(0,8000),ylab = "",xlab = "");par(new=T)
plot(Comparision$test_set.Bitcoin,Comparision$dt_pred,type = "p",col="green",ylim = range(0,8000),ylab = "",xlab="");par(new=T)
plot(Comparision$test_set.Bitcoin,Comparision$ksvm_pred,type = "p",col="blue",ylim = range(0,8000),ylab = " ",xlab = "")

abline(lm(Comparision$test_set.Bitcoin~lr_pred),col="red")
abline(lm(Comparision$test_set.Bitcoin~svm_pred),col="darkgreen")
abline(lm(Comparision$test_set.Bitcoin~ksvm_pred),col="green")
abline(lm(Comparision$test_set.Bitcoin~dt_pred),col="blue")

legend("topleft",c("Linear Regression","SVM","KSVM","Decision Tree"),col=c("red","darkgreen","green","blue"),lty=1, cex=.75)

#install.packages('caret')
library(caret)
#install.packages('e1071')
library(e1071)
pca = preProcess(x = training_set[3:10], method = 'pca', pcaComp = 2)
data_set = predict(pca, training_set[3:10])
data_set = data.frame(data_set)


data_sample=data_set
data_sample["Bitcoin"]=scale(training_set[2])
#Plotting all scalled values of dependent variable(mpg) and PC1,PC2 in 3D plane and fixing the axis.
cols=c("red")
library(rgl)
plot3d(data_sample,col=cols)
ranges <- c(min(data_sample), max(data_sample))
plot3d(data_sample, type = "s", col = cols,xlim=ranges,ylim = ranges,zlim = ranges)

#-----------------------------------------------------------------------------------------
#2.Bitcoin Vs Stock markets

setwd("C:/Users/DTP/Desktop/MS Analytics/Quarter-2/Intermediate Analytics/Bitcoin Prediction")

# Reading data into R
Bc<-read.csv("Bitcoin.csv",header = T)
NYSE<-read.csv("NYSE.csv",header = T)
DAX<-read.csv("DAX.csv",header = T)
JSE<-read.csv("JSE.csv",header = T)
FTSE<-read.csv("FTSE.csv",header = T)
ASX<-read.csv("ASX.csv",header = T)
NIKKEI<-read.csv("Nikkei.csv",header = T)
HANGSENG<-read.csv("HangSeng.csv",header = T)
SENSEX<-read.csv("Sensex.csv",header = T)
NASDAQ<-read.csv("Nasdaq.csv",header = T)

#converting all regional currencies into USD
DAX$Price_US<-DAX$Price*1.22
JSE$Price_US<-JSE$Price*0.008
FTSE$Price_US<-FTSE$Price*1.38
ASX$Price_US<-ASX$Price*0.78
NIKKEI$Price_US<-NIKKEI$Price*0.009
HANGSENG$Price_US<-HANGSENG$Price*0.13
SENSEX$Price_US<-SENSEX$Price*0.016

as.numeric(paste(Bc$Price))
Bc<-data.frame(date=Bc$Date,Price=Bc$Price)
DAX<-data.frame(date=DAX$Date,Price=DAX$Price_US)
NYSE<-data.frame(date=NYSE$Date,Price=NYSE$Price)
ASX<-data.frame(date=ASX$Date,Price=ASX$Price_US)
NASDAQ<-data.frame(date=NASDAQ$Date,Price=NASDAQ$Price)
FTSE<-data.frame(date=FTSE$Date,Price=FTSE$Price_US)
HANGSENG<-data.frame(date=HANGSENG$Date,Price=HANGSENG$Price_US)
SENSEX<-data.frame(date=SENSEX$Date,Price=SENSEX$Price_US)
JSE<-data.frame(date=JSE$Date,Price=JSE$Price_US)
NIKKEI<-data.frame(date=NIKKEI$Date,Price=NIKKEI$Price_US)

#joining tables by date
library(dplyr)
BitJoins <- left_join(Bc,DAX, by="date")
BitJoins <- left_join(BitJoins,NYSE, by="date")
BitJoins <- left_join(BitJoins,ASX, by="date")
BitJoins <- left_join(BitJoins,NASDAQ, by="date")
BitJoins <- left_join(BitJoins,FTSE, by="date")
BitJoins <- left_join(BitJoins,HANGSENG, by="date")
BitJoins <- left_join(BitJoins,SENSEX, by="date")
BitJoins <- left_join(BitJoins,JSE, by="date")
# BitJoins <- left_join(BitJoins,NIKKEI, by="date")

colnames(BitJoins)<-c("Date","Bc","DAX","NYSE","ASX","NASDAQ","FTSE","HANGSENG","SENSEX","JSE")

BitJoins$Bc<-as.numeric(paste(BitJoins$Bc))
BitJoins$DAX<-as.numeric(paste(BitJoins$DAX))
BitJoins$NYSE<-as.numeric(paste(BitJoins$NYSE))
BitJoins$ASX<-as.numeric(paste(BitJoins$ASX))
BitJoins$NASDAQ<-as.numeric(paste(BitJoins$NASDAQ))
BitJoins$FTSE<-as.numeric(paste(BitJoins$FTSE))
BitJoins$HANGSENG<-as.numeric(paste(BitJoins$HANGSENG))
BitJoins$SENSEX<-as.numeric(paste(BitJoins$SENSEX))
BitJoins$JSE<-as.numeric(paste(BitJoins$JSE))

attach(BitJoins)
BitJoins$Date<-as.Date(BitJoins$Date,format = "%B %d, %Y")
BitJoined<-BitJoins

#Time series plot
plot(BitJoined$Date,BitJoined$Bc,type = "l",col="red",ylim = range(0,18000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$DAX,type = "l",col="blue",ylim = range(0,18000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$NYSE,type = "l",col="green",ylim = range(0,18000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$ASX,type = "l",col="yellow",ylim = range(0,18000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$NASDAQ,type = "l",col="orange",ylim = range(0,18000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$FTSE,type = "l",col="darkgreen",ylim = range(0,18000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$HANGSENG,type = "l",col="black",ylim = range(0,18000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$SENSEX,type = "l",col="pink",ylim = range(0,18000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$JSE,type = "l",col="grey",ylim = range(0,18000),xlab = "Years",ylab = "$")

legend("topleft",c("Bc","DAX","NYSE","ASX","NASDAQ","FTSE","HANGSENG","SENSEX","JSE"),col=c("red","blue","green","yellow","orange","darkgreen","black","pink","grey"),lty=1, cex=.45)

#coorelation graph
pairs(BitJoins$Bc~DAX+NYSE+ASX+NASDAQ+FTSE+JSE+SENSEX+HANGSENG,data=BitJoins)

summary(lm(BitJoined$Bc~.,data=BitJoined))

#Replacing null values with mean
BitJoins$DAX[is.na(BitJoins$DAX)] <- round(mean(BitJoins$DAX, na.rm = TRUE),digits = 0)
BitJoins$NYSE[is.na(BitJoins$NYSE)] <- round(mean(BitJoins$NYSE, na.rm = TRUE),digits = 0)
BitJoins$ASX[is.na(BitJoins$ASX)] <- round(mean(BitJoins$ASX, na.rm = TRUE),digits = 0)
BitJoins$NASDAQ[is.na(BitJoins$NASDAQ)] <- round(mean(BitJoins$NASDAQ, na.rm = TRUE),digits = 0)
BitJoins$FTSE[is.na(BitJoins$FTSE)] <- round(mean(BitJoins$FTSE, na.rm = TRUE),digits = 0)
BitJoins$JSE[is.na(BitJoins$JSE)] <- round(mean(BitJoins$JSE, na.rm = TRUE),digits = 0)
BitJoins$SENSEX[is.na(BitJoins$SENSEX)] <- round(mean(BitJoins$SENSEX, na.rm = TRUE),digits = 0)
BitJoins$HANGSENG[is.na(BitJoins$HANGSENG)] <- round(mean(BitJoins$HANGSENG, na.rm = TRUE),digits = 0)

attach(BitJoins)
BitJoins$Date<-as.Date(BitJoins$Date,format = "%B %d, %Y")


#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(BitJoins$Bc, SplitRatio = 0.75)
training_set2 = subset(BitJoins, split == TRUE)
test_set2 = subset(BitJoins, split == FALSE)


# Feature Scaling
training_set2[3:10] = scale(training_set2[3:10])
test_set2[3:10] = scale(test_set2[3:10])
# training_set2$Bc<-round(training_set2$Bc,digits = 0)
# test_set2$Bc<-round(test_set2$Bc,digits = 0)
# training_set2[is.na(training_set2)] <- 0
# test_set2[is.na(test_set2)] <- 0
#class(Bc)

# Fitting Simple Linear Regression to the Training set
reg1 = lm(formula = Bc ~ NYSE+ASX+NASDAQ+FTSE+SENSEX+HANGSENG,
          data = training_set2)

# Predicting the Test set results
lr_pr = predict(reg1, newdata = test_set2)
mse_lr1 <-mean(reg1$residuals^2)
#data.frame(lr_pr)

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
clasf = svm(formula = Bc ~ NYSE+ASX+NASDAQ+FTSE+SENSEX+HANGSENG,
            data = training_set2,
            kernel = 'linear')

# Predicting the Test set results
svm_pr = predict(clasf, newdata = test_set2)
mse_svm1 <-mean(clasf$residuals^2)
#data.frame(svm_pr)

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
reg = rpart(formula = Bc ~ NYSE+ASX+NASDAQ+FTSE+SENSEX+HANGSENG,
            data = training_set2,
            control = rpart.control(minsplit = 4))

# Predicting a new result with Decision Tree Regression
dt_pr = predict(reg, test_set2)
summary(reg)
mse_dt1<-606702.5
mse_dt1

# Fitting Kernel-SVM to the Training set
# install.packages('e1071')
library(e1071)
clasf3 = svm(formula = Bc ~ NYSE+ASX+NASDAQ+FTSE+SENSEX+HANGSENG,
             data = training_set2,
             kernel = 'radial')

# Predicting the Test set results
ksvm_pr = predict(clasf3, newdata = test_set2)
mse_ksvm1 <-mean(clasf3$residuals^2)
#data.frame(ksvm_pr)

#predicted Vs Actual Table
Predicted<-data.frame(lr_pr,svm_pr,ksvm_pr,dt_pr)

Compare<-data.frame(test_set2$Bc)

Compare<-cbind(Compare,Predicted)

#Mean square error table
mse1<-c(mse_lr1,mse_svm1,mse_dt1,mse_ksvm1)
MSE<-data.frame(c("Linear Regression","SVM","Decision Tree","KSVM"),mse1)
colnames(MSE)<-c("Model","MSE")

#Scatter plot of predicted vs actual with trend line
plot(Compare$test_set2.Bc,Compare$lr_pr,type = "p",col="red",ylim = range(0,8000),xlim = range(0,8000),ylab = "Predicted Values",xlab = "Actual Values");par(new=T)

plot(Compare$test_set2.Bc,Compare$svm_pr,type = "p",col="darkgreen",ylim = range(0,8000),xlim = range(0,8000),ylab = " ",xlab = "");par(new=T)
plot(Compare$test_set2.Bc,Compare$ksvm_pr,type = "p",col="green",ylim = range(0,8000),xlim = range(0,8000),ylab = " ",xlab = "");par(new=T)
plot(Compare$test_set2.Bc,Compare$dt_pr,type = "p",col="blue",ylim = range(0,8000),xlim = range(0,8000),ylab = " ",xlab = "")

abline(lm(Comparision$test_set.Bitcoin~lr_pred),col="red")
abline(lm(Comparision$test_set.Bitcoin~svm_pred),col="darkgreen")
abline(lm(Comparision$test_set.Bitcoin~ksvm_pred),col="green")
abline(lm(Comparision$test_set.Bitcoin~dt_pred),col="blue")

legend("topleft",c("Linear Regression","SVM","KSVM","Decision Tree"),col=c("red","darkgreen","green","blue"),lty=1, cex=.75)

#install.packages('caret')
library(caret)
#install.packages('e1071')
library(e1071)
pca1 = preProcess(x = training_set2[3:10], method = 'pca', pcaComp = 2)
data_set1 = predict(pca1, training_set2[3:10])
data_set1 = data.frame(data_set1)
data_sample1=data_set1
data_sample1["Bc"]=scale(training_set2[2])
#Plotting all scalled values of dependent variable(mpg) and PC1,PC2 in 3D plane and fixing the axis.
cols=c("red")
library(rgl)
plot3d(data_sample1,col=cols)
ranges <- c(min(data_sample1), max(data_sample1))
plot3d(data_sample1, type = "s", col = cols,xlim=ranges,ylim = ranges,zlim = ranges)


pca2 = preProcess(x = training_set2[3:10], method = 'pca', pcaComp = 2)
data_set_3 = predict(pca2, training_set2[3:10])
data_set_3 = data.frame(training_set2$Bc,training_set2$Date,data_set_3)
colnames(data_set_3)<-c("BitCoin","Date","Year","PC1","PC2")
data_set_3$Date<-as.numeric(format(data_set_3$Date,'%Y'))
# install.packages("cowplot")  # a gganimate dependency
# devtools::install_github("dgrtwo/gganimate")
#------------------------------------------------------------------------------------------
#3. Volume of shares between Bitcoin and other Crypto currencies over years
setwd("C:/Users/DTP/Desktop/MS Analytics/Quarter-2/Intermediate Analytics/Bitcoin Prediction")

#Reading into R
bitcoin<-read.csv("bitcoin_price.csv",header = T)
bitcoin_cash<-read.csv("bitcoin_cash_price.csv",header = T)
ripple<-read.csv("ripple_price.csv",header = T)
dash<-read.csv("dash_price.csv",header = T)
ethereum<-read.csv("ethereum_price.csv",header = T)
neo<-read.csv("neo_price.csv",header = T)
nem<-read.csv("nem_price.csv",header = T)
monero<-read.csv("monero_price.csv",header = T)
lite_coin<-read.csv("litecoin_price.csv",header = T)

bitcoinV<-data.frame(date=bitcoin$Date,bitcoin$Volume)
bitcoin_cashV<-data.frame(date=bitcoin_cash$Date,bitcoin_cash$Volume,stringsAsFactors=FALSE)
rippleV<-data.frame(date=ripple$Date,ripple$Volume)
dashV<-data.frame(date=dash$Date,dash$Volume)
ethereumV<-data.frame(date=ethereum$Date,ethereum$Volume)
neoV<-data.frame(date=neo$Date,neo$Volume)
nemV<-data.frame(date=nem$Date,nem$Volume)
moneroV<-data.frame(date=monero$Date,monero$Volume)
litecoinV<-data.frame(date=lite_coin$Date,lite_coin$Volume)

#joining tables
library(dplyr)
joinsV <- left_join(bitcoinV,bitcoin_cashV, by="date")
joinsV <- left_join(joinsV,rippleV, by="date")
joinsV <- left_join(joinsV,dashV, by="date")
joinsV <- left_join(joinsV,ethereumV, by="date")
joinsV <- left_join(joinsV,neoV, by="date")
joinsV <- left_join(joinsV,moneroV, by="date")
joinsV <- left_join(joinsV,litecoinV, by="date")
joinsV <- left_join(joinsV,nemV, by="date")

colnames(joinsV)<-c("Date","Bitcoin","Bitcoin_cash","Ripple","Dash","Ethereum","NEO","Monero","Litecoin","NEM")
joinsV$Date<-as.Date(joinsV$Date,format = "%B %d, %Y")

#write.csv(joinsV, file = "Volume.csv",row.names=TRUE, na="0")

joinedV<-read.csv("Volume.csv")
class(joinedV$Ethereum)

joinedV$Bitcoin<-as.numeric(as.character(joinedV$Bitcoin))
joinedV$Bitcoin_cash<-as.numeric(as.character(joinedV$Bitcoin_cash))
joinedV$Ripple<-as.numeric(as.character(joinedV$Ripple))
joinedV$Dash<-as.numeric(as.character(joinedV$Dash))
joinedV$Ethereum<-as.numeric(as.character(joinedV$Ethereum))
joinedV$NEO<-as.numeric(as.character(joinedV$NEO))
joinedV$NEM<-as.numeric(as.character(joinedV$NEM))
joinedV$Litecoin<-as.numeric(as.character(joinedV$Litecoin))
joinedV$Monero<-as.numeric(as.character(joinedV$Monero))

attach(joinedV)
# joinedV["TotalVolume"]=Bitcoin+Bitcoin_cash+Ripple+Dash+Ethereum+NEO+Monero+Litecoin+NEM
# joinedV$TotalVolume<-as.numeric(paste((joinedV$TotalVolume)))
# joinedV["BitCoinPercentage"]=(Bitcoin*100)/TotalVolume
# joinedV$BitCoinPercentage<-as.numeric(paste((joinedV$BitCoinPercentage)))
# joinedV["OtherCryptocurrencies"]=(100-BitCoin.Percentage)
# joinedV$OtherCryptocurrencies<-as.numeric(paste((joinedV$OtherCryptocurrencies)))

colnames(joinedV)=c("Year","Date","Bitcoin","Bitcoin_cash","Ripple","Dash","Ethereum","NEO","Monero","Litecoin","NEM","TotalVolume","BitCoinPercentage","OtherCryptocurrencies")

share<-aggregate(joinedV[, 13:14], list(joinedV$Year), mean)
colnames(share)<-c("Year","BitCoinPercentage","OtherCryptocurrencies")

#plotting the change of volume of shares
plot(share$Year,share$BitCoinPercentage,type = "l",col="red",ylim = range(0,100));par(new=T)
plot(share$Year,share$OtherCryptocurrencies,type = "l",col="blue",ylim = range(0,100))

otc<-cbind(share$BitCoinPercentage,share$OtherCryptocurrencies)

#bar plot comparing volume of shares for bitcoin and other crypto currencies
barplot(t(otc), beside=T, ylab="Percentage", 
        cex.names=0.8, las=2, ylim=c(0,100), col=c("darkblue","red"))
axis(1, at = seq(3,13,by=2.5), labels = c("2013","2014","2015","2016","2017"))
legend("topright",c("BitCoin","Other Cryptocurrencies"),col=c("darkblue","red"), lty=1, cex=.65)


#Code for animation plot
# install.packages("cowplot")  # a gganimate dependency
# devtools::install_github("dgrtwo/gganimate")
library(ggplot2)
library(gganimate)

# install.packages("gapminder")
library(gapminder)
theme_set(theme_bw())  # pre-set the bw theme.

install.package("gminder")
library(gminder)
p2 <- ggplot(joinedV, aes(joinedV$Bitcoin,joinedV$BitCoinPercentage, frame = Year)) +
  geom_point()+scale_x_log10()

gganimate(p2)

p3<-ggplot(joinedV, aes(joinedV$Bitcoin,joinedV$OtherCryptocurrencies, frame = Year,col="red")) +
  geom_point()+scale_x_log10()
gganimate(p3)

Sys.setenv(PATH = paste("C:/Program Files/ImageMagick-7.0.7-Q16",Sys.getenv("PATH"), sep = ";"))
#install.packages("animation")
library(animation)
ani.options(convert = "C:/Program Files/ImageMagick-7.0.7-Q16/convert.exe")


library(ggplot2)
library(gganimate)

bp<-ggplot(share, aes(x=, y=cnt_thisyeartype, fill=current_type, frame = Year, cumulative = FALSE))+
  geom_bar(width = 1, stat = "identity")+coord_polar("y")+
  #geom_text(aes(label = cnt_thisyeartype), position = position_stack(vjust = 0.5)) +
  geom_col() +
  labs(y = "Quantity", title = "Total count of acc", subtitle = "Year by year stat", fill="Type of acc") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()

ani.options(interval=2)
gganimate(bp,"pie.gif")

#-----------------------------------------------------------------------------------------
#4. Time series analysis
getwd()
libraries <- c('forecast','astsa','tseries','ggplot2','xts','data.table','gridExtra','tidyquant','knitr','AnomalyDetection')
sapply(libraries, require, character = T)

data1<-read.csv("bitcoin_price.csv",header = T, sep=',')
head(data1)

data1$Open <- as.numeric(data1$Open)
data1$High <- as.numeric(data1$High)
data1$Low <- as.numeric(data1$Low)
data1$Close <- as.numeric(data1$Close)
data1$Volume <- as.numeric(data1$Volume)
data1$Date <- as.Date(data1$Date, "%d-%B-%y") 

# library(stringr)
# x = str_replace_all(data1$Date, fixed(","),"")
# x = str_replace_all(x, fixed(" "),"")
# data1$Date = as.Date(x,"%B%d%Y")

ggplot(data1, aes(x=Date, y=Close)) + geom_line(col="blue") + theme_tq()

price = ts(rev(data1$Close), frequency = 7)
log.price = log(price)
fit_1 = stl(log.price,"periodic", robust = TRUE)

log.price.seasonal = fit_1$time.series[,"seasonal"]
log.price.trend = fit_1$time.series[,"trend"]
log.price.residuals = fit_1$time.series[,"remainder"]
log.price.expected = log.price.seasonal + log.price.trend
data.decomposed = data.frame(x=1:1655, seasonality = log.price.seasonal, trend=log.price.trend, residuals=log.price.residuals, expected=log.price.expected, time=rev(data1$Date))


p1 <- ggplot(data = data.decomposed,aes(x=x,y=exp(seasonality))) + geom_line() + ylab("Seasonality") + coord_cartesian(xlim=c(1600,1650))
p2 <- ggplot(data=data.decomposed, aes(x=x, y=exp(trend))) + geom_line(col='darkblue')  + ylab("Trend")
#p3 <- ggplot(data=data.decomposed, aes(x=x, y = residuals)) + geom_line()
#p4 <- ggplot(data=data.decomposed, aes(x=x, y=expected)) + geom_line()
grid.arrange(p1,p2, ncol=1)

ggplot(data=data.decomposed,aes(x=x, y=residuals)) + geom_line(col="red")

ggplot(data=data.decomposed, aes(x=residuals)) + geom_histogram()

ggplot(data = data.decomposed, aes(sample=residuals)) + geom_qq()

diff.prices = diff(log.price)
diff.data = data.frame(x=1:length(diff.prices), prices=diff.prices)
ggplot(data=diff.data, aes(x=x, y=prices)) + geom_line(col="steelblue") + ylab("differenced series")

fit_2 = stl(diff.prices,"periodic", robust = TRUE)

diff.price.seasonal = fit_2$time.series[,"seasonal"]
diff.price.trend = fit_2$time.series[,"trend"]
diff.price.residuals = fit_2$time.series[,"remainder"]

diff.data.decomposed = data.frame(x=1:1654, seasonality = diff.price.seasonal, trend=diff.price.trend, residuals=diff.price.residuals)


p1 <- ggplot(data = diff.data.decomposed, aes(x=x,y=seasonality)) + geom_line() + ylab("Seasonality") + coord_cartesian(xlim=c(500,600))
p2 <- ggplot(data=data.decomposed, aes(x=x, y=trend)) + geom_line(col='darkblue')  + ylab("Trend")
#p3 <- ggplot(data=data.decomposed, aes(x=x, y = residuals)) + geom_line()
#p4 <- ggplot(data=data.decomposed, aes(x=x, y=expected)) + geom_line()
grid.arrange(p1,p2, ncol=1)

dif.dif.prices = diff(diff.prices)
diff.diff.data = data.frame(x=1:1653, double_differenced_prices = dif.dif.prices)
ggplot(data = diff.diff.data, aes(x=x, y = double_differenced_prices)) + geom_line(col="steelblue")

fit_3 = stl(dif.dif.prices,"periodic", robust = TRUE)

diff.diff.price.seasonal = fit_3$time.series[,"seasonal"]
diff.diff.price.trend = fit_3$time.series[,"trend"]
diff.diff.price.residuals = fit_3$time.series[,"remainder"]

diff.diff.data.decomposed = data.frame(x=1:1653, seasonality = diff.diff.price.seasonal, trend=diff.diff.price.trend, residuals=diff.diff.price.residuals)


p1 <- ggplot(data = diff.diff.data.decomposed, aes(x=x,y=seasonality)) + geom_line() + ylab("Seasonality") + coord_cartesian(xlim=c(500,600))
p2 <- ggplot(data=diff.data.decomposed, aes(x=x, y=trend)) + geom_line(col='darkblue')  + ylab("Trend")
#p3 <- ggplot(data=data.decomposed, aes(x=x, y = residuals)) + geom_line()
#p4 <- ggplot(data=data.decomposed, aes(x=x, y=expected)) + geom_line()
grid.arrange(p1,p2, ncol=1)

ggplot(data=diff.diff.data.decomposed, aes(x=residuals, y=..density..)) + geom_histogram(col="blue",fill="steelblue",alpha=0.5) + geom_density(col="firebrick",lwd=0.9)

p1<-autoplot(acf(dif.dif.prices,lag.max=30,plot=FALSE)) + ggtitle("Autocorrelation plot, d=2")

p2<-autoplot(pacf(dif.dif.prices, lag.max = 30,plot = FALSE))  + ggtitle("Partial autocorrelation plot, d=2")

grid.arrange(p1,p2,ncol=2)

d=2
for(q in 0:2){
  for(p in 0:7){
    
    if(p+d+q<=7){
      model<-arima(x=data1$Close, order = c((p),d,(q)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p,d,q,'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}
