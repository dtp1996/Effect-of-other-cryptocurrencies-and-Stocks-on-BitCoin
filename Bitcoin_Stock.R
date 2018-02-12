
setwd("C:/Users/DTP/Desktop/MS Analytics/Quarter-2/Intermediate Analytics/Bc Prediction")

Bc<-read.csv("Bc.csv",header = T)
NYSE<-read.csv("NYSE.csv",header = T)
DAX<-read.csv("DAX.csv",header = T)
JSE<-read.csv("JSE.csv",header = T)
FTSE<-read.csv("FTSE.csv",header = T)
ASX<-read.csv("ASX.csv",header = T)
NIKKEI<-read.csv("Nikkei.csv",header = T)
HANGSENG<-read.csv("HangSeng.csv",header = T)
SENSEX<-read.csv("Sensex.csv",header = T)
NASDAQ<-read.csv("Nasdaq.csv",header = T)

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


library(dplyr)
BitJoins <- left_join(Bc,DAX, by="date")
BitJoins <- left_join(BitJoins,NYSE, by="date")
BitJoins <- left_join(BitJoins,ASX, by="date")
BitJoins <- left_join(BitJoins,NASDAQ, by="date")
BitJoins <- left_join(BitJoins,FTSE, by="date")
BitJoins <- left_join(BitJoins,HANGSENG, by="date")
BitJoins <- left_join(BitJoins,SENSEX, by="date")
BitJoins <- left_join(BitJoins,JSE, by="date")

BitJoins$Bc<-as.numeric(paste(BitJoins$Bc))
BitJoins$DAX<-as.numeric(paste(BitJoins$DAX))
BitJoins$NYSE<-as.numeric(paste(BitJoins$NYSE))
BitJoins$ASX<-as.numeric(paste(BitJoins$ASX))
BitJoins$NASDAQ<-as.numeric(paste(BitJoins$NASDAQ))
BitJoins$FTSE<-as.numeric(paste(BitJoins$FTSE))
BitJoins$HANGSENG<-as.numeric(paste(BitJoins$HANGSENG))
BitJoins$SENSEX<-as.numeric(paste(BitJoins$SENSEX))
BitJoins$JSE<-as.numeric(paste(BitJoins$JSE))

colnames(DAX)

colnames(BitJoins)<-c("Date","Bc","DAX","NYSE","ASX","NASDAQ","FTSE","HANGSENG","SENSEX","JSE")

attach(BitJoins)
BitJoins$Date<-as.Date(BitJoins$Date,format = "%B %d, %Y")
BitJoined<-BitJoins

plot(BitJoined$Date,BitJoined$Bc,type = "l",col="red",ylim = range(0,10000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$DAX,type = "l",col="blue",ylim = range(0,10000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$NYSE,type = "l",col="green",ylim = range(0,10000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$ASX,type = "l",col="yellow",ylim = range(0,10000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$NASDAQ,type = "l",col="orange",ylim = range(0,10000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$FTSE,type = "l",col="darkgreen",ylim = range(0,10000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$HANGSENG,type = "l",col="black",ylim = range(0,10000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$SENSEX,type = "l",col="pink",ylim = range(0,10000),xlab = "Years",ylab = "$");par(new=TRUE)
plot(BitJoined$Date,BitJoined$JSE,type = "l",col="grey",ylim = range(0,10000),xlab = "Years",ylab = "$")

pairs(BitJoins$Bc~DAX+NYSE+ASX+NASDAQ+FTSE+JSE+SENSEX+HANGSENG,data=BitJoins)

summary(lm(BitJoined$Bc~.,data=BitJoined))
class(BitJoins$Bc)

summary(BitJoins)
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

pairs(BitJoins$Bc~DAX+NYSE+ASX+NASDAQ+FTSE+JSE+SENSEX+HANGSENG,data=BitJoins)


#k<-c("Date","Bc","DAX","NYSE","ASX","NASDAQ","FTSE","JSE","SENSEX","HANGSENG")
# par(mfrow=c(2,5))
# for(i in 2:10){
#   plot(BitJoins$Date, BitJoins[i])
#   abline(lm(Bc~BitJoins[i],data=BitJoins))
# }

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
data.frame(lr_pr)

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
clasf = svm(formula = Bc ~ NYSE+ASX+NASDAQ+FTSE+SENSEX+HANGSENG,
                 data = training_set2,
                 kernel = 'linear')

# Predicting the Test set results
svm_pr = predict(clasf, newdata = test_set2)
#data.frame(svm_pr)

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
reg = rpart(formula = Bc ~ NYSE+ASX+NASDAQ+FTSE+SENSEX+HANGSENG,
                  data = training_set2,
                  control = rpart.control(minsplit = 4))

# Predicting a new result with Decision Tree Regression
dt_pr = predict(reg, test_set2)
#data.frame(dt_pr)
#install.packages("e1071")
library(e1071)
clasf2 = naiveBayes(Bc ~ NYSE+ASX+NASDAQ+FTSE+SENSEX+HANGSENG, data = training_set2,cross=3)

# Predicting the Test set results
nb_pr = predict(clasf2, test_set2)
data.frame(nb_pr)


# Fitting Kernel-SVM to the Training set
# install.packages('e1071')
library(e1071)
clasf3 = svm(formula = Bc ~ NYSE+ASX+NASDAQ+FTSE+SENSEX+HANGSENG,
                  data = training_set2,
                  kernel = 'radial')

# Predicting the Test set results
ksvm_pr = predict(clasf3, newdata = test_set2)
#data.frame(ksvm_pr)


# Encoding the target feature as factor
Bc_LR = factor(Bc, levels = c(0, 1))
clasf = glm(formula = Bc_LR ~ NYSE+NASDAQ+SENSEX+HANGSENG,
                 family = binomial,
                 data = training_set2)

# Predicting the Test set results
prob_pr = predict(clasf, type = 'response', newdata = test_set2)
y_pr = ifelse(prob_pr > 0.5, 1, 0)

Predicted<-data.frame(lr_pr,svm_pr,ksvm_pr,dt_pr)

Compare<-data.frame(test_set2$Bc)

Compare<-cbind(Compare,Predicted)



#install.packages('caret')
library(caret)
#install.packages('e1071')
library(e1071)
pca = preProcess(x = training_set2[3:10], method = 'pca', pcaComp = 2)
data_set = predict(pca, training_set2[3:10])
data_set = data.frame(data_set)


#plotting PC1 and PC2
#plot(data_set$PC1,data_set$PC2,type="p")
data_sample=data_set
data_sample["Bc"]=scale(training_set2[2])
#Plotting all scalled values of dependent variable(mpg) and PC1,PC2 in 3D plane and fixing the axis.
cols=c("red")
library(rgl)
plot3d(data_sample,col=cols)
ranges <- c(min(data_sample), max(data_sample))
plot3d(data_sample, type = "s", col = cols,xlim=ranges,ylim = ranges,zlim = ranges)
