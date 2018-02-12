setwd("C:/Users/DTP/Desktop/MS Analytics/Quarter-2/Intermediate Analytics/Bitcoin Prediction")

bitcoin<-read.csv("bitcoin_price.csv",header = T)
bitcoin_cash<-read.csv("bitcoin_cash_price.csv",header = T,na.strings = "n/a")
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

plot(share$Year,share$BitCoinPercentage,type = "l",col="red",ylim = range(0,100));par(new=T)
plot(share$Year,share$OtherCryptocurrencies,type = "l",col="blue",ylim = range(0,100))
