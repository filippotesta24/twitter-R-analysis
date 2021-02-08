#PREPARE THE ENVIRONMENT 
BBdate_A_U<-as.Date("YYYY-MM-DD") #The user sets the date in which start the bullish and
                                  #bearish and the over and under-tweeted analysis.
                                  #The Twitter API doesn't allow to access datas older 
                                  #then about a week.
BBdate_A_T<-BBdate_A_U+1          
number_days_return_analysis<- X   #Number of days in which the user wants to track the 
                                  #performances of the portfolios.
num_tweet_A<- XXX                 #Number of tweets to research. 


#all the packages required 
install.packages("twitteR")
install.packages("tidyquant")
install.packages("tidyverse")
install.packages("quantmod")
install.packages("plyr")
install.packages("dplyr")
install.packages("sentimentr")
install.packages("readxl")
install.packages("forecast")
install.packages("ggfortify")
install.packages("survMisc")
install.packages("labstatR")
install.packages("openxlsx")

#load library
library(twitteR) 
library(tidyquant)
library(tidyverse)
library(quantmod)
library(plyr)
library(dplyr)
library(sentimentr)
library(readxl)
library(labstatR)
library(survMisc)
library(ggfortify)
library(forecast)
library(openxlsx)

#load credentials
consumer_key<-"8xXzct0o0NbdUV0Rex4EVzYCJ"
consumer_secret<- "9PjaGd2BVmgaeSmrDrJnLOuY2iTuykY7w79IeKYYbxbDag3hpH"
access_token <- "1331292392977272837-ARmYkbXpNsYADfy1y0Wsi5vA0gZbqT"
access_secret <- "Qa6oQOlnOWboxiImZ4ECkSInfNDdzCRvmZBUHG9zDkjwQ"

#set up to authenticate
setup_twitter_oauth(consumer_key, consumer_secret,access_token,access_secret)

############################################
#Obtain the current weights of the S&P 500 index
#we limit the analysis to the largest companies that make up 80% of the S&P 500 index
all_stocks<-tq_index("SP500")
stocks<-data.frame()
z=0
i=1
while(z<0.8){
  z=z+all_stocks[i,5]
  stocks<-rbind(stocks,all_stocks[i,])
  i=i+1
}

#Create a vector with the ticker of the firms analyzed 
ticker_firms<-pull(stocks[1])
ticker_firms<-gsub("\\.", "-",ticker_firms) #we use this string to remove the "." from 
                                            #some stock name and replace with "-"
#CREATION OF THE RELATIVE WEIGHT 
sp500_W<-pull(stocks[5]) #the weight of the firms in S&P500 
sp500_W_R<-matrix(NA,ncol=1,nrow=length(sp500_W))
rownames(sp500_W_R)<-ticker_firms

for( i in 1:length(sp500_W)){
  sp500_W_R[i,]<-sp500_W[i]/sum(sp500_W)
}

#create the CASHTAG names (add the $ to the ticker)
cashtag<-matrix()
for( i in 1: nrow(stocks)) {
  cashtag[i]= paste("$",stocks[i,1],sep="")
}

#CREATION OF RETURN'S MATRIX
#create a matrix with the firm's name and the correlated return in the sample days 
#took in analysis 
since_data_A <-BBdate_A_T+1
until_data_A <-BBdate_A_T+5
A_Days<-as.character(seq(from=since_data_A, to=until_data_A,by=1))

AAreturn<-getSymbols(ticker_firms[1],from=since_data_A-1 ,to=until_data_A+1, auto.assign = FALSE)
returns_matrix<-matrix(ncol =length(ticker_firms),nrow = nrow(AAreturn)-1)
names.of.a <- as.character(index(AAreturn[-1]))
row.names(returns_matrix) <- names.of.a
colnames(returns_matrix) <- c(ticker_firms)
for (l in 1 : length(ticker_firms)){
  for(k in 1:(nrow(AAreturn)-1)) {
    return<-getSymbols.yahoo(ticker_firms[l],from=as.character(since_data_A-1),to=as.character(until_data_A+1),auto.assign = FALSE)  
    return<-as.vector(return[,6])
    returns_matrix[k,l]<-(return[(k+1)]/return[k])-1
  }
}

#CREATION OF THE MARKET PORTFOLIO 
#Return of the market portfolio 
weight_R1<-matrix(NA,ncol=length(ticker_firms),nrow=nrow(returns_matrix))
for (o in 1: length(names.of.a)){
  for(w in 1:length(ticker_firms)){
    weight_R1[o,w]<- returns_matrix[o,w]*sp500_W[w]
  }
}
market_portfolio_R<-matrix(rowSums(weight_R1),ncol=1,nrow =nrow(returns_matrix))
rownames(market_portfolio_R)<- c(names.of.a)
colnames(market_portfolio_R)<-"Market Returns"               
for(o in 1: length(names.of.a)){ 
  market_portfolio_R[o,]<-sum(weight_R1[o,],na.rm = T)
}

######################################
#DATA GENERATING PROCESS 
#Download the tweets about the companies in the index using their cashtag
#Produce a daily time series of the number of tweets about each company

#Set the dates to research the tweets and the number to research

#since_data <-as.Date("YYYY-MM-DD")
#until_data <-as.Date("YYYY-MM-DD")
#n_tweet_to_research<-XXX

#Create an empty matrix of days and companies that we fill with the daily number of tweets

#vector_Days<-as.character(seq(from=since_data, to=until_data,by=1))
#tweets_Matrix<-matrix(c(NA),ncol=length(vector_Days)-1,nrow=length(cashtag))
#rownames(tweets_Matrix)<-cashtag
#colnames(tweets_Matrix)<-as.character(vector_Days[-length(vector_Days)])

#for(z in 1:length(cashtag) ){
  #for(k in 1:(length(vector_Days)-1)){
   # rm(tweet)
    #tweet<-searchTwitter(cashtag[z], n=n_tweet_to_research,lang="en",since=paste0(vector_Days[k]),until=vector_Days[k+1])
    #if (length(tweet)!=0){
      #tweets_Matrix[z,k]<-length(strip_retweets(tweet))
    #}else{
      #tweets_Matrix[z,k]<-NA
    #}
    #Sys.sleep(22) #Suspend execution of R expressions for a specified time, to avoid problem with the Tweeter API
  #}
#}

#we save the data as csv

#write.csv(XXX,"XXX.csv")

##############################
#ANALYSIS OF THE CYCLICALITY
firms_in_analysis<- read_excel("Companies.xlsx")
Tweets_Matrix_cyclicality <- read_excel("Daily_time_series_of_all_tweets.xlsx")
colnames(Tweets_Matrix_cyclicality) <- convertToDate(colnames(Tweets_Matrix_cyclicality))
rownames(Tweets_Matrix_cyclicality) <- t(firms_in_analysis)
Tweets_Matrix_cyclicality <- as.matrix(Tweets_Matrix_cyclicality)
dates <- colnames(Tweets_Matrix_cyclicality)
Daily_number_of_tweets <- colSums(Tweets_Matrix_cyclicality)
tweets_database<-as.ts(Daily_number_of_tweets)

#ANALYSIS ON CYCLICALITY
#Compute statistical indicators
Indicators_vector<-c(mean(tweets_database), min(tweets_database), max(tweets_database),
                     sd(tweets_database), var(tweets_database), mad(tweets_database))

#First plot with trend
plot(as.Date(dates), as.vector(Daily_number_of_tweets), "l",xlab = "Days", ylab = "Number of tweets", main = "Time series of all tweets")
trend_days<-c(1:length(Daily_number_of_tweets))
reg <- lm(Daily_number_of_tweets~trend_days)
abline(reg,col="red")

#auto correlation test 
ggAcf(tweets_database)
Box.test(tweets_database, lag=1, type = "Ljung")

########################################################
#DATA GENERATING PROCESS
#Download the tweets about the companies in the index using their cashtag
tweet_text_firm<-matrix(NA,ncol=length(cashtag),nrow=num_tweet_A)
colnames(tweet_text_firm)<-c(cashtag)

for (z in 1:length(cashtag)){
  singlefirm_tweet<-searchTwitter(cashtag[z],n=num_tweet_A, lang="en", since=as.character(BBdate_A_U),until=as.character(BBdate_A_T))
  if (length(singlefirm_tweet)!=0 & length(singlefirm_tweet)!=1){
    strip_retweets(singlefirm_tweet)
    text_tweet<-sapply(singlefirm_tweet,function(j) j$getText())
    clear1<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",text_tweet)
    clear2<-gsub("http[^[:blank:]]+","",clear1)#remove the link from the tweets 
    clear3<-gsub("@\\w+","",clear2) 
    clear4<-gsub("[[:punct:]]"," ",clear3) #remove the punctuation character from the tweets 
    text<-gsub("[^[:alnum:]]"," ",clear4) #remove all the alphanumeric characters
    length(text)<-num_tweet_A
    tweet_text_firm[,z]<-text #set the length of the vector to avoid dimensional error
  }else if(length(singlefirm_tweet)==1) {
    text_tweet<-sapply(singlefirm_tweet,function(j) j$getText())
    clear1<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",text_tweet)
    clear2<-gsub("http[^[:blank:]]+","",clear1)#remove the link from the tweets 
    clear3<-gsub("@\\w+","",clear2) 
    clear4<-gsub("[[:punct:]]"," ",clear3) #remove the punctuation character from the tweets 
    text<-gsub("[^[:alnum:]]"," ",clear4) #remove all the alphanumeric characters
    length(text)<-num_tweet_A
    tweet_text_firm[,z]<-text
  } else { 
    tweet_text_firm[z,]<-NA
  }
  Sys.sleep(22)
}


###################################################################
#OVER AND UNDER-TWEETED ANLYSIS

#CREATION OF THE TWITTER INDEX
tweet_V<-matrix(ncol=length(BBdate_A_U),nrow=length(ticker_firms))
rownames(tweet_V)<-c(ticker_firms)
colnames(tweet_V)<-BBdate_A_U
for (i in 1:length(ticker_firms)){
  tweet_V[i,]<-length(na.omit(tweet_text_firm[,i]))
}

twitter_index<-matrix(NA,nrow=length(tweet_V),ncol=1)
rownames(twitter_index)<-c(ticker_firms)
colnames(twitter_index)<-A_Days[1]

for (i in 1: length(ticker_firms)){
  twitter_index[i,]<-tweet_V[i,]/sum(tweet_V[,1])
}

#We create two portfolio of UNDER-tweeted and OVER-tweeted firms with each return
undertweeted_firm<-c()
undertweeted_return<-matrix(NA,ncol=1,nrow=nrow(returns_matrix))
overtweeted_firm<-c()
overtweeted_return<-matrix(NA,ncol=1,nrow=nrow(returns_matrix))

for(q in 1:length(ticker_firms)){
  if (twitter_index[q,]>sp500_W_R[q]){
    overtweeted_firm<-cbind(overtweeted_firm,colnames(returns_matrix)[q])
    overtweeted_return<-cbind(overtweeted_return,returns_matrix[,q])}
  else{
    undertweeted_firm<-cbind(undertweeted_firm,colnames(returns_matrix)[q])
    undertweeted_return<-cbind(undertweeted_return,returns_matrix[,q])}
}

portfolio_return_UO<-matrix(NA,ncol=nrow(returns_matrix),nrow=2)
rownames(portfolio_return_UO)<-c("OVERtwitted portfolio returns","UNDERtwitted portfolio returns")
for (t in 1:nrow(returns_matrix)){
  portfolio_return_UO[1,t] <- sum(overtweeted_return[t,],na.rm = T)/length(overtweeted_firm)
  portfolio_return_UO[2,t] <- sum(undertweeted_return[t,],na.rm = T)/length(undertweeted_firm)
}

portfolio_return_UO<-rbind(portfolio_return_UO,t(market_portfolio_R))

#The user can plot the number of firms in each portfolio
print(c("The number of OVER tweeted firms is:",print(length(overtweeted_firm))))
print(c("The number of UNDER tweeted firms is:",print(length(undertweeted_firm))))

#plot the results
c<-portfolio_return_UO[1,]
v <- as.Date(colnames(portfolio_return_UO))
q<-portfolio_return_UO[3,]
f<-portfolio_return_UO[2,]
plot(v,c,type="p",xaxt='n',lwd=5,col="black",pch = 16,main="Return of the two portfolio UNDER and OVER tweeted",xlab="Dates",ylab="Return",ylim=c(min(portfolio_return_UO),max(portfolio_return_UO)))
lines(v,q,type="l",col="red",lwd=1)
axis(1, at=d, labels=d)
lines(v,f,type="p",col="blue",lwd=5,pch = 16)

#Assign the average return of the three portfolios 
average_overtweeted_R<-mean(portfolio_return_UO[1,])
average_undertweeted_R<-mean(portfolio_return_UO[2,])
average_market_R<-mean(portfolio_return_UO[3,])

###########################################################
#BULLISH AND BEARISH ANALYSIS

#we compute the sentimental analysis and we divide the tweets by polarity in the 
#bearish and bullish vector with the respective returns
sentiment_matrix<-matrix(ncol=length(cashtag),nrow=num_tweet_A)
Bullish_firms<-matrix(NA,ncol=length(cashtag),nrow=nrow(returns_matrix))
colnames(Bullish_firms)<-c(cashtag)
rownames(Bullish_firms)<-c(rownames(returns_matrix))
Bearish_firms<-matrix(NA,ncol=length(cashtag),nrow=nrow(returns_matrix))
rownames(Bearish_firms)<-c(rownames(returns_matrix))
colnames(Bearish_firms)<-c(cashtag)
neitherBB_firms<-matrix(NA,ncol=length(cashtag),nrow=nrow(returns_matrix))
rownames(neitherBB_firms)<-c(rownames(returns_matrix))
colnames(neitherBB_firms)<-c(cashtag)
for (f in 1:length(cashtag)){
  for (w in 1:num_tweet_A){
    sentiment<-sentiment_by(tweet_text_firm[w,f]) 
    sentiment_matrix[w,f]<-sentiment$ave_sentiment#the matrix with the average score of the sentimental analysis 
  }
  if ((sum(sentiment_matrix[,f]>0,na.rm = T))>(sum(sentiment_matrix[,f]<0,na.rm = T))) {
    Bullish_firms[,f]<-returns_matrix[,f]#we add the respective return for days in analysis 
  }else if ((sum(sentiment_matrix[,f]>0,na.rm = T))<(sum(sentiment_matrix[,f]<0,na.rm = T))) {
    Bearish_firms[,f]<-returns_matrix[,f]
  }else {
    neitherBB_firms[,f]<-returns_matrix[,f]
  }
}

Bullish_firms<-Bullish_firms[, colSums(is.na( Bullish_firms)) != nrow( Bullish_firms)]#we remove all the empty columns 
Bearish_firms<-Bearish_firms[, colSums(is.na( Bearish_firms)) != nrow( Bearish_firms)]
neitherBB_firms<-neitherBB_firms[, colSums(is.na( neitherBB_firms)) != nrow( neitherBB_firms)]

#We create a matrix with the total return of each firm in the days analyzed 
Port.RetBB<-matrix(NA,ncol=2,nrow=nrow(returns_matrix))
rownames(Port.RetBB)<-rownames(returns_matrix)
colnames(Port.RetBB)<-c("BULLish Portfolio","BEARish Portfolio")

for (l in 1: nrow(returns_matrix)){
  Port.RetBB[l,1]<-sum(Bullish_firms[l,],na.rm = T)/ncol(Bullish_firms)
  Port.RetBB[l,2]<-sum(Bearish_firms[l,],na.rm=T)/ncol(Bearish_firms)
}

Port.RetBB<-cbind(Port.RetBB,market_portfolio_R)

#The user can print the number of firms in the portfolios 
print(c("The number of bullish firm is:",print(ncol(Bullish_firms))))
print(c("The number of bullish firm is:",print(ncol(Bearish_firms))))
print(c("The number of firm neither bearish neither bullish is:",print(ncol(neitherBB_firms))))

#The user can plot in a graph the return of the portfolio for the subsequent days 
z<-Port.RetBB[,1]
d <- as.Date(row.names(Port.RetBB))
h<-Port.RetBB[,2]
l<-Port.RetBB[,3]
plot(d,z,type="p",lwd=5,xaxt='n',col="Black",main="Return of the two BEARISH and BULLISH portfolio",xlab="Dates",ylab="Return",ylim=c(min(Port.RetBB),max(Port.RetBB)))
lines(d,l,type="l",lwd=1,xaxt='n',col="red")
axis(1, at=d, labels=d)
lines(d, h,type="p",lwd=5,col="blue")

#if we want to compute a more detailed analysis on a firm 
#single_analysis<-sentiment_by(tweet_text_firm[,1])
#summary(single_analysis$ave_sentiment)
#qplot(single_analysis$ave_sentiment,geom="histogram",main="Distribution of the sentiment polarity for AAPL",xlab="Sentiment score",ylab="Number of tweets")

#Assign the average return of the three portfolio 
average_BULL_R<-mean(Port.RetBB[,1])
average_BEAR_R<-mean(Port.RetBB[,2])
average_marketBB_R<-mean(Port.RetBB[,3])


#####################################################
#We hereby certify that
#– We have written the program ourselves except for clearly marked pieces of code 
#– We have tested the program and it ran without crashing 
#Corti Matteo, Simone Giay, Filippo Testa, Andrea Giacobbo



