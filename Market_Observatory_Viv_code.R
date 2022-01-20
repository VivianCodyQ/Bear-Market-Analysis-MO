
#packageurl<-'https://cran.r-project.org/src/contrib/bbdetection_1.0.tar.gz'
#install.packages(packageurl, repos=NULL, type="source")

#install.packages('rlist', repos=NULL, type="source")
library(rlist)

library(bbdetection)
library(quantmod)
library(tidyverse)
library(tidyquant)


```{r} #Get sp500 data from designated start date and end date
sp500data <- tq_get("^GSPC", from = '1950-01-01',
                    to = "2021-12-31", collapse ="monthly")
head(sp500data) #
```
```{r}
sp500data %>%
  ggplot(aes(x = date, y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "S&P500 Bar Chart", y = "Closing Price", x = "")
```
```{r}
###fix the dataset to make it latest

sp500_new <- sp500data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = to.monthly, indexAt = "firstof")

rowindex <-as.vector(sp500_new['date'])

rowindex <-t(t(rowindex))
sp500_new <- sp500_new['adjusted']
library(zoo)
library(xtable)
library(ggplot2)
sp500 <- sp500_new # choose the monthly data
#colnames(sp500) <- c("","SPX")
print(head(sp500))
sp500 <- zoo(sp500, rowindex)
print(class(sp500))

dates <- index(sp500) # retrieve dates
#dates <- as.yearmon(dates) # convert dates to "yearmon" format if monthly data
price <- as.vector(coredata(sp500)) # retrieve prices
#price <- list(price)
print(head(price))
setpar_dating_alg(4, 6, 4, 16, 20) # parameters for monthly data
bull <- run_dating_alg(price) # detect the states
df <- bb.dating.states(price, bull, dates)
df


########################################Priinting all bear periods############################################################
bearperiod <- df[,5:7]

bearperiod$Startdate <- substr(bearperiod$Dates,1,10)
bearperiod$Enddate <- substr(bearperiod$Dates,15,24)
bearperiod <- bearperiod[1:21,]
bearperiod$Startprice <- c(replicate(nrow(bearperiod), 0))
bearperiod$Endprice<- c(replicate(nrow(bearperiod), 0))
bearperiod$PercentageChange <- c(replicate(nrow(bearperiod), 0))
for(i in 1:nrow(bearperiod)){
  start <- bearperiod$Startdate[i]
  end <- bearperiod$Enddate[i]
  start <- as.Date(start)
  end <- as.Date(end)
  bearperiod[i,]$Duration <- end - start
  startprice<- sp500data[sp500data$date == start,]$close
  while (length(startprice) == 0){
    startprice<- sp500data[sp500data$date == (start+1),]$close
    start <- start +1
  }
  #print(class(startprice))
  bearperiod[i,]$Startprice <- startprice
  endprice<- sp500data[sp500data$date == end,]$close
  while (length(endprice) == 0){
    endprice<- sp500data[sp500data$date == (end-1),]$close
    end <- end -1 
  }
  
  bearperiod$Startdate[i] <- as.character.Date(start)
  bearperiod$Enddate[i] <- as.character.Date(end)
  
  #print(class(endprice))
  bearperiod[i,]$Endprice <- endprice
  percent <- round((endprice-startprice)/startprice * 100,2)
  bearperiod[i,]$PercentageChange <- percent
}

bearperiod
bearperiod[order(bearperiod$Amplitude),]

write.csv(bearperiod,'/Users/macbook/Desktop/Market\ Observatory/bearperiod_summary.csv')


############################## Function for getting start_to_trough and trough_to_rebound dataframes #######################

rebound_df <- function(start_date,end_date) {
  start_price<-bearperiod[bearperiod$Startdate==start_date,]$Startprice
  
  df_min<-subset(sp500data,date>end_date&close>=start_price)
  rebound_date<-min(df_min$date)
  
  df_rebound<- tq_get("^GSPC", from =start_date,to =rebound_date, collapse ="monthly")
  df_rebound$trading_days<-1:nrow(df_rebound)
  df_rebound<-df_rebound[,c('close','trading_days','date')]
  df_rebound <- df_rebound[order(df_rebound$date),]  #sort df based on date asc
  
  df_rebound$Perct_diff <- (df_rebound$close - df_rebound$close[1]) / df_rebound$close[1]
  trough_index<-which(df_rebound$close==min(df_rebound$close))
  
  start_to_trough<-df_rebound[1:trough_index,] #datframe1
  trough_to_rebound<-df_rebound[trough_index:nrow(df_rebound),] #dataframe2
  trough_to_rebound$trading_days<-1:nrow(trough_to_rebound)
  
  return(list(start_to_trough,trough_to_rebound))
}

df_se_date<-bearperiod[,c(4,5)]   #dataframe containing two columns:startdate and enddate

start_to_trough_df<-data.frame(matrix(ncol = 4, nrow = 0))
st_names<-c('close','trading_days','date','Perct_diff')
colnames(start_to_trough_df)<-st_names

trough_to_rebound_df<-data.frame(matrix(ncol = 4, nrow = 0))
tr_names<-c('close','trading_days','date','Perct_diff')
colnames(trough_to_rebound_df)<-tr_names

for (i in 1:nrow(df_se_date))
{
 start_date<-df_se_date[i,1]
 end_date<-df_se_date[i,2]
 df_combined<-rebound_df(start_date,end_date)
 
 start_to_trough_df_new<-df_combined[[1]]
 start_to_trough_df <- merge(start_to_trough_df, start_to_trough_df_new, by.x = "trading_days", by.y = "trading_days",all.x = TRUE, all.y = TRUE)
 
 trough_to_rebound_df_new<-df_combined[[2]]
 trough_to_rebound_df <- merge(trough_to_rebound_df,  trough_to_rebound_df_new, by.x = "trading_days",by.y = "trading_days",all.x = TRUE, all.y = TRUE)
}

#############

write.csv(start_to_trough_df,'/Users/macbook/Desktop/Market\ Observatory/start_to_trough_df.csv')
write.csv(trough_to_rebound_df,'/Users/macbook/Desktop/Market\ Observatory/trough_to_rebound_df.csv')


####################### Additional code: Union all bear periods data #################################################

############### Get full data of each bear market period
sp500data <- tq_get("^GSPC", from = '1950-01-01',
                    to = "2021-12-31", collapse ="monthly")

bear_fulldata <- function(start_date,end_date) {
  df<-sp500data[,c(2,6)]
  df$duration<-bearperiod[bearperiod$Startdate==start_date,]$Duration
  df$Trading_days<-nrow(df)
  return(df)
}


df_se_date<-bearperiod[,c(4,5)]   #dataframe containing two columns:startdate and enddate

bear_df<-data.frame(matrix(ncol = 4, nrow = 0))
bear_df_names<-c('date','close','duration','Trading_days')

colnames(bear_df)<-bear_df_names


bear_df <- data.frame(date=as.Date('1990-01-01'), 
                      close=0, 
                      duration=0,
                      Trading_days=0)
str(bear_df)
library(dplyr)

########## Union all the bear market dataframes
for (i in 1:nrow(df_se_date))
{
  start_date<-df_se_date[i,1]
  end_date<-df_se_date[i,2]
  bear_df_new<-bear_fulldata(start_date,end_date)
  class(bear_df_new$date)<-'Date'
  
  bear_df <- union_all(bear_df, bear_df_new)
}


sp500_dateclose<-sp500data[,c(2,6)]

merged_df<-merge(sp500_dateclose,bear_df,by.x='date',by.y='date',all.x = TRUE,all.y=TRUE)

write.csv(merged_df,'/Users/macbook/Desktop/Market\ Observatory/bear_df_union.csv')

###################################################################

rebound_df <- function(start_date,end_date) {
  start_price<-bearperiod[bearperiod$Startdate==start_date,]$Startprice
  
  df_min<-subset(sp500data,date>end_date&close>=start_price)
  rebound_date<-min(df_min$date)
  
  df_rebound<- tq_get("^GSPC", from =start_date,to =rebound_date, collapse ="monthly")
  df_rebound<-df_rebound[,c('close','date')]
  df_rebound <- df_rebound[order(df_rebound$date),]  #sort df based on date asc
  
  df_rebound$Perct_diff <- (df_rebound$close - df_rebound$close[1])/df_rebound$close[1]
  trough_index<-which(df_rebound$close==min(df_rebound$close))
  
  df_rebound[1:trough_index,'trading_days']<-trough_index:1
  rebound_days<-nrow(df_rebound)-trough_index+1
  df_rebound[trough_index:nrow(df_rebound),'trading_days']<-1:rebound_days
  return(df_rebound)
}

df_se_date<-bearperiod[,c(4,5)]   #dataframe containing two columns:startdate and enddate

All_df<-data.frame(matrix(ncol = 4, nrow = 0))
all_names<-c('close','trading_days','date','Perct_diff')
colnames(All_df)<-all_names

for (i in 1:nrow(df_se_date))
{
  start_date<-df_se_date[i,1]
  end_date<-df_se_date[i,2]
  df_new<-rebound_df(start_date,end_date)
  
  All_df <- merge(All_df, df_new, by.x = "trading_days", by.y = "trading_days",all.x = TRUE, all.y = TRUE)
}

