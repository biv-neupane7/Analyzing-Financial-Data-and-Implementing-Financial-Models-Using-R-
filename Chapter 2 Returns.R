
library(quantmod)
library("gridExtra")

## Writing a function imports the CSV file for prices of stocks and ETFs 
## we obtained from Yahoo Finance. In addition to that, it will also
## perform a preliminary check on the data like changing the date variable,
## making the data outlay similar to getsymbols(), changing into xts object.

load.data<- function(rawdata, ticker) {
  data.raw<- read.csv(rawdata, header=TRUE)
  Date<- as.Date(data.raw$Date, format="%Y-%m-%d")
  data.raw<- cbind(Date, data.raw[,-1])
  data.raw<- data.raw[order(data.raw$Date),]
  data.raw<- xts(data.raw[,2:7], order.by = data.raw[,1])
  A<- paste(ticker, ".Open",sep="")
  B<- paste(ticker, ".High",sep="")
  C<- paste(ticker, ".Low",sep="")
  D<- paste(ticker, ".Close",sep="")
  E<- paste(ticker, ".Adjusted",sep="")
  F<- paste(ticker, ".Volume",sep="")
  names(data.raw)<- paste(c(A,B,C,D,E,F))
  data.raw<- cbind(data.raw[,1:4], data.raw[,6],data.raw[,5])
  return(data.raw)
}


########################## Security's returns

## 2.1 Price Returns

options(scipen=999) # To generate output in a readable format

data_apple<- load.data("AAPL.csv", "AAPL")

rets=data_apple$AAPL.Close # Selecting the required column

names(rets)<- "Price" # Changing the column name

rets$lag_price<- lag(rets$Price, k=1) # creating a variable equal to the lag 1 of closing price

rets$price_return<- rets$lag_price/rets$Price -1 # Just the formula of calculating periodic returns

head(rets)

################# ALTERNATUVE WAY ###########################

# Use Delt() function from quantmod package

rets2<- Delt(rets$Price) 
head(rets2) # This is way convenient than the earlier one


rets<- rets[-1,3] # deleting the first row and selecting the price return column

head(rets)


## 2.2 TOTAL RETURNS

# It is actually very similar to the price returns. You only have to use
# adjusted closing price for the analysis

tot_rets= data_apple[-1, 6] # Selecting the required column

names(tot_rets)="Adjusted_Price" # Changing the column name

tot_rets$Lagd_AdjPrice=lag(tot_rets$Adjusted_Price, k=1) # Creating a
                  # lagged price column

tot_rets$Total_return=tot_rets$Lagd_AdjPrice/tot_rets$Adjusted_Price-1

tot_rets<- tot_rets[,3] # Only selecting the 3rd column

head(tot_rets)


## 2.3 Logarithmic Total Returns


log_rets=ROC(data_apple$AAPL.Adjusted)

log_rets=log_rets[-1,]

names(log_rets)="Log_Return"

head(log_rets)

################################################################


## 2.4 Winsorization and Truncation


# Winsorization replaces the values greater than the i-th percentile 
# and less than the (1 ??? i-th) percentile to the values at those 
# levels

# Step 1: Calculate Upper and Lower Cut-Offs

(upper<- as.numeric(quantile(tot_rets, 0.995, na.rm=T)))

(lower<- as.numeric(quantile(tot_rets, 0.005, na.rm=T)))

# Step 2: Winsorize the Data

  
winsorize<- ifelse(tot_rets<= lower, lower,
                   ifelse(tot_rets>= upper, upper, tot_rets))


summary(winsorize) # compare this to the summary down below

summary(tot_rets) # See the difference?


######################### TRUNCATION ###############################

# Truncation is a more systematic way of deleting observations rather
# than eyeballing the data and deleting observations that look like 
# Outliers.


# Using subset(), we can keep any value of tot_rets that is
# between the upper and lower cut-offs.


truncate<- subset(tot_rets, tot_rets<= upper &
                    tot_rets >= lower)

summary(truncate)




































































