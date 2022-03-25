
library(quantmod)
library("gridExtra")

getwd()
setwd("C:/Users/user/Documents/Coursera/Rbook/data")

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

rets$price_return<- rets$Price/rets$lag_price -1 # Just the formula of calculating periodic returns

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

head(data_apple)

tot_rets= data_apple[, 6] # Selecting the required column

names(tot_rets)="Adjusted_Price" # Changing the column name

tot_rets$Lagd_AdjPrice=lag(tot_rets$Adjusted_Price, k=1) # Creating a
                  # lagged price column

tot_rets$Total_return=tot_rets$Adjusted_Price/tot_rets$Lagd_AdjPrice-1

tot_rets<- tot_rets[-1,3] # Only selecting the 3rd column

head(tot_rets)


## 2.3 Logarithmic Total Returns


log_rets=ROC(data_apple$AAPL.Adjusted)

log_rets=log_rets[-1,]

names(log_rets)="Log_Return"

head(log_rets)

################################################################


## 2.4 Winsorization and Truncation


# Winsorization replaces the values greater than the i-th percentile 
# and less than the (1 − i-th) percentile to the values at those 
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
                    tot_rets >= lower) # What this code tells us is
                            # that we are keeping all the returns that
                # are less than upper limit and also all the returns
      # that are more than lower limit


summary(truncate)


####################################################################

# 2.5 Cumulating Multi-Day Returns

# Here we gonna implement the code for generating returns when you
# reinvest dividends everytime you get one


# Step 1: Calculate Daily Gross Returns

head(tot_rets)

gross_ret<- 1+tot_rets$Total_return

head(gross_ret)

# Step 2: Cumulate Gross Returns Daily

cum.arith<- cumprod(gross_ret)

head(cum.arith)

# Step 3: Extract Ending Value

# The last value of cum.arith is the gross cumulative
# arithmetic return for AAPL from 2015 to 2019

cum.arith[nrow(cum.arith)] # subsetting using the index generates
                  # the gross cumulative returns

# For net cumulative returns

as.numeric(cum.arith[nrow(cum.arith)])-1

# So, over the 5 year period, if all the dividends were reinvested
# the total returns would have been 187 %



##################################################################

# 2.5.2 Cumulating Logarithmic Returns

# An alternative way to calculate multi-period returns is to take 
# the sum of the daily logarithmic returns.

cum.log<- sum(log_rets)
head(cum.log)

# So, the cumulative log return is 105 %

# For comparison, lets convert this figure to arithmetic terms by 
# taking the exponential

exp(cum.log)-1

# It is 187% which is exactly what we got earlier when we calculated
# cumulative returns using arithmetic returns


# 2.5.3 Comparing Price Return and Total Return

# Step 1: Calculate Normalized Close Price

(first.close<- as.numeric(data_apple$AAPL.Close[1])) # Just taking the 
                              # 1st closing price

prc.ret<- data_apple$AAPL.Close/first.close
names(prc.ret)<- "Price.Ret"
head(prc.ret)

# Step 2: Calculate Normalized Adjusted Close Price

(first.tot<- as.numeric(data_apple$AAPL.Adjusted[1]))

tot.ret<- data_apple$AAPL.Adjusted/first.tot
names(tot.ret)<- "Price.Ret"
head(tot.ret)


# Step 3: Plot the Price and Total Return

# We use plot() to create a line chart
# comparing the total return and price return for AAPL.


dt<- index(prc.ret)
(y.range<- range(prc.ret, tot.ret))

plot(x=dt, 
     y=tot.ret, 
     xlab="Date",
     ylab="Normalized Price",
     type="l",
     col="blue",
     main="Comparing AAPL Price Return and Total Return")
lines(x=dt, y=prc.ret, col="darkgreen")
abline(h=1)

legend("topleft", 
       c("Total Return", "Price Return"),
       col=c("blue","darkgreen"),
       lwd=c(1,1))

#################################################################

# 2.6 Weekly Returns


# Step 1: Change Daily Prices to Weekly Prices

wk<- data_apple
wk<- to.weekly(wk)
head(wk)

# Step 2: Calculate Weekly Total Return

wk=wk[,6]
names(wk)="Price"

# Using Delt to calculate returns

rets.weekly<- Delt(wk)
names(rets.weekly)<- "AAPL" # Changing column name
rets.weekly<- rets.weekly[-1,] # removing the first row
head(rets.weekly)

# Note that The weekly returns are Friday-to-Friday returns.

#############################################################

# 2.7 Monthly Returns

# Step 1: Change Daily Prices to Monthly Prices

mo<- data_apple
mo<- to.monthly(mo)
head(mo)

# Step2: Returns monthly

mo<- mo[,6] # Subsetting the required column
names(mo)<- "AAPL"

rets.monthly<- Delt(mo)
names(rets.monthly)<- "AAPL"
rets.monthly<- rets.monthly[-1]
head(rets.monthly)

###################################################################

# 2.8 Comparing Performance of Multiple Securities


data.aapl<- data_apple
data.amzn<-load.data("AMZN.csv","AMZN")
data.spy<- load.data("SPY.csv","SPY")
data.goog<- load.data("GOOG.csv","GOOG")

# In the book, there are two ways: Normalizing returns and cumulative
# returns. I am only gonna do the cum returns as i have alredy done
# the normalizing part earlier

rets.amzn<- 1 + Delt(data.amzn$AMZN.Adjusted)
rets.aapl<- 1 + Delt(data.aapl$AAPL.Adjusted)
rets.goog<- 1 + Delt(data.goog$GOOG.Adjusted)
rets.spy<- 1 + Delt(data.spy$SPY.Adjusted)

# concatenating all the variables in a single xts series

rets<- cbind(rets.amzn, rets.aapl, rets.goog, rets.spy)
head(rets)

# Changing the column names

names(rets)<- c("AMZN", "AAPL", "GOOG","SPY")

###### Step 2: Replace December 31, 2014 Value with $1


rets[1,]<- c(1,1,1,1)

### Step 3: Cumulate the Returns of Each Security

cum.rets<- cumprod(rets)
head(cum.rets)

## Step 4: Plot the Cumulative Returns


y.range<- range(cum.rets)


plot(x=index(cum.rets$AAPL),
     y=cum.rets$AAPL,
     ylim=y.range,
     xlab="Date",
     ylab="Wealth",
     type="l",
     lwd=3,
     main="Value of $1 Investment")
lines(x=index(cum.rets),y=cum.rets$AMZN, col="red")
lines(x=index(cum.rets),y=cum.rets$GOOG, col="darkgreen")
lines(x=index(cum.rets),y=cum.rets$SPY, col="blue")
abline(h=1)
legend("topleft", 
       c("APPLE", "AMAZON","GOOGLE","S&P 500"),
       col=c("black", "red", "darkgreen","blue"),
       lwd=c(4,3,2,1))








































