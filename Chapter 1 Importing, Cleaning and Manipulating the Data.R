library(readr)
getwd()

setwd("C:/Users/user/Documents/Coursera/Rbook/data")
file.exists("C:/Users/user/Documents/Coursera/Rbook/data/AMZN.csv")

library(quantmod)

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



## Another function that we shall write is for the code that will show 
## top 3 lines of the df and bottom 3 lines of df


head.tail<- function(dataset){
  print(head(dataset,3))
  print(tail(dataset,3))
}

######################################################################

data_amzn<- read.csv("AMZN.csv", header=TRUE)

head(data_amzn, 5)
tail(data_amzn, 5)

str(data_amzn)

## As you can see the Date column is considered a "Factor" and not a Date 
## column. We need to change this to a Date Variable

Date<- as.Date(data_amzn$Date, format="%Y-%m-%d")
str(Date)

# Now lets combine this Date column and our original "data_amzn" file /df

data_amzn<- cbind(Date, data_amzn[,-1]) # [,-1] means all the rows and the
                                        # last 6 columns

str(data_amzn)
head(data_amzn,3)


## Convert this data into xts() object #################################

### We use the xts package to convert the data
### into an extensible time series, which is a common class of objects in R 
### that we use when dealing with financial data. We convert the object to xts
### in order to make our data identical to the output of using getSymbols() 
### to obtain stock prices

library(xts)

data_amzn<- xts(data_amzn[,2:7], order.by=data_amzn[,1])
# This selects columns 2 to 7 in the data_amzn and converts them into xts 
# object and finally orders them by the 1st column i.e. the Date column

class(data_amzn)

## Rename the Variables To be compatible with getSymbols()

names(data_amzn)<- c("AMZN.Open", "AMZN.High", "AMZN.Low",
                     "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume")


# Exchange the Fifth and Sixth Columns getSymbols() has the
# adjusted close price in Column 6 and the volume data in Column 5.

data_amzn<- cbind(data_amzn[,1:4], data_amzn[,6], data_amzn[,5])

head.tail(data_amzn)





## LOading the same file as above but using our own defined function


amzn_load_data<- load.data("AMZN.csv","AMZN")
head.tail(amzn_load_data)


##################  LOADING FROM YAHOO DIRECTLY

amzn_directly<- getSymbols("AMZN",from ="2014-12-31", to="2020-01-01",
                           auto.assign = FALSE)
tail(amzn_directly,3)

# The reason for calling the data until "2020-01-01" is that the 
# getSymbols () has a weird quirk of not returning the last day of whatever
# is being called.



########################################################################

# CHECKING AND PLOTTING THE DATA

plot(data_amzn$AMZN.Close)

data_missing<- data_amzn[-400:-600,]
plot(data_missing$AMZN.Close)

# As we can see in the plot that whenever we do not have any trading going
# on for the 200 days in the middle. We know this because R just puts a 
# ST. line to replace the non trading days

################### 1.3.3 CHECKING THE DIMENSION ########################


dim(data_amzn)

# We expected 1260 (approx trading days of 252 * 5 years of data) rows 
# but got 1258 which is not bad

summary(data_amzn$AMZN.Close)


######################### 1.4 Basic Data Manipulation #################


# 1.4.1 Keeping and Deleting One Row


data_amzn[1,] # Keeps only the first row

head(data_amzn[-1,]) # Deletes the first row


# 1.4.2 Keeping First and Last Rows by using concatenate ()

data_amzn[c(1,1258),]

# 1.4.3 Keeping Continous rows by using c()
head(data_amzn)

data_amzn[2:6,] # Extracts rows from row no. 2 through row no. 6


#####################################################################


# If we wanted to calculate the 30-day VWAP, we would need the last 30 close 
# prices and last 30 volume data



(last30<- nrow(data_amzn) -30 +2) # This gives the last 30 trading days 

data_amzn[last30:nrow(data_amzn),]

# 1.4.4 Keeping One Column/ Sub setting columns
# Note that until now, we have been only subsetting the rows


names(data_amzn) # The column names

head(data_amzn[,4]) # Prints out just 1 column
head(data_amzn$AMZN.Close) # Gives the same output as above


# 1.4.5 Deleting One Column

head(data_amzn[,-6]) # Deleting the "Adj Close" column



# 1.4.6 Keeping Non-Contiguous Columns

head(data_amzn[,c(1,4)]) # Follows the same process as subsetting contigous rows i.e
                  # by using c() function



# 1.4.7 Keeping Contiguous Columns

head(data_amzn[,c(4:5)]) # Again, same as subsetting rows i.e. using the colon


# 1.4.8 Keeping Contiguous and Non-Contiguous Columns

## In some instances, we may end up wanting to keep several columns that 
## are not all contiguous but some are


### if we want to keep the AMZN open price, close price, and volume data

head(data_amzn)

head(data_amzn[,c(1,4:5)])


####################### 1.4.9 Keeping Rows and Columns ####################



### For example, suppose we want to calculate the VWAP over the last 
### 30 trading days of 2019.

# This means we need the last 30 days of trading (rows) and we also need
# two variables for the calculation of VWAP i.e. Volume and Price



(last30<- nrow(data_amzn)-30 + 1)

vwap<- data_amzn[last30:nrow(data_amzn), c(4,5)]

head.tail(vwap)

dim(vwap)


#########################################################################

# 1.4.10 Subsetting Time Series Data Using Dates


### Since data_amzn is an xts object, we can subset the data by putting a date
### range inside square brackets. The start and end dates inside 
### the square brackets are separated by a slash (/) and the date range is 
### placed inside quotes

amzn_2018a<- data_amzn["2018-01-01/2018-12-31"]
head.tail(amzn_2018a)


### Alternatively, whether the data is an xts object or not, we can use 
### subset() to, as the name indicates, subset the data. Because data_amzn 
### is an xts object, the date is called using index()

amzn_2018b<- subset(data_amzn, index(data_amzn)>="2018-01-01" &
                      index(data_amzn)<= "2018-12-31")
head.tail(amzn_2018b)


## We can also subset using other column names. Suppose we want to know on 
## which dates AMZN closed higher than $2,000. Also select only 2 columns

(amzn_high_prices<- subset(data_amzn[,4:5], AMZN.Close > 2000))

## The above condition when subsetting dates assumes both conditions must hold.
## We can also subset the data when either of the two conditions hold


## Suppose we want to subset only those data when the AMZN close price was over 
## $2,000 or if its volume was over 20 million.

(subset(data_amzn[,4:5], AMZN.Close > 2000 | AMZN.Volume > 20000000 ))



## We can also subset data that combines different types of conditions. 
## Suppose we want to look at data when AMZN close price was above $2,000 or 
## AMZN trading volume was over 20 million, but only data in 2019.

tail(data_amzn)
(subset(data_amzn,(AMZN.Close > 2000 | AMZN.Volume > 20000000) &
          index(data_amzn)>= "2019-01-01"))




############### 1.4.11 Converting to Weekly Prices #########################

wk<- data_amzn
head.tail(to.weekly(wk))

## The reason we use a shorter dataset name like wk is because to.weekly() 
## uses the dataset name as the prefix when it renames the column header.


# If you wanna extract the 2nd week from the weekly price data

to.weekly(wk)[2] #Check out the Volume that was 14614300 at the end of 2nd week

# Lets confirm this by getting the result for volume at the end of 2nd week
# through a different method:

head(data_amzn)
sum(data_amzn[3:7,5])

## As we can see from the result in the console, the volume matches




##################### 1.4.12 Converting to Monthly Prices ##################


mo<- data_amzn

head(to.monthly(mo))

###########################################################################

ls()


rm(list=ls()) # Warning: Clears the environment

##########################################################################

## let us suppose we made an investment in Amazon (AMZN),
## Alphabet (GOOG), Apple (AAPL), and S&P 500 ETF (SPY) on December 31,2014. 
## How would we know how these investments performed through the end of
## 2019 and which of these investments performed better over that period?


getwd() # Check the working directory


load.data<- function(rawdata, ticker){
  data_raw<- read.csv(rawdata, header=T) #reading the data
  Date<- as.Date(data_raw$Date, format="%Y-%m-%d") # changing the date format
  data_raw<- cbind(Date, data_raw[,-1]) #Concatenating the Date and data_raw df
                                    # after removing the original date column
                                      # in the data_raw dataset
  data_raw<- data_raw[order(data_raw$Date),] # We want all the rows to be 
                                                # ordered by Date 
  data_raw<- xts(data_raw[,2:7], order.by=data_raw$Date) # Changing the type
                                                  # to a xts object
  A<- paste(ticker, ".Open", sep="")
  B<- paste(ticker, ".High", sep="")
  C<- paste(ticker, ".Low", sep="")
  D<- paste(ticker, ".Close", sep="")
  E<- paste(ticker, ".Adjusted", sep="")
  F<- paste(ticker, ".Volume", sep="")
  
  names(data_raw)<- paste(c(A, B,C, D,E,F))
  data_raw<- cbind(data_raw[,1:4], data_raw[,6], data_raw[,5])
  return(data_raw)
  
}



## Another function that we shall write is for the code that will show 
## top 3 lines of the df and bottom 3 lines of df


head.tail<- function(dataset){
  print(head(dataset,3))
  print(tail(dataset,3))
}


#####################################################################

# Step 1: Loading the required data

data_amzn<- load.data("AMZN.csv", "AMZN")
data_gog<- load.data("GOOG.csv","GOOG")
data_spy<- load.data("SPY.csv","SPY")
data_apple<- load.data("AAPL.csv","AAPL")


# Step 2: Combine Close Prices of All Securities into One Dataset

data_close<- cbind(data_amzn$AMZN.Close, data_gog$GOOG.Close,
                   data_spy$SPY.Close, data_apple$AAPL.Close)


names(data_close)<- c("AMZN","GOOG","SPY","AAPL") # used to rename the variables

head(data_close) 

## using cbind() here works seamlessly as these securities have the same
## number of observations.


## Step 3: Normalize Prices

## The starting prices of the different securities are not the
## same. For example, AAPL starts at $110.38, while GOOG starts at $524.96. 
## This makes it hard to compare how well the different securities performed


## I could not find any solutions regarding the normalize() function so, i am
## now trying a different method


install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

head(data_close)

# Simple returns

returns<- Return.calculate(data_close)
head(returns)

# Compounded returns (How much will the $ 1 investment be over time?)

returns<- na.omit(Return.calculate(data_close))
comp_wealth<- cumprod(1+returns)
plot(comp_wealth, main="Growth of $1 investment", legend.loc="topleft")


## Alternative way to draw this plot: 4 mini plots


### I have to try this again sometime in the future ###################
















































