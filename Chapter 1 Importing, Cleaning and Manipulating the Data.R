library(readr)
getwd()

setwd("C:/Users/user/Documents/Coursera/Rbook/data")
file.exists("C:/Users/user/Documents/Coursera/Rbook/data/AMZN.csv")

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

class(data_close)
# Simple returns

returns<- Return.calculate(data_close)
head(returns)

# Compounded returns (How much will the $ 1 investment be over time?)

returns<- na.omit(Return.calculate(data_close))
comp_wealth<- 1000*(cumprod(1+returns))
plot(comp_wealth, main="Growth of $1000 investment", legend.loc="topleft")


## Alternative way to draw this plot: 4 mini plots


fig1<- ggplot(comp_wealth, aes(x=Index, y=comp_wealth$AMZN))+ geom_line()+
  labs(y="Final wealth", x="Date") + ggtitle("AMZN Growth of $1000 2014-2019")

fig2<-ggplot(comp_wealth, aes(x=Index, y=comp_wealth$GOOG)) + geom_line()+
  labs(y="Final wealth", x="Date") + ggtitle("GOOGLE Growth of $1000 2014-2019")

fig3<-ggplot(comp_wealth, aes(x=Index, y=comp_wealth$AAPL)) + geom_line()+
  labs(y="Final wealth", x="Date") + ggtitle("S&P 500 Growth of $1000 2014-2019")

fig4<-ggplot(comp_wealth, aes(x=Index, y=comp_wealth$SPY)) + geom_line()+
  labs(y="Final wealth", x="Date") + ggtitle("Growth of $1000 2014-2019")

# Finally putting the 4 plots together
 
grid.arrange(fig1,fig2, fig3, fig4, ncol=2, nrow=2)


#######################################################################


# 1.6 Simple and Exponential Moving Averages


ma20d<- rollapply(data_amzn$AMZN.Close, 20, mean) 
                  # rollapply() function repeatedly applies the mean () func
                # over 20 day window
ema20d<- EMA(data_amzn$AMZN.Close, n=20)
data<- cbind(data_amzn$AMZN.Close, ma20d, ema20d)
names(data)<- c("Price", "MA_20","EMA_20")
data[18:22,]


# Subset Data to 2019 using xts style date subsetting

data_2019<- data["2019-01-01/2019-12-31"]
head(data_2019)


# Plotting the MA and EMA

dt<- index(data_2019)
(y.range<- range(data_2019))
plot(x=dt, y= data_2019$Price, xlab="Date",ylab="Price", ylim=y.range,
     type="l",lwd=2, main="AMZN price and 20 day MAs")
lines(x=dt, y=data_2019$MA_20, col="blue")
lines(x=dt, y=data_2019$EMA_20, col="red")
legend("bottomright", c("AMZN","Simple MA","Exponential MA"), lwd = c(3,2,1),
       col = c("black","blue","red"))


########################################################################

# 1.7 Volume-Weighted Average Price for the last 30 days of 2019


# Step 1: Subset Data to Last 30 Trading Days

(thirty<- nrow(data_amzn)-30+1)

last30<- data_amzn[thirty:nrow(data_amzn), 4:5] 
head.tail(last30)

# Step 2: Calculate the Weighted Volume

tot<- sum(last30$AMZN.Volume)

last30$vol_weight<- last30$AMZN.Volume/tot
head(last30)

# Step 3: Calculated the Weighted Price

last30$price.wt<- last30$vol_weight * last30$AMZN.Close

head.tail(last30)

# Step 4: Calculate the VWAP

(vwap<-sum(last30$price.wt))

#########################################################################

# 1.8 Plotting a Candlestick Chart


### We can use chartSeries() in the quantmod package to create a candlestick 
### chart. However, we would first have to convert the data into an 
### open-high-low-close (OHLC) object.



# Step 1: Setup Monthly Data

ohlc<- load.data("AMZN.csv","AMZN")
ohlc<- to.monthly(ohlc)


    ## dropped the row for December 2014 (Row 1) and the adjusted close 
    ## variable (Column 6).

ohlc<- ohlc[-1,-6]
class(ohlc) # Its xts zoo right now, but we wanna convert the object from 
            # xts zoo to "quantmod.ohlc" object
head(ohlc)


# Step 2: Convert to an OHLC Object

amzn.ohlc<- as.quantmod.OHLC(ohlc, col.names = c("Open","High","Low","Close",
                                            "Volume"))
class(amzn.ohlc) # As we can see now that the object type has changed from 
            # "xts zoo" to "quantmode.ohlc zoo"


# Step 3: Plot OHLC Data


chartSeries(amzn.ohlc, theme="white",name="AMZN OHLC")


####################################################################

# 2-Axis Price and Volume Chart


# Because of the different scales of the price and
# volume, we cannot only use one axis.

head(data_amzn)


prc_vol= data_amzn[-1, c(4:5)] # selecting only the required components of data
class(prc_vol)

date=as.Date(index(prc_vol)) # Creating new variable called "date"

prc_vol= data.frame(date, prc_vol) # Converting to df

names(prc_vol)<- c("Date","Price","Volume") # Changing column names

prc_vol$Volume<- prc_vol$Volume/1000000 # Changing the volume data into millions

rownames(prc_vol)<- seq(1, nrow(prc_vol)) # Changing the rownames

range(prc_vol$Price)


############### I AM BORED ! LETS MOVE TO SOMETHING ELSE ##########


########################## Security's returns

## 2.1 Price Returns



head(data_apple)


































































































