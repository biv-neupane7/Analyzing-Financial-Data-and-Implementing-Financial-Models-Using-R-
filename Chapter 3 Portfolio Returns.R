
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

###########################################################################

# loading the data

amzn<-load.data("AMZN.csv", "AMZN")
goog<- load.data("GOOG.csv", "GOOG")
aapl<- load.data("AAPL.csv","AAPL")

# Returns

rets.amzn<- Delt(amzn$AMZN.Adjusted)
rets.goog<- Delt(goog$GOOG.Adjusted)
rets.aapl<- Delt(aapl$AAPL.Adjusted)


# Concatenate columns

rets<- cbind(rets.amzn,rets.goog,rets.aapl)

# Change column names

head(rets)
names(rets)<- c("AMZN","GOOG","AAPL")

# removing the 1st row

rets<- rets[-1,]
head(rets)


# Gross daily return

port.ret<- 1+ rets
head(port.ret)

# Cumulate gross daily returns to obtain cumulative gross return over the 
# entire period

cum.ret<- cumprod(port.ret)
tail(cum.ret)

# Cum net return over the period

(cum.ret<- cum.ret[nrow(cum.ret)]-1)

# 3.2 Portfolio Returns Using Matrix Algebra #############################


# Step 1: Create a Row Vector of Weights


i.amzn<- 50000
i.goog<- 30000
i.aapl<- 20000

(i.total<- i.amzn+i.aapl+i.goog)

w.amzn<- i.amzn/i.total
w.goog<- i.goog/i.total
w.aapl<- i.aapl/i.total

(weight<- c(w.amzn, w.goog, w.aapl)) # To create a row vector of weights, we use c()

(mat.weight<- matrix(weight, 1)) # The second argument in matrix() tells R 
                      # that we should put all of the elements in one row.

# Step 2: Create a Column Vector of Returns

cum.ret # This is the column vector i already have 

(mat.returns<- matrix(cum.ret, 3)) # The second argument
                        # in matrix() is now 3, which tells R to split the 
                        # data into three rows


# Step 3: Calculate Portfolio Returns


(port_ret<- as.numeric(mat.weight %*% mat.returns))
                          # In the above, we added as.numeric() to the output.
                          # Otherwise, the output will look like a matrix 
                          # instead of a number























