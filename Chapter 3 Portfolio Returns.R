
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


##########################################################################


# 3.3 Constructing Benchmark Portfolio Returns


################################### 3.3.1 Quarterly Returns the Long Way

# Step 1: Calculate 1Q 2019 Cumulative Return

# We first subset the returns data to only include returns from 
# January 1, 2019 to March 31, 2019 (i.e., the first quarter
#  of 2019).


rets.q1<- rets["2019-01-01/2019-03-31"]
head(rets.q1)

    # Gross daily returns for the 1st quarter of 2019

grets.q1<- 1+ rets.q1
tail(grets.q1)

    # Net daily cumulative returns


crets.q1<- apply(grets.q1, 2, cumprod) # apply() to apply cumprod() to each 
                                    # of the three columns in grets.q1.
                                # 2 indicates that the function shall be 
                            # applied to the columns


    # Making it NET

(crets.q1<- crets.q1[nrow(crets.q1), ]-1)


# This process is again applied to each of the remaining 3 quarters.
# Hence, it is not surprisingly the long way

# I dont want to do this repitative process again and again. So, lets try 
# a different approach




# 3.3.2 Quarterly Returns the Shorter Way ##################################


## Step 1: Construct Dataset with Quarterly Prices


    # Subsetting the daily adjusted prices from each of the stocks

prc.amzn<- amzn$AMZN.Adjusted
prc.goog<- goog$GOOG.Adjusted
prc.aapl<- aapl$AAPL.Adjusted

    # Converting the daily prices to quarterly

qtr.amzn<- to.quarterly(prc.amzn)
qtr.goog<- to.quarterly(prc.goog)
qtr.aapl<- to.quarterly(prc.aapl)


## Step 2: Calculate Quarterly Returns


    # Subsetting the Close column and calculating returns for each stock

qtr.rets<- cbind(Delt(qtr.amzn[,4]), Delt(qtr.goog[,4]),
                 Delt(qtr.aapl[,4]))

    # Changing column names

names(qtr.rets)<- c("AMZN","GOOG","AAPL")

    # Removing the 1st row

qtr.rets<- qtr.rets[-1,]

tail(qtr.rets, 4)


# 3.3.3 Equal-Weighted Portfolio ########################################


# we assume that the investment is made on December 31, 2018 and we want to 
# know what the cumulative return would be at December 31, 2019.



## Step 1: Calculate the Value of the Portfolio at the End of 1Q 2019

    ## creating a variable for the initial investment

ew.i0<- 1000

    ## grow that amount by the average return for the securities in the 
    ## portfolio. Since we have 3 securities in our port, each gets 33.33% 
    ## weight

    ## This 33.33 % weight is equivalent to just calculating the avg 

(ew.i1<- ew.i0 * (1+ mean(crets.q1)))


## Step 2: Follow the same procedure and calculate value for other quarters
## as well


# (ew.i2<- ew.i1 * (1+mean(crets.q2))) and so on....


###### 3.3.4 Value-Weighted Portfolio ###################################


# Step 1: Calculate Weights Based on Market Cap as of December 31, 2018


    # market cap of securities

mcl.amzn<- 737.47
mcl.goog<- 720.32
mcl.aapl<- 746.08

    # Total market cap

(mcl.tot<- sum(mcl.amzn, mcl.goog, mcl.aapl))

    # weights 

w1.amzn<- mcl.amzn/mcl.tot
w1.goog<- mcl.goog/mcl.tot
w1.aapl<- mcl.aapl/mcl.tot


# Step 2: Use Weights Calculated Above to Determine How Much Is Invested in
# Each Security

vw.i0<- 1000

(vw.i0.amzn<- vw.i0 * w1.amzn)
(vw.i0.goog<- vw.i0 * wl.goog)
(vw.i0.aapl<- vw.i0 * wl.aapl)


# Step 3: Calculate Value of Portfolio as of March 31, 2019


(vw.i1.amzn<- vw.i0.amzn * (1 + crets.q1[1]))

(vw.i1.goog<- vw.i0.amzn * (1+ crets.q1[2]))

(vw.i1.aapl<- vw.i0.aapl * (1+ crets.q1[3]))


(vw.i1<- sum(vw.i1.amzn, vw.i1.goog, vw.i1.aapl))


# Follow the same procedure as above for the other quarters





























