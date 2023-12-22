# Stock Forecasting with Google
# Outline
# Time series is a sequential set of data points, measured typically over successive times. It is mathematically defined as a set of vectors {Yt,t∈Z}
# where t represents the time. The data used for all the below method (ARIMA) were historical daily prices of the Google. The goal of this study is to model and forecast financial time series using the stochastic process (ARIMA).

# Load Packages
# Important librarys
# The packages used in this study are listed below.

# dates manipulation
suppressPackageStartupMessages(library(lubridate))
# enhanced summary statistics
suppressPackageStartupMessages(library(fBasics))
# coefficients significance tests
suppressPackageStartupMessages(library(lmtest))
# unit rooit test
suppressPackageStartupMessages(library(urca))
# visualization
suppressPackageStartupMessages(library(ggplot2))
# getting financial data
suppressPackageStartupMessages(library(quantmod))
# calculating returns
suppressPackageStartupMessages(library(PerformanceAnalytics))
# GARCH modeling
suppressPackageStartupMessages(library(rugarch))
# ARCH test
suppressPackageStartupMessages(library(FinTS))
# ARMA modeling
suppressPackageStartupMessages(library(forecast))
# structural changes
suppressPackageStartupMessages(library(strucchange))
# ARMA order identification
suppressPackageStartupMessages(library(TSA))
library(tseries)
library(timeSeries)
library(xts)
library(pastecs)


library(readxl)
library(zoo)

# Import Data from Yahoo! Finance
# Getting Data
# The Google Stock dataset having a total number of 105 observations and 7 variables. The data downloaded from Yahoo! Finance. The closing price was chosen to be modeled and predicted. Which are covered the period from February 7, 2005 to July 7, 2005. We will analyze the dataset to identify the order of an autoregressive model.

# Taking advantage of the getSymbols() function made available within quantmod package we get Google stock.

GOOGLE = getSymbols('GOOG', from='2005-02-07', to='2005-07-08',auto.assign = FALSE)
## 'getSymbols' currently uses auto.assign=TRUE by default, but will
## use auto.assign=FALSE in 0.5-0. You will still be able to use
## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
## and getOption("getSymbols.auto.assign") will still be checked for
## alternate defaults.
##
## This message is shown once per session and may be disabled by setting
## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.
GOOGLE = na.omit(GOOGLE)
head(GOOGLE)

# Select the relevant close price series
stock_prices = GOOGLE[,4]
head(stock_prices)
tail(stock_prices)

# Import data from cloud
# The Google Stock dataset downloded from: https://online.stat.psu.edu

google_stock <- read_excel("google_stock.xlsx")
# first 6 observations
head(google_stock)

tail(google_stock)

#Descriptive statistics
summary(google_stock)

# For more descriptive statistics, use stat.desc() from the package {pastecs}:

stat.desc(google_stock)

# Visualize the time series
# More precisely, we have available OHLC (Open, High, Low, Close) index value, adjusted close value and trade volume. Here we can see the corresponding chart as produced by the chartSeries within the quantmod package.

chartSeries(GOOGLE, type = "bars", theme="white",main="Google Stock")

# Time series patterns

# Trend: pattern exists when there is a long-term increase or decrease in the data.

# Seasonal: pattern exists when a series is influenced by seasonal factors (e.g., the quarter of the year, the month, or day of the week).

# Cyclic: pattern exists when data exhibit rises and falls that are not of fixed period (duration usually of at least 2 years).

# for a basic plot, all you need is
plot(google_stock, type='l', col=4, main="Time series Plot of price Google", xlab='Date: from February 7, 2005 to July 23, 2005', ylab='Stock')

autoplot(stock_prices,main="Google stock")

#The graph of the series shows a stagnation of prices at the beginning of the period until April 4, 2005. Then the price have a increase trend. Clearly, there is no seasonality (no repeating patterns in the data series). The average of the series tends to change. The graph shows that the series is not stationary, to ensure the stationarity of the series, we refer to the test “Augmented Dickey-Fuller test”.

#Stationarity test of Google stock
#Augmented Dickey-Fuller test
#The most popular approach, the Augmented Dickey-Fuller (ADF) stationarity test, given by Dickey and Fuller in 1979, is used to examine the stationarity of daily stock. This test is based on two assumptions:

#  H0 The null hypothesis: The series can be represented by a unit root, so it is not not stationary.

# H1 The alternative hypothesis: Rejecting the null hypothesis, suggests that the series has no unit root, which means that it is stationary.

# The p-value of 0.05 from the ADF test tells us that the series is stationary. If the series were to be non-stationary, we would have first differenced the returns series to make it stationary.

adf.test(google_stock$price)
##
##  Augmented Dickey-Fuller Test
##
## data:  google_stock$price
## Dickey-Fuller = -1.9804, Lag order = 4, p-value = 0.5848
## alternative hypothesis: stationary
# The results of the ADF test are shown above. At the 5%
# significance level, the null hypothesis (H0) of the existence of a unit root in the daily stock is accepted (because the p-value (0.5848) higher than 5%). These results indicate that the Google series is not stationary.

# Identifying non-stationary series
acf(stock_prices)

# The ACF of price don’t drops to zero relatively quickly and the ACF of price decreases slowly also the value of autcorrelation is often large and positive, so we can say that the series is not stationary!

#  Stationarize the Series
# Definition of Stationarity
# If {yt}
# is a stationary time series, then for all k
# the distribution of (yt,…,yt+k)
# does not depend on t
#. Transformations help to stabilize the variance. For ARIMA modelling, we also need to stabilize the mean

# There are three commonly used techniques to make a time series stationary:

#  Detrending: Here, we simply remove the trend component from the time series.

# Differencing: This is the commonly used technique to remove non-stationarity. Here we try to model the differences of the terms and not the actual term. This differencing is called as the Integration part in AR(I)MA.

# Seasonality: Seasonality can easily be incorporated in the ARIMA model directly.

# In our case we are going to calculate the log return to make the series stationary, which is exactly the Differencing .

# Simple and log returns

# Simple returns are defined as:

#  Rt:=PtPt−1−1=Pt−Pt−1Pt−1
# log returns are defined as:

#  rt:=lnPtPt−1=ln(1+Rt)
# We compute log returns by taking advantage of CalculateReturns within PerformanceAnalytics package.

# google_return <- CalculateReturns(stock_prices, method = "log")
# google_return <- na.omit(google_return)
# head(google_return)

# Note: The first value in the table is equal to nan because, Pt−Pt−1=nan
# and the simple efficiency Rt=Pt−Pt−1Pt−1=nan
.

tail(google_return)

# This gives the following plot.

# plot(google_return,main='Google return', xlab='Date', ylab='Log(Return)')

# We see sharp increases and decreases in volatility can be eye-balled.
# ∇Pt=Pt−Pt−1
# Note: Differencing helps to stabilize the mean. We can check by differencing the time series once, as follows:

# Time Series Differencing
diff_price = diff(stock_prices)
diff_price = na.omit(diff_price)
plot(diff_price, type="l",main="1st order diff of google stock",ylab="Price Differences",xlab="Days")

# From plot we can see that the trend does not appear, but the first difference series is varied during the time (around zero). In particular, the variance!!

 # We can also remark that the series like white noise!

#  Definition of White Noise
#A time series {wt:t=1,2,…,n}
#is if the variables w1,w2,…,wn
#are independent and identically distributed (i.i.d) with a mean of zero. This implies that the variables all have the same variance σ2and Cor(wi,wj)=0∀i≠j.
# If, in addition, the variables also follow a normal distribution (i.e., wt∼N(0,σ2)) the series is called Gaussian white noise. For a white noise series, all the ACFs are zero. In practice, if all sample ACFs are close to zero, then the series is a white noise series.

# Note: Often, we consider the Gaussian MA(q) model, where {εn} is a Gaussian white noise process: The order q moving average model, abbreviated to MA(q), is
# yn=εn+θ1εn−1+…+θqεn−q
# where {εn} is a white noise process.

# Now applying the ADF test on the differenced time series shows below that the time series is stationary:

  # Apply Unit Root Tests on Differenced data
  # Augmented Dickey-Fuller Test ADF
  adf.test(diff_price,alternative="stationary")

#  Find Optimal Parameters
#  Autocorrelation and Partial Autocorrelation
#  The coefficient of correlation between two values in a time series is called the autocorrelation function (ACF) For example the ACF for a time series Pt  is given by:

#    Corr(Pt,Pt−1).
#  This value of k  is the time gap being considered and is called the lag. A lag 1 autocorrelation (i.e., k=1 in the above) is the correlation between values that are one time period apart. More generally, a lag k  autocorrelation is the correlation between values that are k  time periods apart.

#  The PACF is most useful for identifying the order of an autoregressive model. Specifically, sample partial autocorrelations that are significantly different from 0 indicate lagged terms of y  that are useful predictors of yt
  .

#  The parameters p  , and q  can be found using ACF and PACF plots.

  # ARIMA Models Specification
  # Normal and Partial Autocorrelation Functions ACF & PACF
  acf(diff_price)
#  Based on plot above the daily return (or 1st difference) of google stock are close to white noise (all the ACFs are zero)!

#    The ACF of difference drops to zero relatively quickly, this proves that the series is stationary!

  pacf(diff_price)
    pacf(diff_price)

    The above ARIMA (0,1,0) model has an AIC of 463.61 and BIC of 468.9.Let us also check if the residuals are from a white noise series by performing the following:

      #Check Residuals forARIMA (0,1,0) model
      checkresiduals(model)
