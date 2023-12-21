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
