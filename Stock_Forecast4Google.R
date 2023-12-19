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
