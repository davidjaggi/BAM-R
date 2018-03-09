### Check if Packages are installed otherwise install them #####################
if (!require("pacman")) install.packages("pacman")
pacman::p_load("quantmod",
               "shinydashboard",
               "shiny",
               "xts",
               "shinydashboard",
               "shinyjs",
               "PortfolioAnalytics",
               "PerformanceAnalytics",
               "Quandl",
               "fPortfolio",
               "QRM",
               "sde",
               "timeSeries",
               "car",
               "httr",
               "stringi",
               "futile.logger",
               "rmarkdown",
               "DT",
               "svglite",
               "rsvg",
               "SMFI5",
               "ggplot2",
               "forecast",
               "tseries")

### Loading the installed Packages #############################################
library(pacman)
library(quantmod)
library(shinydashboard)
library(shiny)
library(xts)
library(shinyjs)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(Quandl)
library(fPortfolio)
library(QRM)
library(sde)
library(timeSeries)
library(car)
library(httr)
library(stringi)
library(futile.logger)
library(rmarkdown)
library(DT)
library(svglite)
library(rsvg)
library(SMFI5)
library(ggplot2)
library(forecast)
library(tseries)
# library(googleVis)
# library(quadprog)
# library(xtable)
# library(markdown)
# library(lubridate)
# library(fitdistrplus)
# library(dygraphs)
# library(TTR)
# library(fAssets)
# library(fImport)
# library(corpcor)
# library(Rglpk)