
library(AER)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)


USMacroSWQ<- read_xlsx("D:/231bcada34/econometrics/us_macro_quarterly.xlsx",
                        sheet = 1,
                        col_types = c("text", rep("numeric", 9)))
USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", 
                          "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCS")
GDP <- xts(USMacroSWQ$GDPC96,USMacroSWQ$Date)["1960::2013"]
GDP
help()
GDPGrowth <- xts(400 * log(GDP/lag(GDP)))
GDPGrowth
plot(log(as.zoo(GDP)),
     col = "black",
     lwd = 9,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Quarterly Real GDP")
quants <- function(series) 
{s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s)))))}
quants(GDP["2011-07::2013-01"])
acf(na.omit(GDPGrowth),lag.max = 4,plot = F)
# define series as xts objects
USUnemp <- xts(USMacroSWQ$UNRATE, USMacroSWQ$Date)["1960::2013"]
USUnemp
DollarPoundFX <- xts(USMacroSWQ$EXUSUK, USMacroSWQ$Date)["1960::2013"]
DollarPoundFX
JPIndProd <-xts(log(USMacroSWQ$JAPAN_IP),USMacroSWQ$Date)["1960::2013"]
JPIndProd 
library(dynlm)
# attach NYSESW data
data("NYSESW")  
NYSESW <- xts(Delt(NYSESW))
# divide plotting area into 2x2 matrix
par(mfrow = c(2,2))
# plot the series 
plot(as.zoo(USUnemp), 
     col = "steelblue", 
     lwd = 2, 
     ylab = "Percent",
     xlab = "Date", 
     main = "US Unemployment Rate", 
     cex.main = 1)

plot(as.zoo(DollarPoundFX),
     col = "steelblue",
     lwd = 2,
     ylab = "Dollar per pound",
     xlab = "Date",
     main = "U.S. Dollar / B. Pound Exchange Rate",
     cex.main = 1)

plot(as.zoo(JPIndProd),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Japanese Industrial Production",
     cex.main = 1)

plot(as.zoo(NYSESW),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent per Day",
     xlab = "Date",
     main = "New York Stock Exchange Composite Index",
     cex.main = 1)


#autoregression
#subset data 

GDPGRSub <- GDPGrowth["1962::2012"] 
GDPGRSub
# estimate the model
ar.ols(GDPGRSub, order.max = 1, intercept = T) 

# length of data set 
N <-length(GDPGRSub) 
N
GDPGR_level <- as.numeric(GDPGRSub[-1]) 
GDPGR_lags <- as.numeric(GDPGRSub[-N]) 
# estimate the model
armod <- lm(GDPGR_level ~ GDPGR_lags)
armod 

#to obtain a robust summary on the estimated regression coefficients.
coeftest(armod) 


