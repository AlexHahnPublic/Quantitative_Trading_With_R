#####################################################################
############ Intermediate Theory for Statistical Trading ############
#####################################################################

library(lubridate)
library(quantmod)
library(urca)

#Todo: make candles retrieval into a simple function call, book one is broken and shouldn't repeat this too much
# Examine distributions of SPY
stock_list <- c("SPY")#, "GOOG") # add more symbols here
start_date <- ymd(20070101)#Sys.Date()-5000
end_date <- ymd(20140101)#Sys.Date()
master_df <- NULL
for (idx in seq(length(stock_list))){
    stock_index <- stock_list[idx]
    getSymbols(stock_index, verbose = TRUE, src = "yahoo",
               from=start_date,to=end_date)
    temp_df <- as.data.frame(get(stock_index))
    temp_df$Date <- row.names(temp_df)
    temp_df$Index <- stock_index
    row.names(temp_df) <- NULL
    colnames(temp_df) <- c("Open", "High", "Low", "Close",
                           "Volume", "Adjusted", "Date", "Index")
    temp_df <- temp_df[c("Date", "Index", "Open", "High",
                        "Low", "Close", "Volume", "Adjusted")]
    SPY <- rbind(master_df, temp_df)

    SPY$Date <- ymd(SPY$Date)
    SPY <- xts(SPY[,-1], order.by=SPY[,1]) #converting to xts, note there is some more complexity here to actually use
}

# Basic statistics
prices <- as.double(SPY$Adjusted)
mean_prices <- round(mean(prices, na.rm=TRUE), 2)
sd_prices <- round(sd(prices, na.rm=TRUE), 2)

pdf("hist_SPY_Prices_2007_2014.pdf")
hist(prices, breaks=100,prob=T, cex.main=0.9)
abline(v = mean_prices, lwd=2)
legend("topright", cex = 0.8, border = NULL, bty="n",
       paste("mean=", mean_prices, "; sd=", sd_prices))
dev.off()

# let's examine these mean and sd characterized distrubution in 4 distinct and disjoint periods
plot_4_ranges <- function(data, start, end, title) {
    # Set the plot window to be 2 rows and 2 columns
    par(mfrow = c(2,2))
    #browser()
    for (i in 1:4) {
        ## Create a string with the appropriate date range
        range <- paste(start[i], "::", end[i], sep="")

        ## Create the price vector and corresponding statistics
        time_series <- as.double(data[range]$Adjusted)

        mean_data <- round(mean(time_series, na.rm = TRUE), 3)
        sd_data <- round(sd(time_series, na.rm = TRUE), 3)

        ## Plot the histogram with stats in the legend
        hist_title <- paste(title, range)

        hist(time_series, breaks = 100, prob=TRUE,
             xlab = "", main = hist_title, cex.main = 0.8)
        legend("topright", cex = 0.7, bty = 'n',
               paste("mean=", mean_data, "; sd=", sd_data))
    }
    # Reset the plot window
    par(mfrow = c(1, 1))
}



## Define start and end dates of interest
begin_dates <- c("2007-01-01", "2008-06-06", "2009-10-10", "2011-03-03")
end_dates <- c("2008-06-05", "2009-09-09", "2010-12-30", "2013-01-06")

## Create plots
pdf("nonstationarityOfSPYPrices.pdf")
plot_4_ranges(SPY, begin_dates, end_dates, "SPY prices for:")
dev.off()

## Comparing prices to stationarity of log returns (for proof of log returns approximating returns see logReturnApproximationProof.pdf
## given that prices themselves have a log normal distribution percentage returns can be simplistically modelled as having a normal distribution
SPY$logReturns <- c(0,diff(log(as.double(SPY$Adjusted))))

## no need to repeat, just use a dataframe and abstract, book used all xts objects but this is cumbersome
plot_4_ranges_logReturn <- function(data, start, end, title) {
    # Set the plot window to be 2 rows and 2 columns
    par(mfrow = c(2,2))
    #browser()
    for (i in 1:4) {
        ## Create a string with the appropriate date range
        range <- paste(start[i], "::", end[i], sep="")

        ## Create the price vector and corresponding statistics
        time_series <- as.double(SPY[range]$logReturns)

        mean_data <- round(mean(time_series, na.rm = TRUE), 3)
        sd_data <- round(sd(time_series, na.rm = TRUE), 3)

        ## Plot the histogram with stats in the legend
        hist_title <- paste(title, range)

        hist(time_series, breaks = 100, prob=TRUE,
             xlab = "", main = hist_title, cex.main = 0.8)
        legend("topright", cex = 0.7, bty = 'n',
               paste("mean=", mean_data, "; sd=", sd_data))
    }
    # Reset the plot window
    par(mfrow = c(1, 1))
}

pdf("stationarityOfSpyReturns.pdf")
plot_4_ranges_logReturn(SPY, begin_dates, end_dates, "SPY prices for:")
dev.off()

## let's run some stationarity tests using R's urca package
test <-ur.kpss(as.double(SPY$Adjusted))
class(test) # an S4 object
ts <- test@teststat # 8.8499
## examine critical values
test@cval
##                 10pct  5pct 2.5pct  1pct
## critical values 0.347 0.463  0.574 0.739
# the test stat critical value is large enough that we can reject the null hypothesis that the time series is stationary
spy_returns <- SPY$logReturns
test_returns <- ur.kpss(spy_returns)
test_returns@teststat# .3302
## referring to the above critical values we can reject the null hypothesis that the time series is stationary at the 10% threshold
# subsetting to more "stable" regimes, we cannot reject the null hypothesis that the times series of the returns is stationary

## returns seem to be normally distributed
mu <- mean(spy_returns, na.rm=TRUE)
sigma <- sd(spy_returns, na.rm=TRUE)
x <- seq(-5*sigma, 5*sigma, length=nrow(SPY))

pdf("spyLeptokyriticDistributedReturns.pdf")
hist(spy_returns, breaks=100,
     main ="Histogram of SPY returns",
     cex.main = 0.8, prob=TRUE)
lines(x, dnorm(x, mu, sigma), col = "red", lwd=2)
dev.off()

pdf("spyLeptokyriticReturnsQQLine.pdf")
par(mfrow=c(1,2))

#SPY data
qqnorm(as.numeric(spy_returns),
       main="SPY empirical returns qqplot()",
       cex.main = 0.8)
qqline(as.numeric(spy_returns), lwd=2)
grid()

# Normal distribution data
normal_data <- rnorm(nrow(spy_returns), mean = mu, sd = sigma)
qqnorm(normal_data, main = "Normal returns", cex.main=0.8)
qqline(normal_data, lwd=2)
grid()
dev.off()
