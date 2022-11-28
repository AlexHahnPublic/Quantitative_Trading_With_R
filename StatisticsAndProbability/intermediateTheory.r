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

#spy_vxx <- getSymbols(c("SPY","VXX"),from = as.Date("2009-01-01"), to = as.Date("2014-01-01"))


## stock_list_VXX <- c("^VXX")

## master_df_VXX <- NULL
## for (idx in seq(length(stock_list_VXX))){
##     stock_index <- stock_list_VXX[idx]
##     getSymbols(stock_index, verbose = TRUE, src = "yahoo",
##                from=start_date,to=end_date)
##     temp_df <- as.data.frame(get(stock_index))
##     temp_df$Date <- row.names(temp_df)
##     temp_df$Index <- stock_index
##     row.names(temp_df) <- NULL
##     colnames(temp_df) <- c("Open", "High", "Low", "Close",
##                            "Volume", "Adjusted", "Date", "Index")
##     temp_df <- temp_df[c("Date", "Index", "Open", "High",
##                         "Low", "Close", "Volume", "Adjusted")]
##     VXX <- rbind(master_df, temp_df)

##     VXX$Date <- ymd(VXX$Date)
##     VXX <- xts(VXX[,-1], order.by=VXX[,1]) #converting to xts, note there is some more complexity here to actually use
## }


## for (idx in seq(length(stock_list))){
##     stock_index <- stock_list[idx]
##     getSymbols(stock_index, verbose = TRUE, src = "yahoo",
##                from=start_date,to=end_date)
##     temp_df <- as.data.frame(get(stock_index))
##     temp_df$Date <- row.names(temp_df)
##     temp_df$Index <- stock_index
##     row.names(temp_df) <- NULL
##     colnames(temp_df) <- c("Open", "High", "Low", "Close",
##                  x          "Volume", "Adjusted", "Date", "Index")
##     temp_df <- temp_df[c("Date", "Index", "Open", "High",
##                         "Low", "Close", "Volume", "Adjusted")]
##     SPY <- rbind(master_df, temp_df)

##     SPY$Date <- ymd(SPY$Date)
##     SPY <- xts(SPY[,-1], order.by=SPY[,1]) #converting to xts, note there is some more complexity here to actually use
## }



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

# example of outliers effect on deviance from normality
set.seed(129)
normal.numbers <- rnorm(5000,0,1)
correct.shapiro.test <- shapiro.test(normal.numbers)

# Corrupt a single point, as if jsut manually entered/ input wrong
normal.numbers[50] <- 1000 #with a distribution centered around 0 with a sd of 1 (and 5000 points) a single point of 1000 is crazy unlikely
failed.shapiro.test <- shapiro.test(normal.numbers)

SPY.df <- read.csv("SPY.csv")
VIX.df <- read.csv("VIX.csv")
SPY.df$Date <- ymd(SPY.df$Date)
VIX.df$Date <- ymd(VIX.df$Date)

SPY.df$SPYCloseReturn <- c(0,diff(log(as.double(SPY.df$Close))))
VIX.df$VIXCloseReturn <- c(0,diff(log(as.double(VIX.df$Close))))

# manually enter two outliers for vix
VIX.df$VIXCloseReturn[50] <- 1.8 # a 180% return
VIX.df$VIXCloseReturn[1000] <- 2.1 # a 210% return

m <- merge(SPY.df,VIX.df,by="Date")

rets <- m[,c("Date", "SPYCloseReturn","VIXCloseReturn"),]

# for identifying outliers:
outliers <- which(VIX.df$VIXCloseReturn > 1.0)

#Scatter plot with outliers
pdf("SPYvsVIXWithOutliers.pdf")
plot(m$SPYCloseReturn, m$VIXCloseReturn)
dev.off()

# A negative 60% correlation, which makes sense
cor.with.outliers <- cor(m$SPYCloseReturn, m$VIXCloseReturn)

# filter outliers to see what happens to correlation:
if(length(outliers) > 0) {
    m <- m[-outliers,]
}

# Scatter plot without outliers
pdf("SPYvsVIXWithoutOutliers.pdf")
plot(m$SPYCloseReturn, m$VIXCloseReturn)
dev.off()

#increases to -81% (the outliers were only in VIX space, removing them resulted in a stronger negative correlation)
cor.without.outliers <- cor(m$SPYCloseReturn, m$VIXCloseReturn)

# for a correlation matrix simply enter only the numeric columns aand all pairwise (and symmetric) correlations will be computed
cor.matrix <- cor(m[,c("SPYCloseReturn", "VIXCloseReturn")])

#create a linear regression of spy and vix close returns:
lin.reg <- lm(VIXCloseReturn ~ SPYCloseReturn, data = m)


#summary(lin.reg) returns all the relevant statistics

# Collecting the coeefficients (intercept and slope)
intercept  <- lin.reg$coefficients[1]
slope <- lin.reg$coefficients[2]

# a simple way of superimposing the line of best fit on the scatter plot (I have more advanced linear plotting with stats on the chart elsewhere)
pdf("SPYvsVIXsLinearFit.pdf")
plot(m$SPYCloseReturn, m$VIXCloseReturn)
abline(a=intercept, b=slope)
abline(h=0)
abline(v=0)
dev.off()

# Diagnostic plot of the residuals to examine

pdf("SPYvsVIXsResiduals.pdf")
par(mfrow=c(2,2))
plot(lin.reg$residuals,
     main = "Residuals through time",
     xlab = "Days",
     ylab = "Residuals")
hist(lin.reg$residuals, breaks=100,
     main = "Distribution of residuals", #to check normality
     xlab = "Residuals"
     )
qqnorm(lin.reg$residuals)
qqline(lin.reg$residuals)
acf(lin.reg$residuals,
    main = "Autocorrelation")
dev.off()

# Explore whether there is a linear relationship between yesterday's return and today's.
# note, lag() requires a time series so lets make our input date a time series first:

SPY.ts <- xts(m[,c("SPYCloseReturn")], order.by = ymd(m[,c("Date")]))
SPY.ts.lagged <- lag(SPY.ts, k=1)

VIX.ts <- xts(m[,c("VIXCloseReturn")], order.by = ymd(m[,c("Date")]))


# scatterplot of lagged SPY vs VIX
pdf("laggedSPYvsVIXReturns.pdf")
plot(as.numeric(SPY.ts.lagged), as.numeric(VIX.ts),
     main = "Scatter plot of SPY lagged vs VXX",
     xlab = "SPY Return lagged",
     ylab = "VXX Return",
     cex.main = 0.8,
     cex.axis = 0.8,
     cex.lab = 0.8)
grid()
dev.off()

lagged.lm <- lm(VIX.ts ~ SPY.ts.lagged)

# note that lagging VIX and checking if it is a leading indicator of SPY returns similar results
# we can perform this analysis quickly with the ccf() function. similar to the acf() function,
# but it takes two time series and returns their various lagged pairwise correlations

pdf("crossCorrelationLagsVIXvsSPY.pdf")
ccf(as.numeric(SPY.ts), as.numeric(VIX.ts),
    main = "Cross correlation between SPY and VXX",
    ylab = "Cross correlation", xlab = "Lag", cex.main = 0.8,
    cex.lab = 0.8, cex.axis = 0.8)
dev.off()
