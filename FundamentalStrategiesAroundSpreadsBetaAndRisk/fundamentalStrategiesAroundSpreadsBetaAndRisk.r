library(quantmod)
library(xts)
library(lubridate)
library(ggplot2)
library(gridExtra)


# only need to load in once. uncomment, run, comment again, don't overwrite objects, avoid waiting for network IO
## pepsi <- getSymbols("PEP", from = "2013-01-01",
##                     to = "2014-01-01", adjust = TRUE, auto.assign = FALSE)#, src = "yahoo")

## coke <- getSymbols("COKE", from = "2013-01-01",
##                    to = "2014-01-01", adjust = TRUE, auto.assign = FALSE)#, src="yahoo")

Sys.setenv(TZ = "UTC")

#prices <- cbind(pepsi[,6], coke[,6])
prices <- cbind(pepsi[,c("PEP.Adjusted")], coke[,c("COKE.Adjusted")]) # I think this is better form, don't use col index numbers

price_changes <- apply(prices, 2, diff)

pdf("cokeVsPepsiPriceChanges.pdf")
plot(price_changes[,c("PEP.Adjusted")], price_changes[,c("COKE.Adjusted")],
     xlab = "Coke price changes",
     ylab = "PEP price changes",
     main = "Pepsi vs Coke price changes",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
grid()
dev.off()

ans <- lm(price_changes[,c("PEP.Adjusted")] ~ price_changes[,c("COKE.Adjusted")])
beta <- ans$coefficients[2]

# Although similar the price change of Coke (dependendant) as explained by the price change in PEP (now independant) is
ans2 <- lm(price_changes[,c("COKE.Adjusted")] ~ price_changes[,c("PEP.Adjusted")])
beta2 <- ans2$coefficients[2]

# Notice that you could proxy this by taking the inverse of the slope coeff but running the actual regression again is not the same
# A Total least squares regression  alleviates this by attempting to explain the variability of a system in terms of both timeseries

# Example for SPY vs AAPL price difference principal components (similary comment/uncomment to load once as above)
## SPY <- getSymbols('SPY', from = '2008-01-01', to = '2013-12-31', adjust = TRUE, auto.assign = FALSE)
## AAPL <- getSymbols('AAPL', from = '2008-01-01', to = '2013-12-31', adjust = TRUE, auto.assign = FALSE)

## SPY <- getSymbols('SPY', from = '2011-01-01', to = '2012-12-31', adjust = TRUE, auto.assign = FALSE)
## AAPL <- getSymbols('AAPL', from = '2011-01-01', to = '2012-12-31', adjust = TRUE, auto.assign = FALSE)

spy.x <- diff(as.numeric(SPY[,c("SPY.Close")]))
aapl.y <- diff(as.numeric(AAPL[,c("AAPL.Close")]))

pdf("principalComponentTotalLeastSquaresSPYvsAAPL.pdf")
plot(spy.x, aapl.y, main = "Principal Component Total Least Squares regression SPY vs AAPL price diffs",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(lm(aapl.y~spy.x))
abline(lm(spy.x~aapl.y), lty = 2)
grid()

# Total least squares regression
r <- prcomp(~ spy.x + aapl.y)
slope <- r$rotation[2, 1] / r$rotation[1,1]
intercept <- r$center[2] - slope * r$center[1]

# first principal component on plot
abline(a = intercept, b = slope, lty = 3)
dev.off()

# A simple trading model based on buying the spread when it is below a certain threshold, and selling the spread when it is above a certain threshold

# Function to calculate the spread
calculate_spread <- function(x, y, beta){
    return(y - beta*x)
}

# Function to calculate the beta and leve given start and end dates
calculate_beta_and_level <- function(x, y, start_date, end_date) {
    #browser()
    time_range <- paste(start_date, "::", end_date, sep="")

    x <- x[time_range] # assumes we originally have a wider set of data...
    y <- y[time_range]

    dx <- diff(x[time_range]) #this seems redundant/ the subset does nothing since we just did it? but is in the book so leave for now
    dy <- diff(y[time_range])

    r <- prcomp(~ dx + dy)

    beta <- r$rotation[2,1] / r$rotation[1,1]
    spread <- calculate_spread(x, y, beta)
    names(spread) <- "spread"

    level = mean(spread, na.rm = TRUE)

    outL <- list()

    outL$spread <- spread
    outL$beta <- beta
    outL$level <- level

    return(outL)
}

# Function to calculate buy and sell signals with upper and lower threshold
calculate_buy_sell_signals <- function(spread, beta, level, lower_threshold, upper_threshold) {
    buy_signals <- ifelse(spread <= level - lower_threshold, 1, 0)
    sell_signals <- ifelse(spread >= level + upper_threshold, 1, 0)

    # bind these into a matrix
    output <- cbind(spread, buy_signals, sell_signals)
    colnames(output) <- c("spread", "buy_signals", "sell_signals")

    return(output)
}

# Example implementation

# Pick an in-sample date-range
start_date <- "2009-01-01"
## start_date <- "2011-01-01"
end_date <- "2011-12-31"

## start_date <- "2013-01-01"
## end_date <- "2014-12-31"

spy.adj <- SPY[,c("SPY.Adjusted")]
aapl.adj <- AAPL[,c("AAPL.Adjusted")]

## coke.adj <- coke[,c("COKE.Adjusted")]
## pepsi.adj <- pepsi[,c("PEP.Adjusted")]

results <- calculate_beta_and_level(spy.adj, aapl.adj, start_date, end_date)

pdf("AAPLvsSPYSpreads_InSample.pdf")
plot(results$spread, ylab = "Spread Value", main = "AAPL - beta*SPY",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
dev.off()

start_date_out_of_sample <- "2012-01-01"
end_date_out_of_sample <- "2012-10-22"

range <- paste(start_date_out_of_sample, "::", end_date_out_of_sample, sep = "")

spread_out_of_sample <- calculate_spread(spy.adj[range], aapl.adj[range], results$beta)

pdf("AAPLvsSPYSpreads_OutOfSample.pdf")
plot(spread_out_of_sample, ylab = "Spread Value", main = "AAPL - beta*SPY",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
dev.off()


# Rolling Beta example (for better tracking off wildly changing/ regime price spreads (later we'll see returns are better to use)
window_length <- 10

# Time range
start_date <- "2011-01-01"
end_date <- "2011-12-31"
range <- paste(start_date, "::", end_date, sep = "")

# our stock pair
x.spy <- SPY[range, 6]
y.aapl <- AAPL[range, 6]
df <- cbind(x.spy, y.aapl)
names(df) <- c("x.spy", "y.aapl")

# function to calculate betas
run_regression <- function(df) {
    res <- coef(lm(y.aapl ~ x.spy -1, data = as.data.frame(df)))
    return(res)
}

rolling_beta <- function(z, width) {
    #browser()
    res <- rollapply(z, width = width, FUN = run_regression, by.column = FALSE, align = "right")
    return(res)
}

#cat("In Sample rolling price beta \n")
betas <- rolling_beta(diff(df), 10)

data <- merge(betas, df)
data$priceSpread <- data$y.aapl - lag(betas, 1) * data$x.spy

# Add betas on return (a much more stationary series/ reasonable thing to do)
returns <- diff(df) / df
#cat(" In sample rolling return beta \n")
return_beta <- rolling_beta(returns, 10)
data$returnSpread <- diff(data$y.aapl) / data$y.aapl - return_beta * diff(data$x.spy) / data$x.spy

price.spread.threshold <- sd(data$priceSpread, na.rm=TRUE)
return.spread.threshold <- sd(data$returnSpread, na.rm=TRUE)


# plot the two spreads along with there sd's (from zero and from mean)

## pdf("rollingPriceSpreadsAndReturnSpreadsWithSD.pdf")
## par(mfrow = c(1,2))
# I can't get ablines to plot on the xts object, just going to use ggplot which I know a lot better
## plot(data$priceSpread, main = "AAPL vs. SPY Price Spread in sample",
##      cex.main = 0.8,
##      cex.lab = 0.8,
##      cex.axis = 0.8
##      )
## abline(h = price.spread.threshold, lty = 2)
## abline(h = -price.spread.threshold, lty = 2)
## abline(h = mean(data$priceSpread, na.rm=TRUE) + price.spread.threshold, lty = 2)
## abline(h = mean(data$priceSpread, na.rm=TRUE) - price.spread.threshold, lty = 2)

df.spreads <- as.data.frame(data)
df.spreads$date <- ymd(index(df))

price.spread.plot <- ggplot(df.spreads, aes(date, priceSpread)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "blue") +
    geom_hline(yintercept = mean(df.spreads$priceSpread, na.rm=TRUE), color = "red") +
    geom_hline(yintercept = price.spread.threshold, color = "blue", linetype="dashed") +
    geom_hline(yintercept = -price.spread.threshold, color = "blue", linetype="dashed") +
    geom_hline(yintercept = mean(df.spreads$priceSpread, na.rm=TRUE) + price.spread.threshold, color = "red", linetype="dashed") +
    geom_hline(yintercept = mean(df.spreads$priceSpread, na.rm=TRUE) - price.spread.threshold, color = "red", linetype="dashed")

return.spread.plot <- ggplot(df.spreads, aes(date, returnSpread)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "blue") +
    geom_hline(yintercept = mean(df.spreads$returnSpread, na.rm=TRUE), color = "red") +
    geom_hline(yintercept = return.spread.threshold, color = "blue", linetype="dashed") +
    geom_hline(yintercept = -return.spread.threshold, color = "blue", linetype="dashed") +
    geom_hline(yintercept = mean(df.spreads$returnSpread, na.rm=TRUE) + return.spread.threshold, color = "red", linetype="dashed") +
    geom_hline(yintercept = mean(df.spreads$returnSpread, na.rm=TRUE) - return.spread.threshold, color = "red", linetype="dashed")

pdf("rollingPriceSpreadsAndReturnSpreadsWithSD.pdf")
grid.arrange(price.spread.plot, return.spread.plot, ncol=1)


## plot(data$priceSpread, main = "AAPL vs. SPY Return Spread in sample",
##      cex.main = 0.8,
##      cex.lab = 0.8,
##      cex.axis = 0.8
##      )
## abline(h = return.spread.threshold, lty = 2)
## abline(h = -return.spread.threshold, lty = 2)
## abline(h = mean(data$returnSpread, na.rm=TRUE) + return.spread.threshold, lty = 2)
## abline(h = mean(data$returnSpread, na.rm=TRUE) - return.spread.threshold, lty = 2)
dev.off()

# Simple backtesting of trading strategy: buy at buy signal sell at sell signal, if all ready long or short don't increase the position

# Construct the out of sample spread
# same 10 day window
window_length <- 10

# Time range
start_date_oos <- "2012-01-01"
end_date_oos <- "2013-12-31"
range.oos <- paste(start_date_oos,"::", end_date_oos, sep = "")

# the stock pair
x.spy.oos <- SPY[range.oos, 6]
y.aapl.oos <- AAPL[range.oos, 6]

# make into a matrix
df.oos <- cbind(x.spy.oos, y.aapl.oos)
colnames(df.oos) <- c("x.spy", "y.aapl")

# calculate the oos rolling beta
#cat("OOS rolling price beta \n")

price.beta.oos <- rolling_beta(diff(df.oos), 10)

# calc returns for return strat
returns.oos <- diff(df.oos) / df.oos

#cat("OOS rolling return beta \n")
return.beta.oos <- rolling_beta(diff(returns.oos), 10)

# Buy and sell price spread threshold
data_out_price <- merge(price.beta.oos, df.oos)
data_out_price$spread <- data_out_price$y.aapl - lag(price.beta.oos, 1) * data_out_price$x.spy

# Buy and sell return spread threshold
data_out_return <- merge(return.beta.oos, returns.oos)
data_out_return$spread <- data_out_return$y.aapl - lag(return.beta.oos, 1) * data_out_return$x.spy

#turn into dataframes for ease of use with ggplot
price.oos.df <- as.data.frame(data_out_price)
price.oos.df$date <- ymd(index(data_out_price))

return.oos.df <- as.data.frame(data_out_return)
return.oos.df$date <- ymd(index(data_out_return))

price.spread.plot.oos <- ggplot(price.oos.df, aes(date, spread)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = mean(df.spreads$priceSpread, na.rm=TRUE), color = "red") + # in sample price spread mean
    geom_hline(yintercept = mean(price.oos.df$spread, na.rm=TRUE), color = "blue") + # oos price spread mean
    geom_hline(yintercept = price.spread.threshold, color = "red", linetype="dashed") + # in sample price 1sd from zero
    geom_hline(yintercept = -price.spread.threshold, color = "red", linetype="dashed") + # in sample price -1sd from zero
    geom_hline(yintercept = mean(df.spreads$priceSpread, na.rm=TRUE) + price.spread.threshold, color = "red", linetype="dotted") + # in sample mean + threshold
    geom_hline(yintercept = mean(df.spreads$priceSpread, na.rm=TRUE) - price.spread.threshold, color = "red", linetype="dotted") + # in sample mean - threshold
    ## geom_hline(yintercept = mean(price.oos.df$spread, na.rm=TRUE) + price.spread.threshold, color = "blue", linetype="dashed") + # in sample/ used mean + threshold INSPECT BUT DON"T ACTUALLY USE COZ OF LOOK AHEAD
    ## geom_hline(yintercept = mean(price.oos.df$spread, na.rm=TRUE) - price.spread.threshold, color = "blue", linetype="dashed") +
    ggtitle("OOS Price spread plot with 1sd in sample thresholds")


return.spread.plot.oos <- ggplot(return.oos.df, aes(date, spread)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = mean(df.spreads$returnSpread, na.rm=TRUE), color = "red") + # in sample mean return spread
    geom_hline(yintercept = mean(return.oos.df$spread, na.rm=TRUE), color = "blue") + # oos mean return spread for comparison (can't use flat out due to lookahead, can adjust/ update with new data and study if that's better)
    geom_hline(yintercept = return.spread.threshold, color = "blue", linetype="dashed") + # in sample 1sd return spread
    geom_hline(yintercept = -return.spread.threshold, color = "blue", linetype="dashed") + # in sample -1sd return spread
    geom_hline(yintercept = mean(df.spreads$returnSpread, na.rm=TRUE) + return.spread.threshold, color = "red", linetype="dashed") +
    geom_hline(yintercept = mean(df.spreads$returnSpread, na.rm=TRUE) - return.spread.threshold, color = "red", linetype="dashed") +
    ggtitle("OOS return spread with in sample mean and thresholds")




# plot the two spreads with the in sample computed thresholds
pdf("priceAndReturnSpreadStratOOS.pdf")
grid.arrange(price.spread.plot.oos, return.spread.plot.oos, ncol=1)
dev.off()

# Generate the price/return spread buy and sell signals
buys.price.spread <- ifelse(data_out_price$spread > price.spread.threshold, 1, 0)
sells.price.spread <- ifelse(data_out_price$spread < -price.spread.threshold, -1, 0)
data_out_price$signal <- buys.price.spread + sells.price.spread

buys.return.spread <- ifelse(data_out_return$spread > return.spread.threshold, 1, 0)
sells.return.spread <- ifelse(data_out_return$spread < -return.spread.threshold, -1, 0)
data_out_return$signal <- buys.return.spread + sells.return.spread

pdf("priceSpreadTradesOOS.pdf")
plot(data_out_price$spread, main = "AAPL vs SPY price spread trades",
     cex.main = 0.8,
     cex.label = 0.8,
     cex.axis = 0.8)

## abline(h = price.spread.threshold, lty=2) #these arent showing up...
## abline(h = -price.spread.threshold, lty=2)

#this is hack but should allow a horizontal line where thresholds are to be plotted on the xts plot
data_out_price$threshold <- price.spread.threshold
data_out_price$nthreshold <- -price.spread.threshold
lines(data_out_price[,"threshold"], col = "red")
lines(data_out_price[,"nthreshold"], col = "red")

point_type <- rep(NA, nrow(data_out_price))
buy_index <- which(data_out_price$signal==1)
sell_index <- which(data_out_price$signal==-1)

point_type[buy_index] <- 21
point_type[sell_index] <- 24
points(data_out_price$spread, pch=point_type)
dev.off()

pdf("returnsSpreadTradesOOS.pdf")
plot(data_out_return$spread, main = "AAPL vs SPY return spread trades",
     cex.main = 0.8,
     cex.label = 0.8,
     cex.axis = 0.8)

## abline(h = return.spread.threshold, lty=2) #these arent showing up...
## abline(h = -return.spread.threshold, lty=2)

#this is hack but should allow a horizontal line where thresholds are to be plotted on the xts plot
data_out_return$threshold <- return.spread.threshold
data_out_return$nthreshold <- -return.spread.threshold
lines(data_out_return[,"threshold"], col = "red")
lines(data_out_return[,"nthreshold"], col = "red")

point_type <- rep(NA, nrow(data_out_return))
buy_index <- which(data_out_return$signal==1)
sell_index <- which(data_out_return$signal==-1)

point_type[buy_index] <- 21
point_type[sell_index] <- 24
points(data_out_return$spread, pch=point_type)
dev.off()

num.price.spread.buy.signals <- sum(buys.price.spread, na.rm = TRUE)
num.price.spread.sell.signals <- sum(abs(sells.price.spread), na.rm = TRUE)

num.return.spread.buy.signals <- sum(buys.return.spread, na.rm = TRUE)
num.return.spread.sell.signals <- sum(abs(sells.return.spread), na.rm = TRUE)

# ToDo, make this into a proper function/ inputs/ helpers.
# can even parallelize if desired

# light trading/ backtesting/ sim logic: buy at quantities 100shares aapl and 100*beta shares SPY
# multiple buy or sell signals in a row will be treated as only one signal at the beginning of that period (Ie only hold long or short for one trade size, don't increase position with mutiple of the same signal)
# ie only trade/ hold one spread position at a time
# trade is made day after (open) recieving signal (note in real life there could certainly
# be sensitivity here, would check/ make more robust, this is just an example)


# These are state variables to alter as we process signals over time/days (simple sim)
prev.x.spy.qty <- 0
position.price <- 0
trade.size <- 100
signal.price <- as.numeric(data_out_price$signal)
signal.price[is.na(signal.price)] <- 0
beta.price <- as.numeric(data_out_price$price.beta.oos)

qty.x.spy <- rep(0, length(signal.price))
qty.y.aapl <- rep(0, length(signal.price))

for(i in 1:length(signal.price)) {

    ## if(index(data_out_price)[i] == "2012-02-09") {
    ##     browser()
    ## }

    cat(paste0(index(data_out_price)[i], "\n"))

    # case we're we have no position.price and recieve a buy signal
    if(signal.price[i] == 1 && position.price == 0) {

        # buy the spread
        prev.x.spy.qty <- round(beta.price[i] * trade.size)
        qty.x.spy[i] <- -prev.x.spy.qty
        qty.y.aapl[i] <- trade.size
        position.price <- 1 # we are now long the spread
    }

    # case we're we have no position and recieve a sell signal
    if(signal.price[i] == -1 && position.price == 0) {
        # sell the spread
        prev.x.spy.qty <- round(beta.price[i] * trade.size)
        qty.x.spy[i] <- prev.x.spy.qty
        qty.y.aapl[i] <- -trade.size
        position.price <- -1
    }

    # case we're short and get a buy signal
    if(signal.price[i] == 1 && position.price == -1) {
        #we are short the spread and need to buy
        qty.x.spy[i] <- -(round(beta.price[i] * trade.size) + prev.x.spy.qty)
        prev.x.spy.qty <- round(beta.price[i] * trade.size)
        qty.y.aapl[i] <- 2 * trade.size
        position.price <- 1
    }

    # case we're long and get a sell signal
    if(signal.price[i] == -1 && position.price == 1) {
        # we are long the spread and need to sell
        qty.x.spy[i] <- round(beta.price[i] * trade.size) + prev.x.spy.qty
        prev.x.spy.qty <- round(beta.price[i] * trade.size)
        qty.y.aapl[i] <- -2 * trade.size
        position.price <- -1
    }
}

# zero out any remaining position on the last day
qty.x.spy[length(qty.x.spy)] <- -sum(qty.x.spy)
qty.y.aapl[length(qty.y.aapl)] <- -sum(qty.y.aapl)

# append to our data/ trade table
data_out_price$qty.x.spy <- qty.x.spy
data_out_price$qty.y.aapl <- qty.y.aapl


# Same simulation but for the return spread based signal
# note that these state variables are for the most part resets of the ones above,
# use the ammended data_out frames with all the info there if/when needed
prev.x.spy.qty <- 0
position.return <- 0
trade.size <- 100
signal.return <- as.numeric(data_out_return$signal)
signal.return[is.na(signal.return)] <- 0
beta.return <- as.numeric(data_out_return$return.beta.oos)

qty.x.spy <- rep(0, length(signal.return))
qty.y.aapl <- rep(0, length(signal.return))

for(i in 1:length(signal.return)) {

    # case where we have no position.price and recieve a buy signal
    if(signal.return[i] == 1 && position.return == 0) {

        # buy the spread
        prev.x.spy.qty <- round(beta.return[i] * trade.size)
        qty.x.spy[i] <- -prev.x.spy.qty
        qty.y.aapl[i] <- trade.size
        position.return <- 1 # we are now long the spread
    }

    # case where we have no position and recieve a sell signal
    if(signal.return[i] == -1 && position.return == 0) {
        # sell the spread
        prev.x.spy.qty <- round(beta.return[i] * trade.size)
        qty.x.spy[i] <- prev.x.spy.qty
        qty.y.aapl[i] <- -trade.size
        position.return <- -1
    }

    # case where short and get a buy signal
    if(signal.return[i] == 1 && position.return == -1) {
        #we are short the spread and need to buy
        qty.x.spy[i] <- -(round(beta.return[i] * trade.size) + prev.x.spy.qty)
        prev.x.spy.qty <- round(beta.return[i] * trade.size)
        qty.y.aapl[i] <- 2 * trade.size
        position.return <- 1
    }

    # case we are long and get a sell signal
    if(signal.return[i] == -1 && position.return == 1) {
        # we are long the spread and need to sell
        qty.x.spy[i] <- round(beta.return[i] * trade.size) + prev.x.spy.qty
        prev.x.spy.qty <- round(beta.return[i] * trade.size)
        qty.y.aapl[i] <- -2 * trade.size
        position.return <- -1
    }
}

# zero out any remaining position on the last day
qty.x.spy[length(qty.x.spy)] <- -sum(qty.x.spy)
qty.y.aapl[length(qty.y.aapl)] <- -sum(qty.y.aapl)

# append to our data/ trade table
data_out_return$qty.x.spy <- qty.x.spy
data_out_return$qty.y.aapl <- qty.y.aapl

# Function for computing the equity curve
compute.equity.curve <- function(qty, price) {
    cash.buy <- ifelse(sign(qty) == 1, qty*price, 0)
    cash.sell <- ifelse(sign(qty)== -1, -qty * price, 0)
    position <- cumsum(qty)
    cumulative.buy <- cumsum(cash.buy)
    cumulative.sell <- cumsum(cash.sell)
    equity <- cumulative.sell - cumulative.buy + position * price
    return(equity)
}

data_out_price$equity.curve.x.spy <- compute.equity.curve(data_out_price$qty.x.spy,data_out_price$x.spy)
data_out_price$equity.curve.y.aapl <- compute.equity.curve(data_out_price$qty.y.aapl,data_out_price$y.aapl)

data_out_return$equity.curve.x.spy <- compute.equity.curve(data_out_return$qty.x.spy,data_out_return$x.spy)
data_out_return$equity.curve.y.aapl <- compute.equity.curve(data_out_return$qty.y.aapl,data_out_return$y.aapl)


pdf("priceAndReturnSignalEquityCurve.pdf")
par(mfcol = c(2,1))

plot(data_out_price$equity.curve.x.spy + data_out_price$equity.curve.x.spy,
     type="l",
     main = "AAPl / SPY price spread signal P&L",
     ylab = "P&L",
     cex.main = 0.8,
     cex.label = 0.8,
     cex.axis = 0.8
     )

plot(data_out_return$equity.curve.x.spy + data_out_return$equity.curve.x.spy,
     type="l",
     main = "AAPl / SPY return spread signal P&L",
     ylab = "P&L",
     cex.main = 0.8,
     cex.label = 0.8,
     cex.axis = 0.8
     )

dev.off()
