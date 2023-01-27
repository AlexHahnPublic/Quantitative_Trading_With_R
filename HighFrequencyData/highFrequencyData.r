#####################################################################
########### High Frequency Data Handling and Evaluation #############
#####################################################################

library(xts)

# TODO:
# recreate this with actual real world data (only from public),
# or from TAQ data (don't check in data file at all),
# or spoof better with autoregressive functions/ higher moments (could be a good excercise)

# We don't have the 1GB example csv from Tick Data, Inc. I'll mock up
# the data as needed/ as best I can

# spy_file <- "path/Stocks/QUOTES/SPY_2013_04_15_X_Q.asc"
# spy_quotes <- read.csv(file = spy_file, header = FALSE, stringsAsFactors = FALSE_

# note: book says 7.2 million rows
## spy_quotes <- data.frame(matrix(ncol = 21, nrow = 7000000))

## colnames(spy_quotes) <- c("date", "time", "exchange", "bid_price", "ask_price",
##                           "bid_size", "ask_size", "quote_condition", "mode",
##                           "market_maker_id", "sequence_number", "bid_exchange",
##                           "ask_exchange", "national_bbo_indicator",
##                           "nasdaq_bbo_indicator", "quote_cancel_correction",
##                           "quote_source", "short_sale_restriction_indicator",
##                           "limit_up_down_bbo_indicator_cqs",
##                           "limit_up_down_bbo_indicator_utp",
##                           "finra_adf_mpid_indicator"
##                           )

# =========================================================================================
# data creation, note the data here does not have the full structure as real SPY quote data
# as a result the charts will look different. Uncomment to regen when sourcing/ loading
# =========================================================================================

# need to create some valid time stamps in order to do the xts transformation
## date <- "04/15/2013"

## hours <- c("04","05","06","07","08","09","10","11","12","13","14","15","16","17","18")
## minutes <- c("00", "01","02","03","04","05","06","07","08","09",as.character(10:59))
## seconds <- c("00","01","02","03","04","05","06","07","08","09",as.character(10:59))
## milliseconds <- c("01","02","03","04","05","06","07","08","09",as.character(10:99))

# uncomment to recreate when running/ sourcing r file, no need to create each time,
# create vectors and cat together
## d <- rep(date, 7000000)
## h <- sample(hours, 7000000, replace = TRUE)
## m <- sample(minutes, 7000000, replace = TRUE)
## s <- sample(seconds, 7000000, replace = TRUE)
## ms <- sample(milliseconds, 7000000, replace = TRUE)

# non date part, just time
## ts <- sort(paste(paste(h,m,s, sep=":"), ms, sep ="."))


## spy_quotes$time <- ts
## spy_quotes$date <- d

# bid and ask simulated date
# looking at the book the bid price has on average a small negative return of ~ with a variance of ~
# create the bids price vectors

## starting.bid <- 158.00

## # fun facts... no higher moments or serieal autocorrelation would be nice
## bid.returns <- rnorm(n = 6999999, mean = -0.00000001, sd = 0.000001)

## bid.prices <- exp(cumsum(bid.returns)) * starting.bid

## spy_quotes$bid_price <- c(starting.bid, bid.prices)

## # as to be realistic/ not cross the market, the ask price will be the bid price + a sample between 0 and 10 cents
## spreads <- seq(0.01,0.10, by = .0001)

## spread <- sample(spreads, 7000000, replace = TRUE)

## spy_quotes$ask_price <- spy_quotes$bid_price + spread

# the book looks like both bid and ask size means of ~60 with sd of 20
## bid.sizes <- round(rnorm(7000000,60,20))
## bid.sizes[bid.sizes <= 0 ] <- 1

## ask.sizes <- round(rnorm(7000000,60,20))
## ask.sizes[ask.sizes <= 0 ] <- 1

## spy_quotes$bid_size <- bid.sizes
## spy_quotes$ask_size <- ask.sizes

# gotta put in some P's in the exchange column here so the next line doesn't filter everything
# just picking a few out here
# M = Chicago Stock Exchange, N = NYSE, P = NYSE Arca, Q = NASDAQ OMX, J = Direct Edge A, Z = BATS

## exchange_codes <- c("M", "N", "P", "Q", "J", "Z")

## spy_quotes$exchange <-  sample(exchange_codes, 7000000, replace=TRUE)

## # SPY's primary exchange is NYSE Arca, filter to only look at those
## spy_quotes_arca <- spy_quotes[spy_quotes$exchange %in% c("P"),
##                               c("date", "time", "bid_price", "ask_price", "bid_size", "ask_size")]

# note, this book was written a wile ago and i'd probally just use a filter in dplyr:
## library(dplyr)
# spy_quotes_arca <- spyt_quote %>% filter(exchange == "P")
# spy_quotes_arca <- spy_quotes_arca[,c("date", "time", "bid_price", "ask_price", "bid_size", "ask_size")]

# transform into and xts object

## time_index <- as.POSIXct(paste(spy_quotes_arca$date, spy_quotes_arca$time), format = "%m/%d/%Y %H:%M:%OS")
## spy <- xts(spy_quotes_arca[, -c(1,2)], time_index)

## rm(time_index)


pdf("spyArcaBidPriceFake.pdf")
plot(spy$bid_price, type = "l",
     main = "SPY bid price (fake)",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

dev.off()

# I don't have the negatives so this won't filter anything out
spy_filtered <- spy[spy$bid_size >0 ,]

rows_removed <- nrow(spy) - nrow(spy_filtered)

# inspect columns

## summary(as.numeric(spy_filtered$ask_price))
## summary(as.numeric(spy_filtered$bid_price))
## summary(as.numeric(spy_filtered$ask_size))
## summary(as.numeric(spy_filtered$bid_size))

# check if there are any crossed bids/asks (I believe I generated the data so that there can't be)
crossed_prices <- spy_filtered$bid_price >= spy_filtered$ask_price

ans <- any(crossed_prices)

# note I also do sum on a bool condition often times, that would also give count, zero is none, ie sum(v[is.na(v)])

# look at successive time differences between quote updates. A quote update is any change to the top of the book

# Extract time index
quote_times <- index(spy_filtered)

# Compute the consecutive time differences
time_differences <- as.numeric(diff(quote_times))

summary_time <- summary(time_differences)

# identify gaps in arrivals/quote times (I don't think I'll have any by construction)
long_times <- which(time_differences > 1000)

# show the data around the abnormally long time (which I don't have)
#spy_abnormal <- spy_filtered[(long_times -2):(long_times + 2), ]

# Calculate the bid-ask spread
bid_ask_spread <- spy_filtered$ask_price - spy_filtered$bid_price

#high spreads
high_spreads <- which(bid_ask_spread > 0.25)

# if you want filter them out
## if(length(high_spreads) > 0) {
##     bid_ask_spread <- bid_ask_spread[-high_spreads]
## }

# plot the spread (this will certainly not look like the book given how I generated the data
pdf("bidAskSpreadFake.pdf")
plot(bid_ask_spread, type = "l",
     main = "Bid ask spread (mock data)",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
dev.off()

# the book/ real life claims three regimes from the above plot (once again my mock data won't show)

# create three time partitions for the SPY data
early_morning <- "2013-04-15 04:00:00::2013-04-15 08:29:00"
regular_trading <- "2013-04-15 08:30:00::2013-04-15 16:15:00"
after_hours <- "2013-04-15 16:15:01::2013-04-15 20:00:00"

# create the histogram for each period on one plot
pdf("spreadRegimePeriods.pdf")
par(mfrow = c(3,1))

# Morning
data <- bid_ask_spread[early_morning]
hist(data, main = early_morning, breaks = 1000, # note the title is the time string here
     xlim = c(0, 0.1))
abline(v = mean(data), lwd = 2, lty = 2)

# Afternoon
data <- bid_ask_spread[regular_trading]
hist(data, main = regular_trading, breaks = 1000, # note the title is the time string here
     xlim = c(0, 0.1))
abline(v = mean(data), lwd = 2, lty = 2)

# Evening
data <- bid_ask_spread[after_hours]
hist(data, main = after_hours, breaks = 1000, # note the title is the time string here
     xlim = c(0, 0.1))
abline(v = mean(data), lwd = 2, lty = 2)

dev.off()

# the book points out that the trading day period is the most liquid with the spread almost always one tick
spy_day <- spy_filtered[regular_trading]

# calculate the "microprice" ie the weighted average of the bid ask mid price
spy_micro_price <- (spy_day$bid_price * spy_day$bid_size + spy_day$ask_price*spy_day$ask_size) /
  (spy_day$bid_size + spy_day$ask_size)

# plot the micro price alongside the bid and ask prices

par(mfrow = c(1,1))
range <- 10000:10100
title <- "Micro-price between bid-ask prices"
plot(spy_day_ask_price[range],
     ylim =c(min(spy_day$bid_price[range]),
             max(spy_day$ask_price[range])),
     main = title,
     cex.main = 0.8,
     cex.axis = 0.8,
     cex.lab = 0.8)
lines(spy_day$bid_price[range])
lines(spy_micro_price[range],lty=2)


spy_returns <- diff(log(spy_micro_price))

# extreme leptokurtocity of returns
par(mfrow = c(2,1))
plot(spy_returns,
     main = "Times series plot of micro-price returns",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
hist(spy_returns, breaks = 1000,
     main = "Micro-price distribution",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# superimpose a normal distribution
par(mfrow = c(1,1))
mean_spy <- mean(as.numeric(spy_returns), na.rm = TRUE)
sd_spy <- sd(as.numeric(spy_returns), na.rm = TRUE)

hist(spy_returns, breaks = 10000, prob = TRUE,
     xlim = c(-0.00003, 0.00003),
     main = "Micro-price distribution vs Normal",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

curve(dnorm(x, mean_spy, sd_spy), add = TRUE,
      yaxt = "n", lwd = 3, lty = 3)

# check autocorrelation of high frequency returns
spy_acf <- acf(as.numeric(spy_returns),
               na.action = na.pass,
               main = "Autocorrelation",
               cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# Let's take a look at the trades data now

# TODO: mock up this data, either add noise to TAQ, or for some good statistical exercise create similar distributions. series.
spy_trades <- read.csv("path", header=FALSE, stringsAsFactors=FALSE)

names(spy_trades) <- c("date", "time", "price", "volume", "exchange", "sales_condition",
                       "correction_indicator", "sequence_number",
                       "trade_stop_indicator", "source_of_trade", "trf",
                       "exclude_record_flag", "filtered_price")

# Extract only the ARCA trades
spy_trades_arca <- spy_trades[spy_trades$exchange %in% c("P"), c("date", "time", "price", "volume", "correction_indicator",
                                                                 "filtered_price")]

# check if any filtered prices exist
any(!is.na(spy_trades_arca$filtered_price))

# check iof there are any special correction indicators present
unique(spy_trades_arca$correction_indicator)

# drop the last two columns from the dataframe
spy_trades_arca <- spy_trades_arca[, 1:4]

# convert to a xts object for subsequent analysis
time_index <- as.POSIXct(paste(spy_trades_arca$date,
                               spy_trades_arca$time), format = "%m/%d/%Y %H:%M:%OS")

spy_t <- xts(spy_trades_arca[,-c(1,2)], time_index)
rm(time_index)

# head(spy_t)

# subset to regular trading hours
regular_trading <- "2013-04-15 08:30:00::2013-04-15 16:15:00"
spy_t_day <- spy_t[regular_trading]

#dim(spy_t_day)
#object.size(spy_t_day)

# Compute returns
spy_t_day_returns <- diff(log(spy_t_day$price))[-1]

# plot the distribution and the autocorrelation plot
par(mfrow = c(2,1))
plot(spy_t_day_returns, main = "SPY returns on trades",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
acf(as.numeric(spy_t_day_returns), main = "SPY trades acf",
    cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# impose a normal distribution
par(mfrow = c(1,1))
hist(spy_t_day_returns, breaks = 1000, prob = TRUE,
     xlim = c(-0.0001, 0.0001),
     main = "Distribution of SPY trade returns",
     cex.lab = 0.8, cex.axis = 0.8)

curve(dnorm(x,
            mean(spy_t_day_returns),
            sd(spy_t,day_returns)),
      add = TRUE,
      yaxt = "n",
      lwd = 3,
      lty = 3)

# Use the rle() function to find price sequences
prices_rle <- rle(as.numeric(spy_t_day$price))

# row indexed to keep
end_indexes <- cumsum(prices_rle$lengths)

# start indexes to sum the volumes from
start_indexes <- end_indexes - prices_rle$lengths + 1

# create a vector of total volumes for each p[rice
volume_list <- list()
volume_vector <- as.numeric(spy_t_day$volume)
for (i in 1:length(end_indexes)) {
  volume_list[[i]] <- sum(volume_vector[start_indexes[i]:
                                        end_indexes[i]], na.rm = TRUE)
}

# create a reduced data set with distinct trade prices
spy_t_day_reduced <- spy_t_day[end_indexes,]
spy_t_day_reduced$volume <- unlist(volume_list)

#head(spy_t_day_reduced, 10)
#head(spy_t_day, 10)

# nonlinear relationships between successive lags in return space?

# Random cloud with lag 1
n <- rnorm(50, 0, .20)
n_lag_1 <- c(NA, n[-length(n)])
plot(n_lag1, n)

# create arrows between the points
s <- seq(length(n)-1)
arrows(n_lag1[s], n[s], n_lag1[s+1], n[s+1])

# may reveal point sequences that spend more time traveling around the
# edges of the cloud before diving back in toward the center

# SPY return cloud with lag 1
spy_t_returns <- diff(log(as.numeric(
  spy_t_day_reduced$price[100:150])))
spy_t_returns_lag1 <- c(NA, spy_t_returns[-length(spy_t_returns)])
plot(spy_t_returns_lag1, spy_t_returns)

s <- seq(length(spy_t_returns) - 1)
arrows(spy_t_Returns_lag1[s], spy_t_returns[s],
       spy_t_returns_lag1[s+1], spy_t_returns[s+1])
