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
