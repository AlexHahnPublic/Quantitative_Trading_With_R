#####################################################################
############################# Options ###############################
#####################################################################

# Note: to run these examples you need to have a working installation of
# QuantLib (and I think also you need to have Rccp/ the C++ compiler installed)
# https://www.quantlib.org/install.shtml

library(dplyr)
library(RQuantLib)

call_value <- EuropeanOption(type = "call",
                             underlying = 100,
                             strike = 100,
                             dividendYield = 0,
                             riskFreeRate = 0.03,
                             maturity = 1.0,
                             volatility = 0.30)
# class(call_value)

# Explore sensitivity of option and greeks to changing underlying price (s)
# paramaters to call below function which stores output values
type <- "call"
underlying <- 20:180 # note we can loop over different input variables if desired
strike <- 100
dividendYield <- 0
riskFreeRate <- 0.03
maturity <- 1.0
volatility <- 0.10

# Function to create plots of option values and Greeks
option_values <- function(type,
                          underlying,
                          strike,
                          dividendYield,
                          riskFreeRate,
                          maturity,
                          volatility) {
  # Output list with option values as Greeks
  out <- list()
  for(i in seq_along(underlying)) {
    out[[i]] <- EuropeanOption(type = type,
                               underlying = i,
                               strike = strike,
                               dividendYield = dividendYield,
                               riskFreeRate = riskFreeRate,
                               maturity = maturity,
                               volatility = volatility)
  }

  # Set up the plot window
  par(mfrow = c(3, 2))
  names <- c("Value", "Delta", "Gamma", "Vega", "Theta", "Rho")

  for(i in 1:6) {
    plot(unlist(lapply(out, "[", i)),
         type = "l",
         main = paste(names[i], "vs. Underlying"),
         xlab = "Underlying",
         ylab = names[i])
    grid()
    abline(v = strike, col = "red")
  }
  return(out)
}

## pdf("greeksVsUnderlyingSensitivityAnalysis.pdf")
## option_values(type, underlying, strike, dividendYield, riskFreeRate, maturity, volatility) # this is going to generate a lot of print output to an R session, comment out as needed/ save object
## dev.off()

# Todo: write the above with a dataframe, dplyr, melt, and a facet wrap over 2 dimensions of input variables
# Note: this sort of looping over a parameter changing values is a common method for analysing portfolios/ other
# complex functions of multiple inputs

# Analysis of SPY option trades that happened on April 15, 2015 sourced from the world wide web
spy.trades <- read.csv("~/spyOptionTrades_20130415.csv")

# at this point the book goes through some data format dependant processing, my data is not in the same format
# so I'll just use dplyr to get the same processed "result" data as the book

# Goal: "visualize the volume of contracts traded across strikes and across maturities (and by type, call or put)
results <- spy.trades %>%
  group_by(type, strike, expiration) %>%
  summarize(trades = n(),
            volume = sum(sale_size, na.rm = TRUE),
            avg_price = mean(sale_price, na.rm=TRUE), # note could ise weighted.mean here with volume as the weights
            sd_price = sd(sale_price, na.rm=TRUE),
            avg_stock_price = (mean(underlying_bid_price, na.rm = TRUE) + mean(underlying_ask_price, na.rm = TRUE)) / 2,
            stock_range = max(underlying_ask_price, na.rm = TRUE) - min(underlying_ask_price, na.rm = TRUE)
            ) %>% as.data.frame(stringsAsFactor=FALSE)

unique_maturities <- unique(results$expiration)
n
today <- as.Date("2013-04-15") #note, I usually use ymd() lubridate, just following book here

days_to_expiration <- as.Date(unique_maturities[1]) - today

# Extract only the relevant maturity range
single_maturity_table <- results[results$expiration == unique_maturities[1],]

# Look at the calls and puts separately
calls <- single_maturity_table[
  single_maturity_table$type == "C", ]
puts <- single_maturity_table[
  single_maturity_table$type == "P", ]

#note calls <- single_maturity_table %>% filter(type == "C") is the dplr equivalent

# would also now plot with ggplot and a facet_wrap/grid but following the book:
pdf("volumesVsStrikeSingleMaturity.pdf")
par(mfrow = c(2,1))
plot(calls$strike, calls$volume,
     xlab = "Strike", ylab = "Volume",
     main = "Call volume", cex.main = 0.9)
abline(v = mean(calls$avg_stock_price), lty = 2)
grid()
plot(puts$strike, puts$volume,
     xlab = "Strike", ylab = "Volume",
     main = "Call volume", cex.main = 0.9)
abline(v = mean(puts$avg_stock_price), lty = 2)
grid()
dev.off()
