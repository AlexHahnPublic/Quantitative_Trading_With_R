#####################################################################
############################# Options ###############################
#####################################################################

# Note: to run these examples you need to have a working installation of
# QuantLib (and I think also you need to have Rccp/ the C++ compiler installed)
# https://www.quantlib.org/install.shtml

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

pdf("greeksVsUnderlyingSensitivityAnalysis.pdf")
option_values(type, underlying, strike, dividendYield, riskFreeRate, maturity, volatility)
dev.off()

# Todo: write the above with a dataframe, dplyr, melt, and a facet wrap over 2 dimensions of input variables
# Note: this sort of looping over a parameter changing values is a common method for analysing portfolios/ other
# complex functions of multiple inputs
