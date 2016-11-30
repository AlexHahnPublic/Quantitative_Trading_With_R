###########################################################
########## pairwiseCorrelationBetween Tickers.r ###########
###########################################################

# Purpose
# ---------------------------------------------------------
# Compute and plot the correlations matrix of the returns of a given list of
# symbols

filter_symbols <- function(symbols) {
    # Name: filter_symbols
    # Purpose: Convert to upper case if not, remove any non valid symbols
    # Input: symbols = vector of stock tickers
    # Output: filtered_symbols = filtered symbols

    # Convert symbols to uppercase
    symbols <- toupper(symbols)

    # Validate the symbol names (must be between 2 and 4 chars, no numbers)
    valid <- regexpr("^[A-Z]{2,4}$", symbols)

    # Return only the valid ones
    return(sort(symbols[valid == 1]))
}

extract_prices <- function(filtered_symbols, file_path) {
  # Name: extract_prices
  # Purpose: Read price data from a specified file
  # Inputs: filtered_symbols = vector of symbols,
  #         file_path = location of price data
  # Output: prices = data.frame of prices per symbol

  # Read in the .csv price file
  all_prices <- read.csv(file = file_path, header=TRUE, stringsAsFactors = FALSE)

  # Remove the original Date column
  all_prices$Date <- NULL

  # Extract only the relevant data columns
  valid_columns <- colnames(all_prices) %in% filtered_symbols

  return(all_prices[, valid_columns])
}

filter_prices <- function(prices) {
  # Name: filter_prices
  # Purpose: Idetify rows with missing values
  # Inputs: Prices = data.frame of prices
  # Output: missing_rows = vector of indexes where data is missing out of one
  #         or more columns

  # Returns a boolean vector of dense or not fullt dense rows
  valid_rows <- complete.cases(prices)

  # Identify the index of the missing rows
  missing_rows <- which(valid_rows == FALSE)

  return(missing_rows)
}

compute_pairwise_correlations <- function(prices) {
  # Name: compute_pairwise_correlations
  # Purpose: Calculates pairwise correlations of returns and plots the pairwise
  #          relationshps
  # Inputs: prices = data.frame of prices
  # Output: correlation_matrix = A correlation matrix

  # Convert prices to returns
  returns <- apply(prices, 2, function(x) diff(log(x)))

  # Plot all pairwise relationships
  pairs(returns, main = "Pairwise return scatter plot")

  # Compute the pairwise correlations
  correlation_matrix <- cor(returns, use = "complete.obs")

  return(correlation_matrix)
}

#Execution
#--------------------------------------------------------------------
# Now we can leverage our helper functions to run a full analysis

# Stock tickers entered by user (note some intentionally off)
symbols <- c("IBM", "XOM", "2SG", "TEva", "GOog", "CVX", "AAPL", "BA")

# Location of our database of prices (csv file)
file_path = "~/Coding/R/Quantitative_Trading_With_R/Chapter2_ToolsOfTheTrade/Adj_close.csv"

# Filter and sort symbols
filtered_symbols <- filter_symbols(symbols)

# Extract prices
prices <- extract_prices(filtered_symbols, file_path)

# Filter prices
missing_rows <- filter_prices(prices)

# Compute correlations
correlation_matrix <- compute_pairwise_correlations(prices)
correlation_matrix
