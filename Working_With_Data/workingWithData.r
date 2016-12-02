#####################################################################
######################### workingWithData.r #########################
#####################################################################

# Purpose
# -------------------------------------------------------------------
#

# To quickly get excel data into R, copy then paste with the following command
aapl <- read.table(pipe("pbpaste"))
head(aapl)

# Notice that the import is a data.frame
class(aapl)

# Also notice that the data may have been read in in the reverse order desired
# to correct this use
aapl <- aapl[rev(rownames(aapl)), , drop = FALSE]

# To extract and visualize
prices <- aapl$V2
plot(prices, main = "AAPL plot", type = 'l')

# Instead of copying and pasting (which is useful in some cases) we can load in
# a .csv file
aapl_2 <- read.csv(file = "Coding/R/Quantitative_Trading_With_R/Working_With_Data/AAPL.csv", header = TRUE, stringsAsFactors = FALSE)

# Notice that we'd most likely want dates in chronological order (rev)
aapl_2 <- aapl_2[rev(rownames(aapl_2)) , ]

# Let's select a specific column to analyse, say closing price
aapl_close <- aapl_2[, "Close"]

# To obtain quick summary statistics
summary(aapl_close)

# Load in a test .json file

# Install and load the package
install.packages("RJSONIO")
library(RJSONIO)

# Read the file
out <- fromJSON(content = "Coding/R/Quantitative_Trading_With_R/Working_With_Data/test.json")
