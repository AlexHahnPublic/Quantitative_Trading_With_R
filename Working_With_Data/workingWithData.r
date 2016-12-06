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
out

# Look at the structure of the resulting object
str(out)

# Save the aapl_2 data frame as a .csv file
write.csv(aapl_2, file = "Coding/R/Quantitative_Trading_With_R/Working_With_Data/aapl_2_testsave.csv")

# Now let's try saving a an rdata object (binary/more native, should be more compressed)
save(aapl_2, file = "Coding/R/Quantitative_Trading_With_R/Working_With_Data/aapl_2_testsave.rdata")

# Let's retrieve the files back into memory and ensure that they are identical
aapl_original <- aapl_2
rm(aapl_2)
load(file = "Coding/R/Quantitative_Trading_With_R/Working_With_Data/aapl_2_testsave.rdata")

# The identical() command can be used to check whether objects are the same
identical(aapl_original, aapl_2)

# Excel work:
# -----------------
# Since excel and spreadsheets are ubiquitous in the finance industry R of
# Course has methodology to deal with them

# Load in the XLConnect library
library(XLConnect)

# Create the workbook object
book <- loadWorkbook("Coding/R/Quantitative_Trading_With_R/Working_With_Data/strategy.strategy.xlsx")


