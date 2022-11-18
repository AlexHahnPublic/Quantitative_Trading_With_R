#####################################################################
######################### workingWithData.r #########################
#####################################################################


library(lubridate) #for date type / conversion functions
# Purpose
# -------------------------------------------------------------------
#

# To quickly get excel data into R, copy then paste with the following command
## aapl <- read.table(pipe("pbpaste"))
## head(aapl)

# Notice that the import is a data.frame
#class(aapl)

# Also notice that the data may have been read in in the reverse order desired
# to correct this use
#aapl <- aapl[rev(rownames(aapl)), , drop = FALSE]

# To extract and visualize
#prices <- aapl$V2
#plot(prices, main = "AAPL plot", type = 'l')

# Instead of copying and pasting (which is useful in some cases) we can load in
# a .csv file, assumes running file from this directory
aapl_2 <- read.csv(file = "AAPL.csv", header = TRUE, stringsAsFactors = FALSE)

# Notice that we'd most likely want dates in chronological order (rev)
aapl_2 <- aapl_2[rev(rownames(aapl_2)) , ]

## # let's select a specific column to analyse, say closing price
aapl_close <- aapl_2[, "Close"]

## # To obtain quick summary statistics
#summary(aapl_close)

## # Load in a test .json file

## # Install and load the package
## install.packages("RJSONIO")
library(RJSONIO)

## # Read the file
out <- fromJSON(content = "test.json")
#out

## # look at the structure of the resulting object
## str(out)

## # Save the aapl_2 data frame as a .csv file
write.csv(aapl_2, file = "aapl_2_testsave.csv")

## # Now let's try saving a an rdata object (binary/more native, should be more compressed)
save(aapl_2, file = "aapl_2_testsave.rdata")

## # Let's retrieve the files back into memory and ensure that they are identical
aapl_original <- aapl_2
rm(aapl_2)
load(file = "aapl_2_testsave.rdata")

## # The identical() command can be used to check whether objects are the same
id <- identical(aapl_original, aapl_2)

## # excel work:
## # -----------------
## # Since excel and spreadsheets are ubiquitous in the finance industry R of
## # Course has methodology to deal with them

## # Load in the XLConnect library, there are some java dependencies here, going to comment out, I don't really use excel workbook loaded into r, just use csvs

# install.packages("XLConnect", dependencies=TRUE)
#library(XLConnect)

## # Create the workbook object
## book <- loadWorkbook("Coding/R/Quantitative_Trading_With_R/Working_With_Data/strategy.xlsx")

# for accessing a database:
## require(RODBC)

## #Establish a connection
## con <- odbcConnect("db")

## #choose db name and table
## database_name <- "OptionsData"
## table_name <- "ATMVolatilities"
## symbol <- "SPY"
## sql_command <- paste0("SELECT Symbol, Date, Maturity, Delta, CallPut, ImpliedVolatility FROM ",
##                       datebase_name, ".", table_name,
##                       " WHERE Maturiry = 91 AND Symbol IN ('", symbol, "');")

## iv_data <- sqlQuery(con, sql_command)

## #disconnect
## odbcClose(con)

#very similar process for a mysql instance
## require(RMySQL)
## con <- dbConnect(MySQL(), user="your_login",
##                  password="your_password",
##                  dbname="OptionsData",
##                  host="location_of_database")
## # list the tables and fields
## dbListTables(con)

## #define a mysql command and extract a dataframe
## sql_command <- paste0("SELECT Symbol, Date, Maturity, Delta, CallPut, ImpliedVolatility FROM ",
##                       database_name, ".", table_name,
##                       " WHERE Maturity=91 AND Symbol IN ('", symbol, "');")

## result <- dbGetQuery(con, sql_command)
## dbDisconnect(con)

# a lot of standard data proecessing tools are in
## library(dplyr)

# commands to familiarize with are: tbl(), group_by(), summarize(), do(), %>%

# xts deals with temporal data
library(xts)

# sample data
data(sample_matrix)

#for inspecting use head, calss, str, etc

# cast into an xts_matrix
xts_matrix <- as.xts(sample_matrix, descr="my new xts object")

# Simple plot
pdf(file="xtsOpenPlot.pdf")
plot(xts_matrix[,"Open"], main = "Open xts plot")
dev.off()

# Candle plot (quantmod does better, in fact i don't think candles is even recognized as a type anymore)
pdf(file="xtsCandlePlot.pdf")
plot.xts(xts_matrix, main="Candle plot of xts object", type="c")#type="candles")
dev.off()

library(quantmod)
# Better/ actual candles plto
pdf(file="quantmodCandlePlot.pdf")
chart_Series(xts_matrix, main="Candle plot of xts object")
dev.off()

#example of subsetting dates
pdf(file="subsetQuantmodCandlePlot.pdf")
chart_Series(xts_matrix["2007-01-01::2007-02-12"], main="Candle plot of xts object")
dev.off()

# a simple stock price plotting with additional lines example
set.seed(124)
price_vector <- sample(seq(95,105), 10)

# or for a more realistic non even distribution selection
price_vector_rnorm <- round(rnorm(10,100,5),2)

# non periodic datetimes
dates <- c("03/12/2013 08:00:01",
          "03/12/2013 08:00:02",
          "03/12/2013 08:00:05",
          "03/12/2013 08:00:10",
          "03/12/2013 08:00:11",
          "03/12/2013 08:00:12",
          "03/12/2013 08:00:19",
          "03/12/2013 08:00:25",
          "03/12/2013 08:00:26",
          "03/12/2013 08:00:12")

## dates <- c("03/12/2013 08:00:01.982333",
##           "03/12/2013 08:00:02.650321",
##           "03/12/2013 08:00:05.402321",
##           "03/12/2013 08:00:10.540432",
##           "03/12/2013 08:00:11.004554",
##           "03/12/2013 08:00:12.900213",
##           "03/12/2013 08:00:19.050323",
##           "03/12/2013 08:00:25.430345",
##           "03/12/2013 08:00:26.700123",
##           "03/12/2013 08:00:12.839538")

# Enable R console to display the microsecond field
#options(digits.secs=6)

# create time index with correct format
time_index <- strptime(dates, format="%d/%m/%Y %H:%M:%S")

xts_price_vector <- xts(price_vector_rnorm, time_index)

names(xts_price_vector) <- c("price")

pdf(file="priceChartWithLines.pdf")
# initial plot
plot(xts_price_vector, main = "Fictitious price series", cex.main=0.8)
#add horizontal line at mean of prices
abline(h=mean(xts_price_vector), lwd=2)
#add a vertical line at a specified time
my_time <- as.POSIXct("03/12/2013 8:00:17.872568", format="%d/%m/%Y %H:%M:%OS")
abline(v=my_time, lwd=2, lty=2)
dev.off()

time_diff <- difftime(index(xts_price_vector)[2], index(xts_price_vector)[1], units="secs")

# subobtimally loop and store
diffs <- c()
for(i in 2:length(index(xts_price_vector))) {
    diffs[i] <- difftime(index(xts_price_vector)[i], index(xts_price_vector)[i-1], units="secs")
}

# more optimally offset and take difference of the two vectors
diffs_vectorized <- index(xts_price_vector)[-1] - index(xts_price_vector)[-length(index(xts_price_vector))]



# graphical representation of time differences
pdf(file="tradeTimeDiffs.pdf")
par(mfrow=c(2,1))
diffs <- as.numeric(diffs_vectorized)
plot(diffs,main="Time difference in seconds for trades",
     xlab="", ylab="Time differences",
     cex.lab=0.8,
     cex.main=0.8)
grid()
hist(diffs, main = "Time difference in seconds for trades",
     xlab="Time difference (secs)", ylab="Observations",
     breaks=20,
     cex.lab=0.8,
     cex.main=0.8)
grid()
dev.off()

# quantmod example (note, this has changed decently since the R book was written, everything is pretty googleable these days tho
stock_list <- c("AAPL")#, "GOOG") # add more symbols here
start_date <- Sys.Date()-365
end_date <- Sys.Date()
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
    AAPL <- rbind(master_df, temp_df)

    AAPL$Date <- ymd(AAPL$Date)
    AAPL <- xts(AAPL[,-1], order.by=AAPL[,1]) #converting to xts, note there is some more complexity here to actually use
}

# plotting with a technical indicators support (requires TTR which is a dependancy of quantmod)

pdf(file="technicalIndicatorsPlot_BolingerBands_DEMA.pdf")
chartSeries(xts_matrix, subset="20011::2012",
             theme=chartTheme("white"),
            TA="addBBands(); addDEMA()")
dev.off()

# New indicator example
my_indicator <- function (x) {
    return (x * 1.1)
}

add_my_indicator <- newTA(my_indicator, legend.name="My Custom Indicator", on = 1)

pdf(file="customTestIndicator.pdf")
chartSeries(xts_matrix, subset="20011::2012",
             theme=chartTheme("white"),
            TA="addBBands(); addDEMA()")

add_my_indicator()

dev.off()


# While xts is nice and convenient for a lot of its built in functionality, working with dataframes and manipulating them,
# and ggplotting those objects is potentially even more standard of a practice/ easier to iterate in a fast manner

df <- as.data.frame(AAPL[,c("Adjusted", "Volume")], stringsAsFactors=FALSE)

names(df) <- c("Price", "Volume")

df$Price <- as.numeric(df$Price)
df$Volume <- as.numeric(df$Volume)


df$return <- c(0,diff(log(df[,1])))
#df$cuts <- cut(abs(df$return), breaks=c(0, 0.02, 0.04, 0.25), include.lowest=TRUE) #this doesn't actually return the int of the group like the book says
breaks=c(0, 0.02, 0.04, 0.25)
df$cuts <- findInterval(df$return, breaks)
df$means <- NA

for(i in unique(df$cuts)) {
    group <- which(df$cuts == i)
    if(length(group) > 0) {
        df$means[group] <- mean(df$Volume[group], na.rm=TRUE)
    }
}

# ^ I'm pretty certain this is just a df %>% group_by(cuts) %>% summarize(mean=mean(volume, na.rm=TRUE)) but just following the book here

library(ggplot2)
pdf(file="ggplotHistogramExample.pdf")
ggplot(df) + geom_histogram(aes(x=Volume)) + facet_grid(cuts ~.) + geom_vline(aes(xintercept=means), linetype="dashed", size=1)
dev.off()
