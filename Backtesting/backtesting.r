#####################################################################
################ Backtesting with Quantsrat Framework ###############
#####################################################################

require(quantstrat)
require(PerformanceAnalytics)

# Setting up quantstrat

# surpress  warnings
options("getSymbols.warnings4.0"=FALSE)

# clean up environment
#rm(list=ls(.blotter), envir = .blotter)

# set the currency and timezone
currency("USD")
Sys.setenv(TZ="UTC")

# Symbols of interest
symbols <- c("XLB",
             "XLF",
             "XLP",
             "XLI",
             "XLU",
             "XLV",
             "XLK",
             "XLY",
             "RWR",
             "EWJ",
             "EWG",
             "EWU",
             "EWC",
             "EWY",
             "EWA",
             "EWH",
             "EWS",
             "IYZ",
             "EZU",
             "IYR",
             "EWT",
             "EWZ",
             "EFA",
             "IGE",
             "EPP",
             "LQD",
             "SHY",
             "IEF",
             "TLT"
             )

from = "2002-01-01"
to = "2015-12-31"

# SPDR ETFs first, iShares ETFs afterwards
if(!"XLB" %in% ls()) {
    #if data is not present, get it from yahoo
    suppressMessages(getSymbols(symbols,
                               from = from, #"2010-01-01",
                               to = to, #"2011-01-01",
                               src = "yahoo",
                               adjust = TRUE))
}

# Define the instrument type
stock(symbols, currency = "USD", multiplier = 1)

# Compute ATR
"lagATR" <- function(HLC, n = 14, maType, lag = 1, ...) {
    ATR <- ATR(HLC, n = n, maType = maType, ...)
    ATR <- lag(ATR, lag)
    out <- ATR$atr
    colnames(out) <- "atr"
    return(out)
}

# trade sizing given ATR
"osDollarATR" <- function(orderside, tradeSize, pctATR,
                          maxPctATR = pctATR, data, timestamp,
                          symbol, prefer = "Open", portfolio, integerQty = TRUE,
                          atrMod = "", rebal = FALSE, ...) {

    if(tradeSize > 0 & orderside == "short") {
        tradeSize <- tradeSize * -1
    }

    pos <- getPosQty(portfolio, symbol, timestamp)
    atrString <- paste0("atr", atrMod)
    atrCol <- grep(atrString, colnames(mktdata))

    if(length(atrCol) == 0) {
        stop(paste("Term", atrString, " not found in mktdata column names."))
    }

    atrTimeStamp <- mktdata[timestamp, atrCol]
    if(is.na(atrTimeStamp) | atrTimeStamp == 0) {
        stop(paste("ATR corresponding to ", atrString, " is invalid at this
 point in time. Add a logical operator to account for this."))
    }

    dollarATR <- pos * atrTimeStamp
    desiredDollarATR <- pctATR * tradeSize
    remainingRiskCapacity <- tradeSize * maxPctATR - dollarATR

    if(orderside == "long") {
        qty <- min(tradeSize * pctATR / atrTimeStamp, remainingRiskCapacity / atrTimeStamp)
    } else {
        qty <- max(tradeSize * pctATR / atrTimeStamp, remainingRiskCapacity / atrTimeStamp)
    }

    if(integerQty) {
        qty <- trunc(qty)
    }

    if(!rebal) {
        if(orderside == "long" & qty < 0) {
            qty <- 0
        }
        if(orderside == "short" & qty > 0) {
            qty <- 0
        }
    }

    if(rebal) { # thinkthis can just be an else given we have if not rebal above, rebal is either TRUE or FALSE...
        if(pos == 0) {
            qty <- 0
        }
    }

    return(qty)
}

# Strategy:


initDate = "1990-01-01"
from = "2003-01-01"
to = "2013-12-31"
options(width = 70)

# Trade sizing and initial equity setting
tradeSize <- 10000
initEq <- tradeSize * length(symbols)

strategy.st <- "Clenow_Simple"
portfolio.st <- "Clenow_Simple"
account.st <- "Clenow_Simple"
rm.strat(portfolio.st)
rm.strat(strategy.st)

initPortf(portfolio.st, symbols = symbols,
          initDate = initDate, currency = "USD")

initAcct(account.st, portfolios = portfolio.st,
         initDate = initDate, currency = "USD", initEq = initEq)

initOrders(portfolio.st, initDate = initDate)

strategy(strategy.st, store=TRUE)


# Backtesting

# Adding Indicators
nLag = 252
pctATR = 0.02
period = 10

namedLag <- function(x, k = 1, na.pad = TRUE, ...) {
    out <- lag(x, k = k, na.pad = na.pad, ...)
    out[is.na(out)] <- x[is.na(out)]
    colnames(out) <- "namedLag"
    return(out)
}


add.indicator(strategy.st, name = "namedLag",
              arguments = list(x = quote(Cl(mktdata)), k = nLag),
              label = "ind")

add.indicator(strategy.st, name = "lagATR",
              arguments = list(HLC = quote(HLC(mktdata)), n = period),
              label = "atrX")

test <- applyIndicators(strategy.st, mktdata = OHLC(XLB))

# Adding Signals
add.signal(strategy.st, name = "sigCrossover",
           ## arguments = list(columns = c("XLB.Close", "namedLag.ind"),
           arguments = list(columns = c("Close", "namedLag.ind"),
                            relationship = "gt"),
           label = "coverOrBuy")

add.signal(strategy.st, name = "sigCrossover",
           ## arguments = list(columns = c("Close", "namedLag.ind"),
           arguments = list(columns = c("Close", "namedLag.ind"),
                            relationship = "lt"),
           label = "sellOrShort")

# Adding Rules

# long rules
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "coverOrBuy",
                           sigval = TRUE,
                           ordertype = "market",
                           orderside = "long", replace = FALSE,
                           prefer = "Open", osFUN = osDollarATR,
                           tradeSize = tradeSize, pctATR = pctATR,
                           atrMod = "X"),
         type = "enter", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "sellOrShort",
                          sigval = TRUE, orderqty = "all",
                          ordertype = "market", orderside = "long",
                          replace = FALSE, prefer = "Open"),
         type = "exit", path.dep = TRUE)

# short rules
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "sellOrShort",
                          sigval = TRUE, ordertype = "market",
                          orderside = "short", replace = FALSE,
                          prefer = "Open", osFUN = osDollarATR,
                          tradeSize = -tradeSize, pctATR = pctATR,
                          atrMod = "X"),
         type = "enter", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "coverOrBuy",
                          sigval = TRUE, orderqty = "all",
                          ordertype = "market", orderside = "short",
                          replace = FALSE, prefer = "Open"),
         type = "exit", path.dep = TRUE)




# Run the strategy:

# get begin time
t1 <- Sys.time()

# only need to run this and generate tStats once, then don't rerun the sim everytime, just examine tStats
# and rerun if new indicator/ signal / rule is added or one is changed
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# record end time
t2 <- Sys.time()
#print(t2 - t1)

updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st, dateRange)
updateEndEq(account.st)

 tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE)
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)

#print(data.frame(t(tStats[,-c(1,2)])))

aggPF <- sum(tStats$Gross.Profits)/ -sum(tStats$Gross.Losses)

aggCorrect <- mean(tStats$Percent.Positive)

numTrades <- sum(tStats$Num.Trades)

meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[
                              tStats$Avg.Win.Trade < Inf], na.rm = TRUE)

# return analystics

instRets <- PortfReturns(account.st)

portfRets <- xts(rowMeans(instRets) * ncol(instRets),
                 order.by = index(instRets))

portfRets <- portfRets[!is.na(portfRets)]

cumPortfRets <- cumprod( 1 + portfRets)

firstNonZeroDay <- as.character(index(portfRets)[
    min(which(portfRets != 0))])

#Obtain symbol
getSymbols("SPY", from = firstNonZeroDay, to = to)
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1 + SPYrets)
comparison <- merge(cumPortfRets, cumSPYrets, all=FALSE) #note, this differs from the book, I pull wider dates in getSymbol usually than needed, so can't just assume they line up and cbind, merge bate date index instead
colnames(comparison) <- c("strategy", "SPY")

pdf("averageTrueRiskTrendFollowerIndicatorSignalRulesBacktestVSSPY.pdf")
chart.TimeSeries(comparison, legend.loc = "topleft")
##                  ,colors = c("green", "red"))
dev.off()

# Calculate risk metrics
SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

# print out some of these stats
SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)


pdf("XLB_ATR_strategy_alysis.pdf")
chart.Posn(portfolio.st, "XLB")
tmp <- namedLag(Cl(XLB), k = nLag)
add_TA(tmp$namedLag, col = "blue", on = 1)
dev.off()
