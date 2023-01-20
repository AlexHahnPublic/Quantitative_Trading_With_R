#==========================================================
#=============== importTickersAdjClose.py =================
#==========================================================

# Purpose
#----------------------------------------------------------
# Given dates and ticker names as inputs, create an Adj_close.csv output file
# of each tickers yahoo finance adj close prices for that interval
# Note, this code is not in the book

#TODO list:
#----------------------------------------------------------
# - Error handling of ticker name input
#   - All symbols may not exist for the interval
#   -

import datetime as dt
import pandas as pd
import pandas_datareader.data as web

if __name__ == "__main__":
    # Collect the start date
    start_year = raw_input("Please enter start year: ")
    start_month = raw_input("Please enter start month (numeric): ")
    start_day = raw_input("Please enter starting day of the month: ")

    # Collect end date
    end_year = raw_input("Please enter end year: ")
    end_month = raw_input("Please enter end month (numeric): ")
    end_day = raw_input("Please enter ending day of the month: ")

    # Construct start_date and end_date datetime object
    start_date = dt.datetime(int(start_year), int(start_month), int(start_day))
    end_date = dt.datetime(int(end_year), int(end_month), int(end_day))

    # Gather the tickers list
    raw_ticker_lst = raw_input("Please enter the ticker names with a space in \
between: ")

    # Split into a list
    ticker_list = raw_ticker_lst.split()

    # Retrieve Yahoo Finance data
    yahoo_finance_data = web.DataReader(ticker_list, 'yahoo', start_date,
            end_date)

    # Output the Adj_Close .csv containing all tickers
    yahoo_finance_data["Adj Close"].to_csv("Adj_close.csv")
