#####################################################################
###############Speed, Testing, and Reporting ########################
#####################################################################

# Vectorized code is prefered over explicit looping
sum_with_loop_in_r <- function(max_value) {
    sum <- 0
    for(i in 1:max_value){
        sum <- sum + i
    }
    return(sum)
}

sum_with_vectorization_in_r <- function(max_value) {
    numbers <- as.double(1:max_value)
    return(sum(numbers))
}

# Benchmarking
library(microbenchmark)

speedtest <- microbenchmark(loop = sum_with_loop_in_r(1e5),
                            vectorized = sum_with_vectorization_in_r(1e5)
                            )

# By default R code is interpreted. We can also compile code in r do reduce runtime, and port compiled code from machine to machine
library(compiler)
compiled_sum_with_loop_in_r <- cmpfun(sum_with_loop_in_r)
compiled_sum_with_vectorization_in_r <- cmpfun(sum_with_vectorization_in_r)

compiled_speedtest <- microbenchmark(loop = sum_with_loop_in_r(1e5),
                                     compiled_loop = compiled_sum_with_loop_in_r(1e5),
                                     vectorized = sum_with_vectorization_in_r(1e5),
                                     compiled_vectorized = compiled_sum_with_vectorization_in_r(1e5)
                                     )

# I think either some of the code is compiled behind the scene/ precompiles sum
# but the compiled vs not doesn't make that big of a difference here

# example of Rccp compiled language call
library(Rcpp)

# Create a C++ function
cppFunction('
long add_cpp(long max_value) {
  long sum = 0;
  for(long i = 1; i <= max_value; ++i) {
    sum = sum + i;
  }
  return sum;
}'
)

s <- add_cpp(1e5)

cpp_speedtest <- microbenchmark(loop = sum_with_loop_in_r(1e5),
                                compiled_loop = compiled_sum_with_loop_in_r(1e5),
                                vectorized = sum_with_vectorization_in_r(1e5),
                                compiled_vectorized = compiled_sum_with_vectorization_in_r(1e5),
                                compiled_cpp = add_cpp(1e5)
                                )

# my results are still different (like ordering) than the book. I think the book is kinda old and advancements to r
# have been made enough so that basic stuff is like just as fast/ can't tell speedup in some cases.

# sourcing a separate .cpp file into r:
sourceCpp('./add_2.cpp')

cpp_test <- add_2_cpp(100)


# Two test driven development libraries:
library(RUnit)
library(testthat)

# Test driven development example for computing the log returns of a vector of prices
# Define function (note can have it return with message or just provide message an not return, commented out latter)
convert_to_returns <- function(prices) {
    #browser()
    if(length(prices) <2) {
        message("Not enough price entries.") # after running convert_to_returns(input_single_price) and watching it fail we implement this
        #} else {
    }
    #return(9) #fail to start
    return(diff(log(prices))) # implement after running and ensuring test fails to start
    #}
}

# Group related functionality together with context()
context("Price to log-return conversion")

# Define the expectations using expect_that()
test_that("convert_to_returns produces the correct values", {

    # For these inputs
    input_prices <- c(100, 101, 102, 103, 99)

    # Expect these outputs
    expected_returns <- c(0.009950331, 0.009852296, 0.009756175, -0.039609138)

    # Verify the expectation of equality
    expect_equal(expected_returns, convert_to_returns(input_prices))

    # input single price
    input_single_price <- c(100)
    msg <- "Not enough price entries."

    expect_message(convert_to_returns(input_single_price))
}
)
