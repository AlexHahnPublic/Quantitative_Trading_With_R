#####################################################################
######################## toolsOfTheTrade.r ##########################
#####################################################################

# Purpose
# -------------------------------------------------------------------
# Simple Commands using R, syntax review, common data containers, basic R review
#
# Note that I will often call the variable immediately after assigning it in
# order to print to the console (the file can be executed with Rscript filename

# Basic Math
# -------------------------------------
# R can be used as a scientific calculator to evaluate expressions

1+1
sqrt(2)
20 + (26.8 * 23.4) / 2 + exp(1.34) * cos(1)
sin(1)
5^4
sqrt(-1 + 0i)

# Advanced mathematical operations are also possible
integrand <- function(x) 1 / ((x + 1) * sqrt(x))
integrate(integrand, lower = 0, upper = Inf)

# Variables and assignment
# -------------------------------------
# The assignment of a value to a variable is accomplished via the <- operation
x <- 3
x

x <- x + 1
x

z <- x^2
z

z <- "hello quants"
z

y <- "a"
y

Z <- sqrt(2)
Z

# Dots are acceptable in variable names
new.X <- 2.3
new.X

# Note that assignment via = is acceptable too, but for a more functional style
# I will continue to use <-


# Data Containers
# -------------------------------------
# Important and common data containers in R are vectors, matrices, data frames,
# lists, and environments

# Using the concatenation operator c() object
first_vector <- c(1, 2, 3, 4, 5, 6)
first_vector

second_vector <- c("a", "b", "hello")
second_vector

# Note the 2 and 23 are converted to strings
third_vector <- c("a", 2, 23)
third_vector

# It is straightforward to concatenate vectors and elements
new_vector <- c(first_vector, 7, 8, 9, second_vector)
new_vector

# Note that R is 1-indexed (unlike python which is 0-indexed)
example_1 <- new_vector[4]
example_1

# Extract the 5th and 8th elements
example_2 <- new_vector[c(5,8)]
example_2

# Vectorization is also supported in R
x <- c(1, 5, 10, 15, 20)

x2 <- x * 2
x2

x3 <- x ^2
x3

x4 <- x / x2
x4

x5 <- round(x * (x / x2) ^ 3.5 + sqrt(x4), 3)
x5

x6 <- round(c(c(x2[2:4], x3[1:2]), x5[4]), 2)
x6

# The matrix() object
# ---------------------------
# Matrices are stored in columnar format
my_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow =2, ncol = 3)
my_matrix

# To arrange by row format:
my_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow =2, ncol = 3, byrow = TRUE)
my_matrix

# A matrix is an object and has certain attributes as a result, lets create a
# matrix with its rows and columns labelled
dimnames(my_matrix) <- list(c("one", "hello"), c("column1", "column2", "column3"))
my_matrix

# We can now query the matrix for its attributes
attributes(my_matrix)

# To extract an element
ans <- my_matrix[1,3]
ans

# Operations on matrices can also be vecorized
new_matrix_1 <- my_matrix * my_matrix
new_matrix_1

new_matrix_2 <- sqrt(my_matrix)
new_matrix_2

# Vectorization and single element operations can be used in tandem
mat1 <- matrix(rnorm(1000), nrow = 100)
round(mat1[1:5, 2:6],3)

mat2 <- mat1[1:25, ] ^ 2
head(round(mat2,0), 9)[,1:7]

# The data.frame() object
# ---------------------------
# It's helpful to think of the data.frame() object as a single spreadsheet. It
# is a hybrid, two-dimensional container that can include different variable
# types at the same time. When data is read into R from an external environment
# it usually results in a data.frame() object

df <- data.frame(price = c(89.2, 23.2, 21.2),
                 symbol = c("MOT", "AAPL", "IBM"),
                 action = c("Buy", "Sell", "Buy"))
df

# Factors are a convenient data type that can assist in the categorization of
# data, to explicitly disable use the stringsAsFactors = FALSE argument
df3 <- data.frame(price = c(89.2, 23.2, 21.2),
                  symbol = c("MOT", "AAPL", "IBM"),
                  action = c("Buy", "Sell", "Buy"),
                  stringsAsFactors = FALSE)
class(df3$symbol)

# To find out what arguments are available for standard R functions use ? in
# front of the function
#?data.frame #suppress the output incase running as a script

# Dataframes can also be indexed via [ , ]
price <- df[1, 1]
price

# Intentionally show that R will throw an error if cols are different lengths
#df2 <- data.frame(col1 = c(1, 2, 3), col2 = c(1, 2, 3, 4))

# The $ operator extracts data columns by name
symbols <- df$symbol
symbols

# The "Levels" descriptor for the symbols implies that the type is a "factor
class(symbols)

# But remember we can set this type casting off as we did with df3 above
symbols <- df3$symbol
symbols

# The list() object
# ---------------------------
# The list object is very useful in that it is a more general container and can
# store objects of different types and sizes

my_list <- list(a = c(1, 2, 3, 4, 5),
                b = matrix(1:10, nrow = 2, ncol = 5),
                c = data.frame(price = c(89.3, 98.2, 21.2),
                               stock = c("MOT", "IBM", "CSCO")))
my_list

# Lists can be indexed by passing a number or by passing the element name into
# a double bracket operator [[]]
first_element <- my_list[[1]]
first_element

second_element <- my_list[["b"]]
second_element
class(second_element)

# Note that single brackets extract a section of a list (which is a list as a
# result), double brackets return an element of the list

part_of_list <- my_list[c(1, 3)]
part_of_list
class(part_of_list)

# length() returns the size of a list
length(my_list)

# The new.env() object
# ---------------------------
# The environment object is a powerful data structure in R due to leveraging
# quite a bit of it's inner workings "under the hood. It is most similar to a
# list object in the variability of what size and type of objects it can store
# but also has an added reference layer to a parent environment. Reference
# Semantics entail that variables do not store objects by their values but
# rather their address in memory, so only the address is passed around rather
# than the entire object. In short Environments are often used to emulate
# hash maps with O(1) lookup performance.

env <- new.env()
# Just as with lists we can assign by [[]] or by $
env[["first"]] <- 5
env[["second"]] <- 6
env$third <- 7

# Note that calling the object won't display it's contents but rather it's
# hexadecimal memory address
env

# ls() is needed in order to access names
ls(env)

# To obtain values associated with the names we can use the get() method
get("first", envir = env)

# Remove elements from an environment with rm()
rm("second", envir = env)
ls(env)

# Note that because environments use Reference Semantics, creating a copy of an
# environment than altering it, will also alter the original environment
# because the same underlying memory addresses/ pointers are always used

env_2 <- env
env_2$third <- 42
get("third", envir = env) # will be 42 even though we didn't directly change env

# Plotting
# -------------------------------------
# R has decent plotting/graphing capabilities even in the base installation.
# The more advanced graphing functionality is provided by external packages
# such as ggplot2, ggvis, rCharts, and rgl, all available on CRAN.

# The plot() command
# ---------------------------
# Create a vector of numbers x and plot them
# NOTE: running via terminal/ shell with Rscript will not create plot windows.
# These lines/ this section is more convenient in the R console or RStudio
x <- c(1,2, 3.2, 4, 3, 2.1, 9, 19)
plot(x)

# The type argument can be used to modify the graph, (say you want a line plot)
plot(x, type = 'l')

# Some nicer plotting (axis labels, title, grid, etc)
plot(rnorm(1000), main = "Some returns", cex.main = 0.9, xlab = "Time", ylab =
       "Returns")

# Superimpose a basic grid
grid()

# Create a few vertical and horizontal lines
abline(v = 400, lwd = 2, lty = 1)
abline(h = 2, lwd = 3, lty = 3)

# The par() command is used to query or set up global parameters that can be
# used to by all subsequent calls to plot() (like subplot set up)

# Create a 2-row, 2-column format
par(mfrow = c(2,2))

# First plot (points)
plot(rnorm(100), main = "Graph 1")

# Second plot (lines)
plot(rnorm(100), main = "Graph 2", type = "l")

# Third plot (steps) with a vertical line
plot(rnorm(100), main = "Graph 3", type = "s")
abline(v = 50, lwd = 4)

# Forth plot
plot(rnorm(100), type = "h", main = "Graph 4")

# Reset plot window
par(mfrow = c(1, 1))

# Add some text and a legend to a plot
plot(rnorm(100), main = "A line plot",
     cex.main = 0.8,
     xlab = "x-axis",
     ylab = "y-axis",
     type = "l")

# Extra text
mtext("Some text at the top", side = 3)

# At x = 40 and y = -1 coordinates add a legend
legend(40, -1, "A legend")

# The formals() function can also be used to extract the arguments of a function
formals(plot.default)

# Function
# -------------------------------------

# Add up all integers between 1 and 100, functionally
ans <- sum(1:100)
ans

# Imperitively we could write
answer <- 0
for(i in 1:100) {
  answer <- answer + i
}
answer

# The following few functions are worth memorizing

# Create 100 standard normals
x <- rnorm(100, mean = 0, sd = 1)
x

# Find the length of a vector x
length(x)

# Compute the mean of x
mean(x)

# Compute the standard deviation of x
sd(x)

# Compute the median value of x
median(x)

# Compute the range (min, max) of x
range(x)

# Find the sum of all the numbers in x
sum(x)

# Do a cumulative sum of the values in x
cumsum(x)

# Display the first three elements of x
head(x, 3)

# Display summary statistics on x
summary(x)

# Sortt x from largest to smallest
sort(x, decreasing = TRUE)

# Compute the successive difference in x
diff(x)

# Create an integer sequence from 1 to 10
1:10

# Create a sequence from 1 to 10 with step size 0.1
seq(1, 10, 0.1)

# Print the string hello to the screen
print("hello")

# Example of a conditional statement / loop
# Define a boolean variable
my_boolean <- 1 == 2

if(my_boolean) {
  print("not correct")
} else {
  print("XYZ")
}

# for() loop examples
for(i in 1:5) {
  cat(i, "\n")
}

some_list <- list()
for(z in c("hello", "goodbye")) {
  some_list[[z]] <- z
}
some_list
