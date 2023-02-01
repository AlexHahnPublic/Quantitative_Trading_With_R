#####################################################################
########################## Optimization #############################
#####################################################################

# Example of using first and second order derivatives to compute a min/max / optimal solutions

# Create a function
f <- function(x) {
    return((1+x)^2)
}

# Create the derivatives
fp <- function(x) {
    return(2*(1+x))
}

# plot the function and its derivative
x <- seq(-5, 5, 0.1)
pdf("functionAndDerivative.pdf")
plot(x, f(x), type ='l', lwd = 2,
     main = "f(x) and f'(x)",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
grid()
lines(x,fp(x), lty = 3, lwd = 2)
abline(h = 0)
abline(v = 0)
dev.off()

f <- function(x) {
    #return(-x^2 - 4 * x + 1) # don't think this is correct, doesn't follow the results of the book despite it alleging this is the function
    #return(x^2 + 4 * x - 1) # this gets the appropriate results
    return(-x^2 - 4 * x + 1) # similarly this works, (and only is different by the minus sign in front)

}

## uniroot(f,c(-8,-1))
## uniroot(f,c(-1,2))

# Newton's method with a first order approximation
newton <- function(f, tol=1E-12, x0=1 , N=20) {
    # N = total number of iterations
    # x0 = initial guess
    # tol = abs(xn + 1 - xn)
    # f = function to be evaluated for a root

    h <- 0.001
    i <- 1; x1 <- x0
    p <- numeric(N)

    while (i <= N) {
        df_dx <- (f(x0 + h) - f(x0)) / h
        x1 <- (x0 - (f(x0) / df_dx))
        p[i] <- x1
        i <- i + 1
        if (abs(x1 - x0) < tol) {
            break
        }
        x0 <- x1
    }
    return(p[1:(i-1)])
}

## newton(f, x0 = -10)
## newton(f, x0 = 10)

# to see the iteration increasing accuracy you may have to set the digits displayed to a higher number
# options(digits = 14)

# Symbolic computation

# Create an expression
e <- expression(sin(x))

# Compute the derivative
d <- D(e, "x")

f_expr <- expression(x^2 + 4*x -1) # this one is actually correct/ consistent, looks like it was x^2+4x-1

test <- eval(f_expr, list(x=2)) # should be 11

newton_alternate <- function(f, tol=1E-12, x0=1, N=20) {
    # N = total number of iterations
    # x0 = initial guess
    # tol = abs(xn + 1 - xn)
    # f = function to be evaluated for a root

    # Compute the symbolic derivative
    df_dx = D(f, "x")

    i <- 1; x1 <- x0
    p <- numeric(N)
    while (i <= N) {
        x1 <- (x0 - eval(f, list(x=x0)) /
               eval(df_dx, list(x=x0)))
        p[i] <- x1
        i <- i +1
        if (abs(x1 - x0) < tol) {
            break
        }
        x0 <- x1
    }
    return(p[1:(i-1)])
}

## newton_alternate(f_expr, x0 = 10)
## newton_alternate(f_expr, x0 = -10)

# Brute force line of best fit, minimize obective function of sum of squared differences

# Create a set of random points x
set.seed(123)
x <- rnorm(100, 0, 1)

# Make y a function of x
y <- 3.2 + 2.9 * x + rnorm(100, 0, 0.1)

pdf("randomCloudForLineOfBestFit.pdf")
plot(x, y)
dev.off()

# objective function to minimize
objective_function <- function(y, x, a, b) {
    value <- sum((y - (a * x + b)) ^2)
    return(value)
}

# Create a range of a and b values and loop through all of them
a <- seq(-10, 10, 0.25)
b <- seq(-10, 10, 0.25)

output <- list()
z <- 1
for(i in 1:length(a)) {
    for(j in 1:length(b)) {
        output[[z]] <- c(objective_function(y, x, a[i], b[j]),
                         a[i], b[j])
        z <- z + 1
    }
}

# Create a matrix of the list and find the minimum value
mat <- do.call(rbind, output)

colnames(mat) <- c("obj","a","b")

smallest <- which(mat[, "obj"] == min(mat[, "obj"]))

# note we covered answer in the range and can only increase accuracy with a more granular search, ie
#a = seq(-5, 5, 0.1)
#b = seq(-5, 5, 0.1)

# Curve fitting exercise of bond maturities and interest rate yields

# Create fictitious yields
rates <- c(0.025, 0.03, 0.034, 0.039, 0.04, 0.045, 0.05, 0.06, 0.07, 0.071,
           0.07, 0.069, 0.07, 0.071, 0.072, 0.074, 0.076, 0.082, 0.088,
           0.09)

maturities <- 1:20

pdf("yieldCurveExampleForFitting.pdf")
plot(maturities, rates,
     main = "Yields",
     xlab = "years",
     ylab = "rate",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
grid()
dev.off()

# fit with a fifth-degree polynomial
poly_5 <- function(x, p) {
    f <- p[1] + p[2]*x + p[3]*x^2 + p[4]*x^3 + p[5]* x^4 + p[6]*x^5
    return(f)
}


# objective function to minimize (sum of squares error)
obj_5 <- function(x, y, p) {
    error <- (y - poly_5(x, p))^2
    return(sum(error))
}

# Fit the parameters. Assume 0 for all initial values
out_5 <- optim(obj_5, par = c(0, 0, 0, 0, 0, 0), x = maturities, y = rates)

pdf("yieldCurveFit5thDegree.pdf")
plot(maturities, rates,
     main = "Yields",
     xlab = "years",
     ylab = "rate",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

grid()

lines(poly_5(maturities, out_5$par), lwd = 1.5, lty = 2)

dev.off()

# trying to capture the kink in the middle using a higher order polynomial
poly_7 <- function(x, p) {
    f <- p[1] + p[2]*x + p[3]*x^2 + p[4]*x^3 + p[5]* x^4 + p[6]*x^5 + p[7]*x^6 + p[7]*x^7
    return(f)
}

obj_7 <- function(x, y, p) {
    error <- (y - poly_7(x, p))^2
    return(sum(error))
}

out_7 <- optim(obj_7, par = c(0, 0, 0, 0, 0, 0, 0, 0), x = maturities, y = rates)

pdf("yieldCurveFit5thAnd7thDegree.pdf")
plot(maturities, rates,
     main = "Yields",
     xlab = "years",
     ylab = "rate",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

grid()

lines(poly_5(maturities, out_5$par), lwd = 1.5, lty = 2, col="blue")
lines(poly_7(maturities, out_7$par), lwd = 1.5, lty = 2, col="red")

dev.off()


# ^ not much better! and increasing the degrees of freedom will simply memorize (overfit) the data

# One idea is two claim (with research/ support, more of an art) that there are two regimes and glue together two lower order polynomials

# We will keep the 5th degree polynomial and define another 3rd degree polynomial to use with the offset
poly_3 <- function(x, offset, intercept, b) {
    #browser()
    fu <- intercept + b[1]*(x-offset) + b[2]*(x-offset)^2 + b[3]*(x-offset)^3
    return(fu)
}

# When the maturity is equal to 9 (end of first curve beginning of second) we
# must make sure the polynomials have the same value
obj_3_5 <- function(x, y, offset, p) {
    #browser()

    # All points are at infinity initially
    fit <- rep(Inf, length(x))
    ind_5 <- x <= offset
    ind_3 <- x > offset

    fit[ind_5] <- poly_5(x[ind_5], p[1:6])
    fit[ind_3] <- poly_3(x[ind_3], offset,
                         poly5(offset, p[1:6]), p[7:9])

    error <- (y - fit) ^ 2
    return(sum(error))
}

# Fit the parameters. Assume 0 for all initial values
offset <- 9
out_3_5 <- optim(obj_3_5, par = rep(0,9),
                 x = maturities, y = rates, offset = offset)


pdf("yieldCurve2PolynomialsSpliceFit.pdf")
plot(maturities, rates, xlab = "years",
     main = "Yields",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
grid()

#original 5th degree polynomial entire curve
lines(poly_5(maturities[maturities <= offset],
             out_3_5$par[1:6]), lwd = 2)

# spliced 3 and 5 degree curve
lines(c(rep(NA, offset),
        poly_3(maturities[maturities > offset], offset,
               poly_5(offset, out_3_5$par[1:6]),
               out_3_5$par[7:9])), lwd = 2)
abline(v = offset)
dev.off()

# this is a bit contrived but shows what knobs you have to turn manually (number
# of polynomials, degrees of freedom for each, windows, etc)
# Another method is smoothing splines. Another is a locally weighted regression
# which can be implemented with R's loess() function

# Fit loess to the data
obj <- loess(rates ~ maturities, span = 0.5)

# Plot the data and the fit
pdf("loessFit.pdf")
plot(maturities, rates, main = "Rates", cex.main = 0.8)
lines(predict(obj), lty = 2)
dev.off()

# this looks okay but in real practice usually you k cross validate across
# span parameter ie optimize span as well with perhaps penalty

# Portfolio Construction/ Optimization example using Differential Evolution (DEoptim)
library(DEoptim)

# Drawdown function
compute_drawdown <- function(x, returns_default = TRUE, geommetric = TRUE) {
    browser()
    # x = Vector of raw pnl or returns
    # If returns_default = FALSE, the grometric argument is ignored and pnl is used
    # Output = the maximum drawdown

    if(returns_default) {
        # Cumulative return calculation
        if(geometric) {
            cumulative_return <- cumprod(1+x)
        } else {
            cumulative_return <- 1 + cumsum(x)
        }
        max_cumulative_return <- cummax(c(1, cumulative_return))[-1]
        drawdown <- -(cumulative_return / max_cumulative_return - 1)
    } else {
        # PnL vector is used
        cumulative_pnl <- c(0, cumsum(x))
        drawdown <- cummax(cumulative_pnl) - cumulative_pnl
        drawdown <- drawdown[-1]
    }

    # Drawdown vector for either pnl or returns
    return(drawdown)
}

obj_max_drawdown <- function(w, r_matrix, small_weight) {
    # w is the weight of every stock
    # r_matrix is the returns matrix of all stocks

    # Portfolio return
    portfolio_return <- r_matrix %*% w

    # Max drawdown
    drawdown_penalty <- max(compute_drawdown(portfolio_return))

    # Create penalty component for sum of weights
    weight_penalty <- 100 * (1 - sum(w)) ^ 2

    # Create a penalty component for negative weights
    negative_penalty <- -sum(w[w < 0])

    # Create penalty component for small weights
    small_weight_penalty <- 100 * sum(w[w < small_weight])

    # Objective function to minimize
    obj <- drawdown_penalty + weight_penalty + negative_penalty + small_weight_penalty

    return(obj)
}

# Calculate a returns matrix for multiple stocks
symbol_names <- c("AXP", "BA", "CAT", "CVX", "DD", "DIS", "GE", "HD", "IBM",
                  "INTC", "KO", "MMM", "MRK", "PG", "T", "UTX", "VZ")

# Load these prices into memory
price_matrix <- NULL
for(name in symbol_names) {
    # Extract the adjusted close price vector
    price_matrix <- cbind(price_matrix, get(name)[,6])
}
colnames(price_matrix) <- symbol_names

# Compute returns
returns_matrix <- apply(price_matrix, 2, function(x)
    diff(log(x)))

# Specify a small weight below which the allocation should be 0%
small_weight_value <- 0.02

# Specify lower and upper bounds for the weights
lower <- rep(0, ncol(returns_matrix))
upper <- rep(1, ncol(returns_matrix))

optim_result <- DEoptim(obj_max_drawdown, lower, upper,
                        control = list(NP = 400,
                                       itermax = 300,
                                       F = 0.25,
                                       CR = 0.75
                                       #, trace = FALSE # disable display output
                                       ),
                        returns_matrix, small_weight_value)
