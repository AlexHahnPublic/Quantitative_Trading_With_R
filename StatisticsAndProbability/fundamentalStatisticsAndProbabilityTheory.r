#####################################################################
############# Fundamental Statistic and Probabilities ###############
#####################################################################


# Demonstration of the convergence property of the law of large numbers

# Create a population of 1,000,000 random numbers from a Gaussian distribution defined with mean=2.33 and sd=0.5
set.seed(100)
X <- rnorm(1000000, mean = 2.33, sd = 0.5)

mu <- mean(X)
df <- sd(X)

pdf("populationHist.pdf")
hist(X,breaks = 100)
abline(v = mu, lwd = 3, lty= 2)
dev.off()

sample5 <- sample(X, 5, replace=TRUE)
sample10 <- sample(X, 10, replace=TRUE)
sample50 <- sample(X, 50, replace=TRUE)

m5 <- mean(sample5)
m10 <- mean(sample10)
m50 <- mean(sample50)

m1000 <- mean(sample(X,1000, replace=TRUE))
m10000 <- mean(sample(X,10000, replace=TRUE))

# not only does taking larger samples converge the sample mean to the population (law of large numbers),
# taking repeat measurements from X with the same sample size also similarly improves the estimation (central limit theorem)
mean_list <- list()
for(i in 1 :10000) {
    mean_list[[i]] <- mean(sample(X,10, replace=TRUE))
}

pdf("centralLimitTheoremEx.pdf")
hist(unlist(mean_list),
     breaks   = 500,
     xlab     = "Mean of 10 samples from X",
     main     = "Convergence of sample distribution",
     cex.main = 0.8)

abline(v = mu, lwd = 3, col = "white", lty = 2)

dev.off()

# To see how strong the CLT can be on non normal distributions consider picking a population by repeatedly choosing a 1 or
# 0 with 50% probability
pdf("ctlOfBiModal1Or0Dist_pop.pdf")
population <- sample(c(0,1), 100000, replace=TRUE)
hist(population, main = "Non-normal", cex.main = 0.8)
abline(v= mean(population), lwd=3, lty=3)
dev.off()


# by repeatedly extracting samples of even just size 10 from this highly non normal distribution we still obtain a normal looking distribution for the sample mean
mean_list <- list()
for(i in 1:10000) {
    mean_list[[i]] <- mean(sample(population, 10, replace=TRUE))
}

pdf("ctlOfBiModal1Or0Dist_sampleMeans.pdf")
hist(unlist(mean_list), main = "Distribution of averages",
     cex.main = 0.8,
     xlab = "Average of 10 samples")
abline(v=0.5, lwd=3)
dev.off()

# Three properties of a good sample stat:
# 1) expected value of a sample stat should be equal to true population, the "bias" of an estimator measures this
# 2) The variance of the sample stat should be as small as possible, this is the "efficiency" of an estimator
# 3) The sample distribution should converge to the true population parameter as we increase the sample size, ie "consistency"

# Bias of an estimator variance illustration:
# formula for population variance:
population_variance <- function(x) {
    mean <- sum(x) / length(x)
    return(sum((x - mean)^2) / length(x))
}

# create a population
population <- as.numeric(1:100000)
variance <- population_variance(population)

variance <- population_variance(population)

# compute the variance repeatedly with a sample size of 100
output <- list()
for(i in 1:1000) {
    output[[i]] <- population_variance(sample(population, 100, replace = TRUE))
}

variance_estimates <- unlist(output)
pdf("varianceEstimates.pdf")
hist(variance_estimates, breaks=100, cex.main = 0.9)
average_variance <- mean(variance_estimates)
abline(v=average_variance, lty=2, lwd=2)
abline(v=variance, lwd=2)
dev.off()

# Using a little bit of outside knowledge could we
# find a different formula such that the distribuition's mean will be closer to the true population varince?
sample_variance <- function(x) {
    mean <- sum(x) / length(x)
    return(sum((x-mean)^2) / (length(x) - 1))
}

output <- list()
for(i in 1:1000) {
    output[[i]] <- sample_variance(sample(population, 10, replace=TRUE))
}

sample_variance_estimates <- unlist(output)
average_sample_variance <- mean(sample_variance_estimates)

pdf("improvedSampleVarianceEstimates.pdf")
hist(variance_estimates, breaks=100, cex.main = 0.9)
average_variance <- mean(variance_estimates)
abline(v=average_variance, lty=2, lwd=2)
abline(v=average_sample_variance, lty=2, lwd=2, col="red")
abline(v=variance, lwd=2)
dev.off()

# this improvement is not coincidental, see proof in sampleVarianceProof.pdf

# example probability distribution
pdf("coinTossProbailityMassFunction.pdf")
plot(c(-1,1), c(0.5, 0.5), type="h", lwd=3,
     xlim=c(-2,2), main = "Probability mass function of a coin toss",
     ylab="Probaiility",
     xlab="Random Variable",
     cex.main=0.9)
dev.off()

# coin toss simulation, fair
outcomes <- sample(c(0,1), 1000, replace=TRUE)


# biased coin sumulation
set.seed(101)
biased_outcomes <- sample(c(0,1), 1000, replace = TRUE, prob = c(0.4, 0.6))

# frequenstist estimation of the probaility
prob_estimate <- sum(biased_outcomes) / length(biased_outcomes)
