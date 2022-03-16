## Libraries
## install.packages('sn')
library(sn)

## Chapter 4
## Normal Distributions
## Set up two ranges of values
sequence1 <- seq(from = -3, to = 3, by = .01)
sequence2 <- seq(from = 6, to = 32, by = .01)

## Get densities
norm1 <- dnorm(sequence1, mean = 0, sd = 1)
norm2 <- dnorm(sequence2, mean = 19, sd = 4)

## Calculating Z-score from raw score
RawToZ <- function(obs, mean, sd){
    (obs - mean)/sd
}

## Calculating a Percentile from Z-score or Raw Score
## Z-Score (q is your z-score of interest)
pnorm(q = 2, mean = 0, sd = 1)

## Raw Score 
##(q is your raw score, make sure to change the mean and SD to reflect population values)
pnorm(q = 17, mean = 19, sd = 4)

## Calculating Z-score or Raw Score from a Percentile
## Z-score (p is your percentile)
qnorm(p = .80, mean = 0, sd = 1)

## Raw Score (remember to use population values here for mean and SD)
qnorm(p = .43, mean = 19, sd = 4)

## Calculating Raw Score from Z-score
ZToRaw <- function(Z, mean, sd){
    (Z*sd) + mean
}


## Chapter 5
## Central Limit Theorem Example

## Example of a skewed population distribution
## the rsn() function creates a vector of random numbers from a skewed normal distribution
skewedpop <- rsn(n=10000, xi=5, omega=5, alpha=20, tau=0, dp=NULL)
hist(skewedpop)

## Test CLT by drawing many random samples of size 30 and taking the mean
samplemeans <- rep(NA, 10000)
for (i in 1:10000){
    tmp <- rsn(n=30, xi=5, omega=5, alpha=20, tau=0, dp=NULL)
    samplemeans[i] <- mean(tmp)
}
hist(samplemeans, breaks = 100)


## Confidence Intervals
CIfun <- function(obs, CI, se) {
    z <- qnorm((1-CI)/2, lower.tail = FALSE)
    margerr <- z*se
    lowerlim <- obs - margerr
    upperlim <- obs + margerr
    print(c(lowerlim, upperlim))
}

CIfun(obs = 1, CI = .95, se = .5)

## ----Confidence Interval Plot-------------------------------------------------------------------------------------------------------
## This code creates a loop to generate some data
## and then calculate the confidence interval
lower <- rep(NA, 100)
upper <- rep(NA, 100)
set.seed(587500)
for(i in 1:100) {
    y <- rnorm(100, mean = 5, sd = 5)
    lower[i] <- mean(y) - 1.96 * sd(y) / sqrt(length(y))
    upper[i] <- mean(y) + 1.96 * sd(y) / sqrt(length(y))
}
CIs <- cbind(lower, upper)

## identify intervals not covering mu
## for a 95% CI and 100 samples this should be about 5
ID <- which(!(CIs[1:100, 1] <= 5 & 5 <= CIs[1:100, 2]))

## initialize the plot
plot(0, 
     xlim = c(3, 7), 
     ylim = c(1, 100), 
     ylab = "Sample", 
     xlab = expression(mu), 
     main = "Confidence Intervals")

## set up color vector
colors <- rep(gray(0.6), 100)
colors[ID] <- "red"

## draw reference line at mu=5
abline(v = 5, lty = 2)

## add horizontal bars representing the CIs
for(j in 1:100) {
    lines(c(CIs[j, 1], CIs[j, 2]), 
          c(j, j), 
          col = colors[j], 
          lwd = 2)
}

## ----Proportions NHST with Confidence Interval -------------------------------------------------------------------------------------------
## This function requires the number of successes, 
## total number of observations, and probability to test against
prop.test(x = 62, n = 100, p = .5)