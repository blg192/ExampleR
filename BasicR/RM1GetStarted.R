## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(psych)
library(psychTools)
library(ggplot2)
## possible data
data("mtcars")
data("sleep")
data("sat.act") ## in psych package
data("epi.bfi") ## psychTools package


## ## Code to set the working directory
## 
## ## NOTE: This file path will be different for everyone and dependent on how
## ## you have your files set up on your computer
## 
## setwd("~/classwork/PSYCH3010")
## 
## ## If you would like to see what your current WD is
## ## you can use
## getwd()


## Installing Packages
## ## Replace 'Package Name' with the name of the package you want to install
## install.packages("Package Name", dependencies = TRUE)


## Frequency Distribution Section
## Load in psych library
library(psych)
## Call up data set
data(sat.act)
## Create factor variables
sat.act$gender <- factor(sat.act$gender, levels = c(1, 2), labels = c('Male', 'Female'))
sat.act$education <- factor(sat.act$education, levels = c(0,1,2,3,4,5),
                            labels = c('< 12 years', 'HS Grad', 'Some College',
                                       'Enrolled College', 'College Grad', 'Grad/Pro Degree'))

## The table function will give us a count of how many
## people are in each group of the gender variable in the data
table(sat.act$gender)

## The breaks argument can be used to increase or decrease the
## width of the bins used in the histogram
hist(sat.act$ACT, breaks = 15, xlab = "ACT Score", main = "Histogram of ACT Scores")



## Measures of central tendency
## Mean
mean(sat.act$ACT)

## Median
median(sat.act$ACT)

## Mode
library(modeest)
mfv(sat.act$ACT)

## Standard Deviation 
sd(sat.act$ACT)




## t-test Section
data(sleep)
t.test(extra ~ group, data = sleep)

## NOTE: The defalut argument for t.test is to assume variances are not equal
## If you have tested and see they are, use var.equal = TRUE for Student's t-test

## If data is is wide format, change the arguemtns to be two vectors.
## t.test(sleep$group1, sleep$group2)


## Cohen's D Section

library(MOTE)
## need t-statistic, group sizes, alpha level
## From sleep data above: t = -1.8608, group 1 n = 10, group 2 n = 10
d.ind.t.t(t = -1.8608, n1 = 10, n2= 10, a = 0.05)





## Plots Section
## Bar Graph
library(ggplot2)

## NOTE: Make sure your grouping variable is a 'factor'
## If not you can turn it into one using factor()

## This code here creates an object to clean up the 
## ugly default ggplot formatting
cleanup <- theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"))

## Create empty plot object
## In aes, put x-axis variable then y-axis variable
sleepbar <- ggplot(data = sleep, aes(group, extra))

## Use stat summaries to get means and errors bars
sleepbar +
    stat_summary(fun = mean, geom = 'bar', fill = 'white', color = 'black') +
    stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = .2) +
    xlab('Group Number') + ylab('Increase in Hours of Sleep') + 
    ggtitle('Sleep Bar Graph') +cleanup


## Another example using the ACT data
actbar <- ggplot(data = sat.act, aes(education, ACT))

actbar +
    stat_summary(fun = mean, geom = 'bar', fill = 'white', color = 'black') +
    stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = .2) +
    xlab('Education Level') + ylab('Average ACT Score') + 
    ggtitle('Average ACT Score Across Education Levels') +cleanup


## Box Plot
sleepbox <- ggplot(sleep, aes(group, extra))

sleepbox +
    geom_boxplot() + xlab('Group') + ylab('Extra Hours of Sleep') +
    ggtitle('Box Plot for Sleep Data') + cleanup


## Stem-and-Leaf Plot
## The length of the plot can be changed with the 'scale = ' argument
stem(sleep$extra)


## Violin Plot
sleepviolin <- ggplot(sleep, aes(group, extra))

sleepviolin +
    geom_violin() + xlab('Group') + ylab('Extra Hours of Sleep') +
    ggtitle('Box Plot for Sleep Data') + cleanup


## Confidence Interval for a proportion 
## This function requires the number of successes, 
## total number of observations, and probability to test against
prop.test(x = 62, n = 100, p = .5)


## Confidence Interval Plot
## This code creates a loop to generate some data
## and then calculate the confidence interval
lower <- rep(NA, 100)
upper <- rep(NA, 100)
set.seed(587463)
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


## One-Way ANOVE Section
## One-way ANOVA using lm() function 
regmodel <- lm(ACT ~ education, data = sat.act)
anova(regmodel)

## Alternatively, use 'ez'
library(ez)

## ez needs a variable with participant numbers
## so create that here
sat.act$partno <- c(1:nrow(sat.act))

## One-way ANOVA
ezANOVA(data = sat.act, 
        dv = ACT, 
        between = education, 
        wid = partno, 
        type = 3, 
        return_aov = TRUE)


## Post-hoc Section
## The argument 'p.adjust.method = ' is what applies the correction
## in this case, we use a Bonferroni corrections on alpha
pairwise.t.test(sat.act$ACT,
                sat.act$education,
                paired = FALSE,
                var.equal = FALSE,
                p.adjust.method = 'bonferroni')


## Two Predictor Regression Section
regmodel <- lm(ACT ~ SATV + SATQ, data = sat.act)
summary(regmodel)


## Confidence Interval for Regression
confint(regmodel, level = 0.95)

