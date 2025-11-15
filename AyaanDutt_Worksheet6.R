setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")

handspan <- read.csv(file="handspan.csv", na.strings = c("","  ", "NA", "na"))

# Simulating repeating the study many times by resampling with replacement from my original dataset
# Calculating the means and storing it

left.mean <- mean(handspan$L_cm)
left.se <- sd(handspan$L_cm)/(length(handspan$L_cm)-1)^0.5

### PROBABILITY DENSITY DISTRIBUTIONS ###

curve(dnorm, -3,3, xlab = "Z", ylab = "Probability Density")

# dnorm(x) = probability of x (output: x has a probability y)
# pnorm(x) = cumulative probability of x (output: y% of outcomes lie below x)
# qnorm(x) = y for which cumulative probability is x (output: x% of outcomes lie below y)

qnorm(0.975, mean = 0, sd = 1) # For 95% CI we use 0.975 because we subtract 0.25 from either end 
pnorm(1.96, mean = 0, sd = 1) # pnorm is essentially the inverse of qnorm

abline(v = c(-1.96, 1.96), col = "red") # Marks the 95% CI

# Computing 80%, 95% and 99% CI
left.80CI <- c(left.mean - qnorm(0.9, mean=0, sd=1)*left.se,
               left.mean + qnorm(0.9, mean=0, sd=1)*left.se)
left.80CI

left.95CI <- c(left.mean - qnorm(0.975, mean=0, sd=1)*left.se,
               left.mean + qnorm(0.975, mean=0, sd=1)*left.se)
left.95CI

left.99CI <- c(left.mean - qnorm(0.995, mean=0, sd=1)*left.se,
               left.mean + qnorm(0.995, mean=0, sd=1)*left.se)
left.99CI

data.frame(Lower.CL = c(left.80CI[1], left.95CI[1], left.99CI[1]),
           Upper.CL = c(left.80CI[2], left.95CI[2], left.99CI[2]),
           row.names = c(80, 95, 99))

# Experimental data plotting
iter <- 10000
mean.dist <- numeric(iter)
for (ctr in 1:iter) {
  mean.dist[ctr] <- mean(sample(handspan$L_cm, replace=T))
}

mean.dist[1:10]
hist(mean.dist)
sd(mean.dist)

### T-DISTRIBUTION ###

# The T-distribution is used for lower sample sizes and therefore has greater uncertainty 

# EXERCISE 1: Compare the normal distribution to the T-distribution
xvals = seq(-4,4, by = 0.01)
# Uniform Dist
plot(xvals, dnorm(xvals), type = "l", lty = 2, 
     ylab = "Probability Density", xlab = "Random Deviates", col='magenta')
# T-Dist using dt()
lines(xvals, dt(xvals, df = 5), col='red')
lines(xvals, dt(xvals, df = 10), col='blue') 
lines(xvals, dt(xvals, df = 100), col='green') 

# qt = does the same thing as qnorm, but for t-dist
qt(0.975, df = c(1:10, 20, 30, 50, 100, 1000, 1500))

# EXERCISE 2: Calculate a 95% CI assuming a T distribution for the left hand data
df.left = length(handspan$L_cm) - 1
left.95CI.t = c(left.mean - qt(0.975, df = df.left)*left.se,
                left.mean + qt(0.975, df = df.left)*left.se)
left.95CI.t
qt(0.975, df = df.left)

# We can perform the same analysis for a smaller sample size, thereby reducing our certainty
df.left.2 = (length(handspan$L_cm) - 1)/2
left.95CI.t = c(left.mean - qt(0.975, df = df.left.2)*left.se,
                left.mean + qt(0.975, df = df.left.2)*left.se)
left.95CI.t
qt(0.975, df = df.left.2)

### BOOTSTRAPPING CI ###

iterations = 1000
mean.dist2 = numeric(iterations)
for (i in 1:iterations){
  mean.dist2[i] = mean(sample(handspan$L_cm, replace = T))
}
hist(mean.dist2)
quantile(mean.dist2, probs = c(0.025, 0.975))

# EXERCISE 3: Using the deer data, use the f.mass and dominance categories to calculate the mean 
#            female mass of each dominance category. Find the 95% confidence intervals for each 
#            mean (assuming either a normal distribution or a bootstrapped CI).

deer = read.csv("deerdata.csv", na.strings = c("", " ", "na", "NA", "N/A"))
deer <- deer[complete.cases(deer),]

deer %>% 
  group_by(dom) %>% 
    summarise(mean_mass=mean(f.mass, na.rm=T))

# General Bootstrap Function
bootstrap_ci <- function(data, iterations = 1000) {
  boot_means <- replicate(iterations, mean(sample(data, replace = T)))
  quantile(boot_means, probs = c(0.025, 0.975))
}

# Finding female mass means + 95% CI by dominance category
deer_bootstrap <- deer %>%
                      group_by(dom) %>%
                        summarise(
                          Mean = mean(f.mass, na.rm = T),
                          LowerCI = bootstrap_ci(f.mass)[1],
                          UpperCI = bootstrap_ci(f.mass)[2])

deer_bootstrap

