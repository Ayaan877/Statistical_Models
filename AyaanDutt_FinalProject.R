### IMPORT DATA ###
setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")
data <- read.csv("res_data_Ayaan.csv", na.strings = c("", " ", "na", "NA", "N/A"))
head(data)
plot(data)

### DATA DISTRIBUTION (NORMALITY TESTING) ###
hist(data$rent, 10, xlab = "Rent", ylab = "Frequency", main = "Histogram of Rent")

qqnorm(data$rent); qqline(data$rent)
shapiro.test(data$rent)

# Shapiro test failed (p = 0.287)
# QQ-norm plot shows a good fit
# 
# Hence, the data can be considered to be normally distributed we can use a simple linear model

### LINEAR MODEL ###
# We have tested a linear model of the form: rent = b0 + b1*m.dist + b2*cc.dist
mod = lm(rent ~ m.dist + cc.dist, data=data)
summary(mod)
confint(mod)

# Extract coefficients
b0 = coef(mod)["(Intercept)"]
b1 = coef(mod)["m.dist"]
b2 = coef(mod)["cc.dist"]

# Mean m.dist and cc.dist
m.mean <- mean(data$m.dist)
cc.mean <- mean(data$cc.dist)

### PLOTS ###

# Rent vs Metro Distance
plot(data$m.dist, data$rent, pch = 19,
     xlab = "Metro Distance", ylab = "Rent", main = "Rent vs Metro Distance")
curve((b0 + b1*x + b2*cc.mean), add=T, col='blue')

# Rent vs City Center Distance
plot(data$cc.dist, data$rent, pch = 19,
     xlab = "City Center Distance", ylab = "Rent", main = "Rent vs City Center Distance")
curve((b0 + b1*m.mean + b2*x), add=T, col='blue')

# Alternative plots using ggplot2
library(ggplot2)

ggplot(data, aes(x = m.dist, y = rent)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Rent vs Metro Distance")

ggplot(data, aes(x = cc.dist, y = rent)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Rent vs City Center Distance")

### MODEL CHECKING ###
plot(mod)









