tl = c(35.1,45.8,55.1,40.8,53.3,52.7,58.4,43.3, 34.2,44.8,46.6,40.4,46.4,51.8,39.7)

# Likelihood
mod1 = dnorm(tl, mean=30, sd=7, log=FALSE)
mod2 = dnorm(tl, mean=40, sd=7, log=FALSE)
mod3 = dnorm(tl, mean=50, sd=7, log=FALSE)
mod4 = dnorm(tl, mean=60, sd=7, log=FALSE)

prod(mod1)
prod(mod2)
prod(mod3)
prod(mod4)

# Log Likelihood

mod.log1 = dnorm(tl, mean=30, sd=7, log=TRUE)
mod.log2 = dnorm(tl, mean=40, sd=7, log=TRUE)
mod.log3 = dnorm(tl, mean=50, sd=7, log=TRUE)
mod.log4 = dnorm(tl, mean=60, sd=7, log=TRUE)

-sum(mod.log1)
-sum(mod.log2)
-sum(mod.log3)
-sum(mod.log4)

# Importing Data
setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")

crop.dat <- read.csv("example3.csv", na.strings = c("", " ", "na", "NA", "N/A"))
head(crop.dat)

# Linear Model with response = damaged/total, predictor = distance
plot(damaged/total ~ distance, data = crop.dat)
mod = lm(damaged/total ~ distance, data = crop.dat)
abline(coef(mod))

# Model check
plot(mod)

### BINOMIAL GLM ###

# logitP = b0 + b1 * dist
# Probability of damage, P = exp(b0 + b1 * dist)/(1 + exp(b0 + b1 * dist))
# P = exp(logitP)/(1+exp(logitP))

crop.dat$undamaged <- crop.dat$total - crop.dat$damaged
crop.dat$undamaged

glm.result = glm(cbind(damaged, undamaged) ~ distance, data=crop.dat, 
                 family=binomial(link='logit'))
summary(glm.result)

confint(glm.result)

plot(damaged/total ~ distance, data = crop.dat)
points(predict(glm.result, type="response") ~ distance, data=crop.dat, col='blue', pch=16)

# Manually finding the prediction
plot(damaged/total ~ distance, data = crop.dat)
logit.p = 2.63548 - 0.55260*crop.dat$distance
p.damage = exp(logit.p)/(1+exp(logit.p))

# Model point prediction
points(p.damage ~ distance, data = crop.dat, col='red', pch=16)

# Model curve
curve(exp(coef(glm.result)[1] + coef(glm.result)[2]*x)/
        (1+exp(coef(glm.result)[1] + coef(glm.result)[2]*x)), add=T, col='blue')

# Model checking
plot(glm.result)

### DISPLAY RATE DATA ###
dis.dat <- read.csv("display_rate.csv", na.strings = c("", " ", "na", "NA", "N/A"))
head(dis.dat)

plot(display ~ females, data=dis.dat, ylim=range(c(-10, 30)))

# Linear Model with response = display, predictor = females
mod = lm(display ~ females, data = dis.dat)
abline(coef(mod))

plot(mod)

### POISSON GLM ###

# Y = exp(b0 + b1 * x)

glm.dis = glm(display ~ females, data=dis.dat, family=poisson(link='log'))
summary(glm.dis)
confint(glm.dis)

# Plot model
plot(display ~ females, data = dis.dat)
points(predict(glm.dis, type="response") ~ females, data=dis.dat, col='blue', pch=16)
curve(exp(coef(glm.dis)[1] + coef(glm.dis)[2]*x), add=T, col='red')

# Model checking
plot(glm.dis)

### EXERCISE ### 

# Fit a simple lm and compare with a glm for calf.mass ~ f.mass 
# family = gaussian/normal (check which one)

setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")

deer.dat <- read.csv("deerdata.csv", na.strings = c("", " ", "na", "NA", "N/A"))
deer.dat <- with(deer.dat, deer.dat[!is.na(calf.mass)&!is.na(f.mass),])
head(deer.dat)

# Linear Model with response = calf.mass, predictor = f.mass
plot(calf.mass ~ f.mass, data = deer.dat)
mod = lm(calf.mass ~ f.mass, data = deer.dat)
abline(coef(mod))

# Model check
plot(mod)

### GAUSSIAN GLM ###

glm.deer = glm(calf.mass ~ f.mass, data=deer.dat, family=gaussian(link = "identity"))
summary(glm.deer)
confint(glm.deer)

# Plot model
plot(calf.mass ~ f.mass, data = deer.dat)
points(predict(glm.deer, type="response") ~ f.mass, data=deer.dat, col='blue', pch=16)
curve((coef(glm.deer)[1] + coef(glm.deer)[2]*x), add=T, col='red')

# Model checking
plot(glm.dis)