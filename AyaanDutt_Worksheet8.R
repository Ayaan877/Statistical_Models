setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")

deer <- read.csv("deerdata.csv", na.strings = c("", " ", "na", "NA", "N/A"))
deer <- with(deer, deer[!is.na(calf.mass) & !is.na(f.mass) & !is.na(resources) & !is.na(age),])

### LINEAR MODELS ###

# Response = calf.mass, Predictor = f.mass  
calf.female.mod <- lm(calf.mass ~ f.mass, data=deer)
summary(calf.female.mod)
confint(calf.female.mod)
plot(calf.female.mod) # fit is linear (i.e. female mass is a good linear predictor for calf mass)

# Response = calf.mass, Predictor = age 
calf.age.mod <- lm(calf.mass ~ age, data=deer)
plot(calf.age.mod) # fit is non-linear (i.e. calf mass may have a non-linear relationship with age)

### ANOVA MODELS ###

# ANOVA: Linear model with a categorical variable
deer$dom <- factor(deer$dom)
deer$resources <- factor(deer$resources)

calf.dom.mod <- lm(calf.mass ~ dom, data=deer)
summary(calf.dom.mod)

# NOTE: the mean of the first category (alphabetically) is used as the reference point 
#       from which the deviation of the means of the other categories are measured

# We can calculate the individual means to show the deviation is equal to the linear model estimates
dom.means <- with(deer, tapply(calf.mass, dom, mean))
dom.means[1]
dom.means[2]-dom.means[1]
dom.means[3]-dom.means[1]
dom.means[4]-dom.means[1]

# EXERCISE 1: Linear model, response = calf.mass, predictor = resources
calf.res.mod <- lm(calf.mass ~ resources, data=deer)
summary(calf.res.mod)

### TREATMENT CONTRASTS ###

# Comparison A - 1,2 vs 3,4
# Comparison B - 1 vs 2
# Comparison C - 3 vs 4

contrasts(deer$resources) <- cbind(c(1/2, 1/2, -1/2, -1/2), 
                                  c(1/2, -1/2, 0, 0), 
                                  c(0, 0, 1/2, -1/2))
contrasts(deer$resources)

calf.res.contrasts.mod <- lm(calf.mass ~ resources, data=deer)
summary(calf.res.contrasts.mod)

# Comparing with calculated means and their differences
res.means <- with(deer, tapply(calf.mass, resources, mean))
mean(res.means)
mean(res.means[1:2] - res.means[3:4]) # Comparison 1
mean(res.means[1] - res.means[2])     # Comparison 2
mean(res.means[3] - res.means[4])     # Comparison 3

# EXERCISE 2: Find default contrasts + treatment contrasts for dominance category 

# Default contrasts
calf.dom.mod <- lm(calf.mass ~ dom, data=deer)
summary(calf.dom.mod)

# Treatment contrasts
contrasts(deer$dom) <- cbind(c(1/2, 1/2, -1/2, -1/2), 
                                   c(1/2, -1/2, 0, 0), 
                                   c(0, 0, 1/2, -1/2))
calf.dom.contrasts.mod <- lm(calf.mass ~ dom, data=deer)
summary(calf.dom.contrasts.mod)

# Checking with calculated means
mean(dom.means)
mean(dom.means[1:2] - dom.means[3:4]) # Comparison 1
mean(dom.means[1] - dom.means[2])     # Comparison 2
mean(dom.means[3] - dom.means[4])     # Comparison 3

# Reset contrasts to default (IMPORTANT)
contrasts(deer$resources) <- contr.treatment(levels(deer$resources))
contrasts(deer$dom) <- contr.treatment(levels(deer$dom))

# Plotting ANOVA models
plot(res.means)
plot(dom.means)

### MODEL CHECKING ###

summary(calf.res.mod)
plot(calf.res.mod)

# Multivariable model (multiple predictors)
calf.multi.mod <- lm(calf.mass ~ resources + f.mass + age + I(age^2), data=deer)
summary(calf.multi.mod)
