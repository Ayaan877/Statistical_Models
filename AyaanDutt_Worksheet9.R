# Import data 
setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")

deer <- read.csv("deerdata.csv", na.strings = c("", " ", "na", "NA", "N/A"))
deer.sub <- with(deer, deer[!is.na(calf.mass)&!is.na(f.mass)&
                             !is.na(resources)&!is.na(age)&!is.na(feed.rate),])

### INTERACTIONS ###

# Eg: calf mass depends on feed rate but feed rate depends on dom category

deer.sub$dom = factor(deer.sub$dom) # Make dom categorical

# Linear Model with Interaction
mod1 = lm(calf.mass ~ dom + feed.rate + dom:feed.rate, data=deer.sub)
summary(mod1)

# Alternative syntax
mod1 = lm(calf.mass ~ dom*feed.rate, data=deer.sub)

### SIGNIFICANCE OF INTERACTION ###

# Generate a model without interaction
mod1.add = update(mod1, ~.-dom:feed.rate)
summary(mod1.add)

# Use the ANOVA function to check the difference of significance between the two models
anova(mod1, mod1.add)

# Eg 2: Feed rate depends on dom but dom depends on age 
mod2 = lm(feed.rate ~ age + dom + age:dom, data=deer.sub)
mod2.add = update(mod2, ~.-age:dom)
anova(mod2, mod2.add)

### EXAMINING INTERACTIONS ###

par(mfrow=c(2,3))
plot(feed.rate ~ age, data=deer.sub) # feed rate vs age

# Feed rate vs age (dom 1)
plot(feed.rate ~ age, deer.sub, subset = (dom=="1"), 
     xlim = range(age), ylim = range(feed.rate), pch=19,  col="red")
coef(mod2)
# Plot a a line with y = a + bx where a = intercept, b = age coeff 
abline(a = coef(mod2)["(Intercept)"], b = coef(mod2)["age"], col="red")

# Feed rate vs age (dom 2)
plot(feed.rate ~ age, deer.sub, subset=(dom=="2"),
     xlim = range(age), ylim = range(feed.rate), pch=19, col="blue")
# Note: Intercept is taken wrt dom 1, so coeff of dom 2 must be added to get new intercept
abline(c(coef(mod2)["(Intercept)"]+coef(mod2)["dom2"],
         coef(mod2)["age"]+coef(mod2)["age:dom2"]), col="blue") 

# Feed rate vs age (dom 3)
plot(feed.rate ~ age, deer.sub, subset=(dom=="3"),
     xlim = range(age), ylim = range(feed.rate), pch=19, col="purple")
abline(c(coef(mod2)["(Intercept)"]+coef(mod2)["dom3"],
         coef(mod2)["age"]+coef(mod2)["age:dom3"]), col="purple")

# Feed rate vs age (dom 4)
plot(feed.rate ~ age, deer.sub, subset=(dom=="4"),
     xlim = range(age), ylim = range(feed.rate), pch=19, col="orange")
abline(c(coef(mod2)["(Intercept)"]+coef(mod2)["dom4"],
         coef(mod2)["age"]+coef(mod2)["age:dom3"]), col="orange")

# EXERCISE 1: Load bf dataset and perform the same analysis. 

# Response = pop density
# Predictors = tree density, protection category
# Population density depends on tree density but tree density depends on protection category

bf = read.csv("butterfly_data.csv", na.strings=c("NA", "", " "))
bf.sub = with(bf, bf[!is.na(pop.den)&!is.na(protection)&!is.na(tree.den)&!is.na(soil.moist),])
bf.sub$protection = factor(bf.sub$protection) # categorize protection status
summary(bf.sub)

# Generate models with and without interaction
mod3 = lm(pop.den ~ protection*tree.den, data=bf.sub) 
mod3.add = update(mod3, ~.-protection:tree.den)

# Use ANOVA function to check for difference of significance
anova(mod3, mod3.add) 

# Examining the interactions
par(mfrow=c(2,2))
plot(pop.den ~ tree.den, data=bf.sub)

# Population density vs tree density (protection = full)
plot(pop.den ~ tree.den, bf.sub, subset=(protection=="Full"),
     xlim=range(tree.den), ylim=range(pop.den), pch=19, col='red')
abline(c(coef(mod3)["(Intercept)"], coef(mod3)["tree.den"]), col='red')

coef(mod3)

# Population density vs tree density (protection = none)
plot(pop.den ~ tree.den, bf.sub, subset=(protection=="None"),
     xlim=range(tree.den), ylim=range(pop.den), pch=19, col="blue")
abline(c(coef(mod3)["(Intercept)"]+coef(mod3)["protectionNone"],
         coef(mod3)["tree.den"]+coef(mod3)["protectionNone:tree.den"]), col="blue")

# Population density vs tree density (protection = partial)
plot(pop.den ~ tree.den, bf.sub, subset=(protection=="Partial"),
     xlim=range(tree.den), ylim=range(pop.den), pch=19, col="purple")
abline(c(coef(mod3)["(Intercept)"]+coef(mod3)["protectionPartial"],
         coef(mod3)["tree.den"]+coef(mod3)["protectionPartial:tree.den"]), col="purple")
