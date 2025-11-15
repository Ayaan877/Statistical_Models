# Loading data
setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")
handspan = read.csv("handspan.csv", na.strings = c("NA",""))
str(handspan)
head(handspan)
summary(handspan)

### TESTING NULL HYPOTHESES ###

# Null hypotheses: Mean of right and left hand are equal. Shuffling values does not change result.
hand.pop = c(handspan$R_cm, handspan$L_cm)
hand.pop

sample(hand.pop, replace = F) # Random sample from data (without replacement)

# for() loop shuffles data set 1000 times and randomly assigns left/right to find null.diff. 
# The distribution of null.diff tells us about whether the null hypothesis is true or not

iter = 1000
null.diff = numeric(iter)

for(i in 1:iter){
  rand = data.frame(ran = sample(hand.pop, replace = F),
                     hand = rep(c("L", "R"),times = length(hand.pop)/2))
  means = with(rand,tapply(ran,hand,mean))
  null.diff[i] = means[1]-means[2]
}

# Plot null.diff distribution
hist(null.diff) 

# Observed left/right difference
obs.diff = mean(handspan$L_cm)-mean(handspan$R_cm) 

# Plot observed difference on histogram to determine probability
abline(v = obs.diff, col = "red")

# Calculating p-value (null hypothesis is rejected if p <= 0.05)
pval = length(null.diff[abs(null.diff)>=abs(obs.diff)])/iter
pval # Null hypothesis cannot be rejected (i.e. left and right handspan may not be correlated)

# EXERCISE 1: Do the same analysis for male and female right handspans
sample(handspan$R_cm, replace = F)

iter = 1000
mf.null = numeric(iter)

# Note: we remove one row to be able to divide by 2

for(i in 1:iter){
  Rand = data.frame(random = sample(handspan$R_cm[-1], replace = F), 
                     mf = rep(c("M", "F"), times = length(handspan$R_cm)/2))
  means_mf = with(Rand, tapply(random, mf, mean))
  mf.null[i] = means_mf[1]-means_mf[2]
}

hist(mf.null, xlim=c(-3, 3))

# Observed male/female difference
obs.mf.diff = with(handspan, tapply(handspan$R_cm, Sex, mean))
od = abs(obs.mf.diff[1]-obs.mf.diff[2])
abline(v = od, col = "red")

# Find p-value (null hypothesis is rejected if p <= 0.05)
p.mf.val = length(mf.null[abs(mf.null)>=abs(od)])/iter
p.mf.val # Null hypothesis is rejected (i.e. right handspan is correlated with sex)
