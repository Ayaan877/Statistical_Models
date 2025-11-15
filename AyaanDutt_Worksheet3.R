### CENTRAL LIMIT THEOREM ###

# Uniform dist.
n.samples <- 500 # no. of samples

n.obs <- 30 # no. of obs per sample
  
mean.vec <- numeric(n.samples) # empty vector to store means
  
for (i in 1:n.samples) {
  mean.vec[i] <- mean(runif(n.obs)) # store mean of each distribution
}

hist(mean.vec)  
mean(mean.vec)

# EXERCISE 1: Change sample size (n.obs)

# for n.obs = 30, mean = 0.499747
# for n.obs = 10, mean = 0.5052716
# for n.obs = 5, mean = 0.4986066
# for n.obs = 2, mean = 0.5126445

# Note: We used a large number of samples, so even a small sample size 
# gives us a reasonably good value for the mean

### UPLOADING, CLEANING AND ANALYSING A DATASET ###

setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")

deer <- read.csv(file="deerdata.csv", na.strings = c("","  ", "NA", "na"))

names(deer) # column headings
head(deer) # initial values
tail(deer) # final values
str(deer) # column names and data types

# Extracting age data
deer$age
with(deer, age)
deer[,"age"]

# Body mass of young females
with(deer, deer[age<median(age, na.rm=T), "f.mass"])

# EXERCISE 2: Find body mass of young deer in two steps
young.deer <- subset(deer, age < median(deer$age, na.rm=T))
young.deer$f.mass

summary(deer)

# Cleaning data: Converting column types, dropping NA values, remove duplicates
deer$dom <- factor(deer$dom)
deer$resources <- factor(deer$resources)

# Checking for duplicate rows
which(duplicated(deer)=="TRUE")
which(duplicated(deer))
deer[duplicated(deer), ]

# Checking for NA values
which(complete.cases(deer)=="FALSE")
which(!complete.cases(deer))
deer[!complete.cases(deer), ]

# Removing NAs and duplicates
deer.new <- deer[!duplicated(deer), ] 
deer.new <- deer.new[complete.cases(deer.new), ]

# Which age values have NA? Using is.na 
which(is.na(deer$age))
which(!is.na(deer$age))

deer[!is.na(deer$age), ]

# Summary statistics
mean(deer.new$age)
median(deer.new$age)
sd(deer.new$age)
var(deer.new$age)

table(deer.new$resources)
with(deer.new, table(dom, resources))

# Mean mass according to dominance category. Using tapply()
with(deer.new, tapply(X = f.mass, INDEX = resources, FUN = mean))
mean(deer.new$f.mass)

