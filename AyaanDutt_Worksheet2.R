# Dataframe
dat <- data.frame(mass = rnorm(n = 16, mean = 10, sd = 1), 
                  sex = rep(c("m", "f"), 8), 
                  call.rate = rnorm(n = 16, mean = 15, sd = 2))

# Subsetting
subset(dat, sex == "m") # all data for only males
dat[dat$sex == "m", ]

dat[dat$sex == "m", "mass"] # mass data of only males

mean(dat$mass)
subset(dat, mass < mean(mass)) # all data for individuals below avg mass

# Exercise 1
female <- subset(dat, sex == "f") # data of all females
mean(female$call.rate) # mean call rate of females 

### CREATING FUNCTIONS ###

# function(arg1, arg2, ....) {commands}
leaf.area <- c(100, 137, 188, 193, 203, 508, 694, 732, 921, 1045)

fun.select <- function (x, a) {
  x[x<a] # select values in x which are less than a
}

mean(leaf.area)
fun.select(x=leaf.area, a=mean(leaf.area)) # leaf areas with area less than mean

### FOR LOOPS ###

# for(variable in seq) {commands}
for (i in 1:5){
  print(i)
}

for (i in 1:10){
  print(i^2)
}

### IF ELSE COMMANDS ###  

temp <- rnorm(20, 5, 1)

# if command
if (length(temp) > 30){
  print(c(mean(temp), sd(temp)))
}

# if-else command
if (length(temp) > 30){
  print(c(mean(temp), var(temp)))
} else {
  print(c(median(temp), range(temp)))
}

### JACK-KNIFING ###
mass <- round(rgamma(n=20, shape=2, scale=20), 2)

mass.mean <- numeric(length(mass))

for(i in 1:length(mass)) {
  mass.mean[i] <- mean(mass[-i])
}

# Plotting
plot(1:length(mass), mass.mean, xlab = "Datapoint No.", ylab = "Subset Mean")
abline(h=mean(mass))

# EXERCISE 2: Use sapply() instead of a for loop

# Using for loop to find square of mass
mass.square <- numeric(length(mass))

for (i in 1:length(mass)){
  mass.square[i] <- (mass[i]^2)
}
print(mass.square)

# Using sapply() to find square of mass
fun.square <- function(x){
  x^2
}
sapply(mass, fun.square)
