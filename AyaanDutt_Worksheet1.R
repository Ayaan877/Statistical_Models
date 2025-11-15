# Expression commands
8-1
4/6+2

# Assignment commands
a <- 9 
a
a + 1

# Vectors
a <- c(1, 2, 3, 4, 5) # vectors of num, chr, etc. c() combines objects
a*a 
a <- c("red", "purple", "darkgreen")
a <- c(male="red", female="purple", juv="darkgreen") # labels
a 

b <- seq(from=5, to=10)
b
b <- seq(from=5, to=100, by=7) # jumps "by 7"
b

p <- rep(c(1, 3), times=10) # repeated output
p

# Using vector subscripts - accessing vectors
b[1] 
b[c(1, 12)] # referencing values in a vector

pos <- c(2, 3, 11)
b[pos]  # displays position values
b[-pos] # removes position values 

a[1]
a["male"]

# Logical operators
b < 49
b[b<49] # values inside b less than 49 
which(b<49 & b>10) # index of values satisfying condition

b[b<49 & b>13] 

# Exercise 1
z <- b[b<25] # new vector with values of b less than 25
z

# Functions
log(10)
log(x=100, base=10)
log(100, 10) # equivalent to the above 

seq(from=2, to=20, by=2)
seq(from=2, to=20, length.out=15)

# Exercise 2
uni <- runif(20) # 20 random numbers from uniform dist. 
uni[c(2, 12)] # 2nd and 12th values
uni[uni<0.5] # values less than 0.5

length(b)
sum(b)
sum(b[b<25])

# Generating data-sets
trait.x <- rnorm(n=20, mean=1, sd=0.5) # 20 values from a Gaussian dist.
mean(trait.x)
sd(trait.x)

# Matrix and data-frames
dat.m <- matrix(c(1, 3, 5, 7, 9, 11), nrow=2, ncol=3, byrow=TRUE)
rownames(dat.m) <- c("Births", "Deaths")
colnames(dat.m) <- c("Turtles", "Snakes", "Lizards")
dat.m

cbind(Turtles=1:2, Snakes=3:4, Lizards=5:6)
d <- cbind(b, p)
d

# Accessing matrix
dat.m[1, 2]
dat.m[,-1]