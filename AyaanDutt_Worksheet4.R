# Loading data
setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")

deer <- read.csv(file="deerdata.csv", na.strings = c("","  ", "NA", "na"))
new <- deer[complete.cases(deer),]

# Using tapply()
with(new, tapply(X=f.mass, INDEX=resources, FUN=mean))
with(new, tapply(X=f.mass, INDEX=list(dom,resources), mean))

### TIDYVERSE ###
library(tidyverse)
library(dplyr)
library(scales)
library(colorspace)

# Filtering, selecting, grouping and summarising data
new %>% select(ID, age)
new %>% filter(age<median(age))

new %>% group_by(dom) %>% 
  summarise(means = mean(f.mass))

new %>% group_by(dom, resources) %>% 
  summarise(means = mean(calf.mass))

# Example: Mean calf mass of younger females of dominance category 1 for each resource level
new %>% filter(age<median(age), dom==1) %>%
  group_by(resources) %>%
  summarise(means=mean(calf.mass), sds=sd(calf.mass, na.rm=T))

# EXERCISE 1: Find mean f.mass of older females of dom 2 & 3, for each resource level
new %>% filter(age>median(age), dom==c(2,3)) %>% # Can also use dom==2&3 
  group_by(resources) %>%
  summarise(means = mean(f.mass))

### VISUALISING DATA ###

# Histogram syntax
hist(x=new$f.mass)
hist(x=new$f.mass[new$dom==1])

# Scatter plot syntax
plot(x=new$age, y=new$f.mass)
plot(f.mass~age, data=new)

# EXERCISE 2: Plot calf mass against female mass and color code by dom
plot(calf.mass~f.mass, data=new)
plot(calf.mass[dom==1]~f.mass[dom==1], xlim=range(f.mass), data=new, col='magenta')
points(calf.mass[dom==2]~f.mass[dom==2], data=new, col='red')
points(calf.mass[dom==3]~f.mass[dom==3], data=new, col='blue')
points(calf.mass[dom==4]~f.mass[dom==4], data=new, col='green')

# EXERCISE 3: Repeat the above for age vs female mass (note the lack of correlation)
plot(age~f.mass, data=new)
plot(age[dom==1]~f.mass[dom==1], xlim=range(f.mass), data=new, col='magenta')
points(age[dom==2]~f.mass[dom==2], data=new, col='red')
points(age[dom==3]~f.mass[dom==3], data=new, col='blue')
points(age[dom==4]~f.mass[dom==4], data=new, col='green')

### RECODING DATA ###

# Creating a new category column
ifelse(new$age>3, "old", "new")
new$newcat <- ifelse(new$age>3, "old","new") # new$newcat adds a column to the dataframe "new"
head(new)

new$newcat<- cut(new$age, breaks=c(0, 2, 6, 12), 
                 labels=c("juv", "sub", "adult")) 
head(new)


