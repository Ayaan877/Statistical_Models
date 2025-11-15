# Import data
setwd("C:/Users/Ayaan Dutt/OneDrive/Desktop/College/Statistical Models")

bf <- read.csv("butterfly_data.csv", na.strings = c("", " ", "na", "NA", "N/A"))

# Response = butterfly population density
# Predictor = tree density

# With interaction, coeff of tree.den changes wrt pop.den for different protection categories
# Without interaction, coeff of tree.den is same wrt to pop.den for different protection categories, only differ by intercept

bf$protection = factor(bf$protection) # Categorise protection status

##ANCOVA with a categorical variable and continuous variable
mod1 = lm(pop.den ~ tree.den*protection, data=bf) # Interaction model
mod1.add = update(mod1, .~-tree.den:protection) # Independent model

anova(mod1, mod1.add, test="F")

### PLOTTING ###
# Note: colors are assigned by alphabetical order of category 
plot(pop.den ~ tree.den, col=as.factor(protection), pch=19, data=bf) 

# Alternative plotting syntax
plot(pop.den ~ tree.den, data=subset(bf, protection=="Full"),
     xlim=c(2,30), ylim=c(5,31), pch=19, col="black")
points(pop.den ~ tree.den, data=subset(bf, protection=="None"), col='red', pch=19)
points(pop.den ~ tree.den, data=subset(bf, protection=="Partial"), col='green', pch=19)

### FITTING THE PLOT ###
mod.est=coef(mod1)

curve(mod.est["(Intercept)"] + mod.est["tree.den"] * x, add=TRUE, col='black')
curve(mod.est["(Intercept)"] + mod.est["protectionNone"] +
        (mod.est["tree.den"] + mod.est["tree.den:protectionNone"]) * x,
      add=TRUE, col='red')
curve(mod.est["(Intercept)"] + mod.est["protectionPartial"] +
        (mod.est["tree.den"] + mod.est["tree.den:protectionPartial"]) * x,
      add=TRUE, col="green")

# We find that there seems to be a strong interaction for no protection, 
# but very little difference between partial and full protection 