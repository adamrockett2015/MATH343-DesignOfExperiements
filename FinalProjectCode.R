library(openxlsx)

data = read.xlsx("FinalProjectData.xlsx")
attach(data)

number.factor = as.factor(Number)
location.factor = as.factor(Location)


model = lm(Distance ~ number.factor + location.factor + number.factor*location.factor)

anova(model)

interaction.plot(number.factor, location.factor, Distance, main = "Interaction Plot")
qtukey(0.05, 9, 18, lower.tail = FALSE)

# Normal probability plot
resid = model$residuals
qqnorm(resid, main="Normal Probability Plot of Residuals")
qqline(resid)

# Plot of residuals
library(MASS)
e.star = studres(model)
y.hat=predict(model)
plot(e.star~y.hat, ylim=c(-3,3), ylab="Studentized Residuals", 
     xlab="Treatment Mean", main="Plot of Studentized Residuals vs. Treatment Means")
abline(h=2, col="blue", lty=2)
abline(h=-2, col="blue", lty=2)
abline(h=0)

hist(resid, main = "Histogram of Residuals")
index = 1:27
plot(resid~index, cex = 0.5)
lines(resid~index)

library(nortest)
ad.test(resid)    
