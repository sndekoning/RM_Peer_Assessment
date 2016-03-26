# Setting seed and loading data & dependencies
set.seed(23072016)
data("mtcars")
library(knitr)
# Transforming variables to factor, giving proper labels.
cols <- c("cyl", "vs", "gear", "carb")
mtcars[, cols] <- lapply(mtcars[, cols], as.factor)
mtcars$am <- factor(mtcars$am, levels = c("0", "1"), labels = c("Automatic", "Manual"))
# Testing for the assumption of normality
normal <- shapiro.test(mtcars$mpg)
# Studen t.test for difference in means.
result <- t.test(mpg ~ am, data = mtcars)
# Fitting a linear model between mpg as the dependent, and am as the independent variable
fit <- lm(mpg ~ am, data = mtcars)
summary(fit)$coefficients
# Fitting a linear model between mpg as the dependent variable against all the other variables
fit.all <- lm(mpg ~ ., data = mtcars)
fit.final <- step(fit.all, direction = "both") # Using step method to determine which variables to include
summary(fit.final)$coefficients
# Testing the simple lm against the new multivariate lm
anova(fit, fit.final)
# Boxplot fig I
boxplot(mpg ~ am, data = mtcars, main = "Figure I")
#Pairsplot fig II
pairs(mtcars,
      main = "Figure II",
      panel = function(x, y){
          points(x, y)
          abline(lm(y~x), col = "red")})
# Residuals and Diagnostics plot fig III
par(mfrow = c(2,2))
plot(fit.final, main = "Figure III")