set.seed(23072016)
# Loading in the data.
data("mtcars")

# Transforming the proper variables to factor variables. 
cols <- c("cyl", "vs", "gear", "carb")
mtcars[, cols] <- lapply(mtcars[, cols], as.factor)
mtcars$am <- factor(mtcars$am, levels = c("0", "1"), labels = c("Automatic", "Manual"))

# Some exploratory analysis.
summary(mtcars)
boxplot(mpg ~ am, data = mtcars)

pairs(mtcars, panel = function(x, y){
    points(x, y)
    abline(lm(y~x), col = "red")
})

# Testing for significant differences between MPG for TT
shapiro.test(mtcars$mpg) # Assumption of normalcy holds
result <- t.test(mpg ~ am, data = mtcars)

# Fitting a linear regression model.
fit <- lm(mpg ~ am, data = mtcars)
summary(fit)

fit.all <- lm(mpg ~ ., data = mtcars)
fit.final <- step(fit.all, direction = "both")
summary(fitall)