# Obligatory exercise 1 problem 1

# Reading datafiles
no2data <- read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/no2.txt",sep="\t",header=TRUE)

# 1a)
# Catogorizing the data
names(no2data)=c("log.no2","log.cars","temp","wind.speed","hour.of.day")

# Summary of the pollution levels and number of cars
summary(log.no2)
summary(log.cars)

# Boxplot of log.no2 and log.cars
boxplot(log.no2, log.cars, names = c("log.no2", "log.cars"))

# Scatterplot of log.cars and log.no2
plot(log.cars, log.no2)

# 1b)
# Linear fit
fit=lm(log.no2~log.cars)
summary(fit)

# Plotting linear fit together with the scatterplot
abline(fit, col = "red")
plot(log.cars,log.no2)

# Comparing r^2 to R^2
cor(log.cars,log.no2)**2

# 1c)
# Checking normality
library(car)
crPlots(fit, terms=~log.cars)

# Checking for constant variance
par(mfrow = c(1,2))
plot(fit,1)
plot(fit,3)

# Checking for normality
par(mfrow = c(1,2))
hist(fit$res)
boxplot(fit$res)
qqnorm(fit$res); qqline(fit$res)

# 1d)

# Checking correlation to see if two predictors are correlated
cor(no2data)

# Fitting a multiple linear regression model
fit.multi = lm(log.no2~log.cars+temp+log(wind.speed) + log(hour.of.day))


# Fitting a multiple linear regression model without hours.per.day as it is closely correlated to log.cars 
fit.multifinal = lm(log.no2~log.cars+temp+log(wind.speed))

# Summary of the final model
summary(fit.multifinal)
crPlots(fit.multifinal, terms=~log.cars+temp+log(wind.speed))
