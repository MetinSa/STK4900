# Reading in data
olympic=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/olympic.txt",sep="\t",header=TRUE)

# Making a fit for all covariates
fit.1 = glm(Total2000~offset(Log.athletes) + Total1996 + Log.population + GDP.per.cap ,data=olympic,family=poisson)
summary(fit.1)

# Checking correlation between covariates
plot(olympic)
cor(olympic$Total2000, olympic$Total1996)


# Making a fit without 1996
fit.2 = glm(Total2000~offset(Log.athletes)+Log.population + GDP.per.cap,data=olympic,family=poisson)
summary(fit.2)

# Making a fit without GDP and 1996
fit.3 = glm(Total2000~offset(Log.athletes)+Log.population,data=olympic,family=poisson)
summary(fit.3)

# Computing rate ratio
exp(fit.3$coefficients)
