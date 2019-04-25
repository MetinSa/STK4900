# Reading in data
crabs=read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/crabs.txt",header=T)
summary(crabs)

# Choose a binary logistic regression model
fit.width=glm(y~width, data=crabs,family=binomial)
summary(fit.width)

# Computing the odd ratio
delta = 1
OR = exp(fit.width[["coefficients"]][["width"]]*delta)

# Finding confidence interval
expcoef=function(glmobj)
{
  regtab=summary(glmobj)$coef
  expcoef=exp(regtab[,1])
  lower=expcoef*exp(-1.96*regtab[,2])
  upper=expcoef*exp(1.96*regtab[,2])
  cbind(expcoef,lower,upper)
}
expcoef(fit.width)



# Binary logistic regression model for the other covariates one by one
fit.weight=glm(y~weight, data=crabs,family=binomial)
summary(fit.weight)

fit.color=glm(y~factor(color), data=crabs,family=binomial)
summary(fit.color)

fit.spine=glm(y~factor(spine), data=crabs,family=binomial)
summary(fit.spine)

# Grouped logistic fit for all significant covariates
fit.multi=glm(y~width+weight+factor(color)+factor(spine), data = crabs, family = binomial)
summary(fit.multi)

# Grouped logistic fit for all significant covariates
fit.multisig=glm(y~width+weight, data = crabs, family = binomial)
summary(fit.multisig)

# Checking correlation between width and weight
plot(crabs$weight, crabs$width, xlab = "Weight [kg]", ylab = "Width [cm]")
cor(crabs$weight, crabs$width)

# Fitting the final model
fit.multisig=glm(y~width, data = crabs, family = binomial)
summary(fit.multisig)

# Checking interaction
fit.multiint=glm(y~width+ factor(color) + width:factor(color), data = crabs, family = binomial)
fit.multiint=glm(y~width+ factor(spine) + width:factor(spine), data = crabs, family = binomial)
fit.multiint=glm(y~factor(color)+ factor(spine) + factor(color):factor(spine), data = crabs, family = binomial)
summary(fit.multiint)

