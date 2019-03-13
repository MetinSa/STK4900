# Obligatory exercise 1 problem 2

# 2a)

# Reading in data
blood <- read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/blood.txt",sep=",",header=TRUE, col.names=c("bloodpressure","agegroup"))

# Making a categorical boxplot
boxplot(blood$bloodpressure ~ blood$agegroup, xlab = "Age Group", ylab = "Blood Pressure")

# Summaries of each group
summary(blood$bloodpressure[blood$agegroup==1])
summary(blood$bloodpressure[blood$agegroup==2])
summary(blood$bloodpressure[blood$agegroup==3])

# 2b)
# Defining agegroup as a categorical variable
blood$agegroup = factor(blood$agegroup)

# Making a one way anova 
aov.blood = aov(bloodpressure~agegroup, data = blood)

# Summary of anova
summary(aov.blood)

# Making a linear regression
fit.blood = lm(bloodpressure~agegroup, data = blood)

# Summary of the fit
summary(fit.blood)
