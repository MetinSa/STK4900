# Reading in data
cirrhosis = read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/cirrhosis.txt",header=TRUE)

# Importing the survival library
library(survival)

#a)

# Making survival fits for each of the 4 covariates of interest
fit.treat = survfit(Surv(time,status)~treat, data=cirrhosis, conf.type="none")
fit.sex = survfit(Surv(time,status)~sex, data=cirrhosis, conf.type="none")
fit.asc = survfit(Surv(time,status)~asc, data=cirrhosis, conf.type="none")
fit.agegr = survfit(Surv(time,status)~agegr, data=cirrhosis, conf.type="none")

# Plotting the Kaplan-Meier estimate for each fit
plot(fit.treat,lty=1:2,xlab="Time [days]",ylab="Survival")
legend(3000,1,c("prednisone","placebo"),lty=1:2)

plot(fit.sex,lty=1:2,xlab="Time [days]",ylab="Survival")
legend(3300,1,c("Female","Male"),lty=1:2)

plot(fit.asc,lty=1:3,xlab="Time [days]",ylab="Survival")
legend(3300,1,c("None","Slight","Marked"),lty=1:3)

plot(fit.agegr,lty=1:3,xlab="Time [days]",ylab="Survival")
legend(3500,1,c("< 50","50-65",">65"),lty=1:3)
     

#b)

# Logrank tests
survdiff(Surv(time,status)~treat, data=cirrhosis)
survdiff(Surv(time,status)~sex, data=cirrhosis)
survdiff(Surv(time,status)~asc, data=cirrhosis)
survdiff(Surv(time,status)~agegr, data=cirrhosis)

#c)

# Cox regression fitting all covariates
fit.all=coxph(Surv(time,status==1)~factor(sex)+factor(treat)+factor(asc)+age ,data=cirrhosis)
summary(fit.all)