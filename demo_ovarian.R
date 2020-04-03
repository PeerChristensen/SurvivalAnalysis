# demo

library(survival)
library(survminer)
library(dplyr)
library(ggfortify) # autoplot

data(ovarian)
glimpse(ovarian)

# futime column holds the survival times
# fustat: whether censored or not
# rx: treatment type
# resid.ds: presence of residual disease
# ecog.ps: patient performance

# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Data seems to be bimodal
hist(ovarian$age) 

ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object 

fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)

ggsurvplot(fit1, data = ovarian, pval = T)
autoplot(fit1)

# Examine prdictive value of residual disease status
fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE)


fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)
ggforest(fit.coxph, data = ovarian)

cox_fit <- survfit(fit.coxph)
#plot(cox_fit, main = "cph model", xlab="Days")
autoplot(cox_fit)

# a hazard ratio of 0.25 for treatment groups tells you that patients
# who received treatment B have a reduced risk of dying compared to patients
# who received treatment A (which served as a reference to calculate the hazard ratio).
# As shown by the forest plot, the respective 95% confidence interval is
# 0.071 - 0.89 and this result is significant.

