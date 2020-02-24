# survival analysis - scrit 1


library(tidyverse)
library(RODBC)
library(lubridate)
library(survival)
library(survminer)

# library(data.table)
# library(dtplyr)
# library(dplyr, warn.conflicts = FALSE)
# library(ggridges)
# library(factoextra)
# library(ggthemes)
# library(tictoc)
# library(h2o)
# library(hms)

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])


query <- "SELECT * FROM [DataMartMisc].[r].[CLTV_Premium_input]"

df <- sqlQuery(channel,query) %>%
  as_tibble()

df <- df %>% filter(MembershipDays>0)
# ---------------------------------------------------------------------------
# COX PH model

# 1 compute model
cox1 <- coxph(Surv(MembershipDays, Cancelled) ~ PremiumMembershipSource, df)

# 2 decide on imaginary data combinations
newdat <- expand.grid(PremiumMembershipSource =levels(df$PremiumMembershipSource))
rownames(newdat) <- letters[1:2]

# compute survival curves
cox_sf <- survfit(cox1, data = df, newdata = newdat, conf.type = "none")
head(cox_sf$surv)

# create df with survival curves
cox_surv <- surv_summary(cox_sf)
head(cox_surv)
cox_surv_df <- cbind(cox_surv,
                     PremiumMembershipSource = newdat[as.character(cox_surv$strata), ]) 

# plot
ggsurvplot_df(cox_surv_df, color = "PremiumMembershipSource",   legend.title = NULL, censor = FALSE)

cox_plot <- cox_surv_df %>%
  ggplot(aes(time,surv,colour = PremiumMembershipSource)) +
  geom_line() + 
  theme_minimal() +
  xlim(c(0,2000)) +
  ylim(c(0,1)) +
  theme(legend.position = "top") +
  ggtitle("Cox Proportional Hazard")


# compare with weibull
wbmod <- survreg(Surv(MembershipDays, Cancelled) ~ PremiumMembershipSource, df) 
summary(wbmod)


surv <- seq(.99, .01, by = -.01) 
t <- predict(wbmod, type = "quantile", p = 1 - surv, newdata = newdat)

surv_wbmod_wide <- cbind(newdat, t)

library("reshape2") 
surv_wbmod <- melt(surv_wbmod_wide, 
                   id.vars = c("PremiumMembershipSource"),
                   variable.name = "surv_id",  
                   value.name = "time") 

surv_wbmod$surv <- surv[as.numeric(surv_wbmod$surv_id)]

surv_wbmod[, c("upper", "lower", "std.err", "strata")] <- NA

ggsurvplot_df(surv_wbmod, surv.geom = geom_line,
              color = "PremiumMembershipSource", legend.title = NULL) 

wb_plot <- surv_wbmod %>%
  ggplot(aes(time,surv,colour = PremiumMembershipSource)) +
  geom_line() +
  xlim(c(0,2000)) +
  ylim(c(0,1)) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("Weibüll")

gridExtra::grid.arrange(cox_plot,wb_plot,ncol=2)                        
