# survival analysis - script 1

# ---------------------------------------------------------------------------
library(tidyverse)
library(RODBC)
library(lubridate)
library(survival)
library(survminer)

# ---------------------------------------------------------------------------

# credentials <- read_rds("credentials.rds")
# 
# channel <- odbcConnect(credentials[1],credentials[2],credentials[3])
# 
# 
# query <- "SELECT * FROM [DataMartMisc].[r].[CLTV_Premium_input]"
# 
# df <- sqlQuery(channel,query) %>%
#   as_tibble()
# 
# df <- df %>% filter(MembershipDays>0)

# ---------------------------------------------------------------------------
# LOAD DATA
# ---------------------------------------------------------------------------

df <- read_csv("survival_data.csv") %>%
  mutate(months = ceiling(MembershipDays / 30),
         start_month = factor(zoo::as.yearmon(StartDate)))

df <- df %>%
  mutate_if(is.character,as.factor) #%>%
 # filter(StartDate >= as.Date("2018-01-01"),
  #       StartDate <= today()-364)

# ---------------------------------------------------------------------------
# Kaplan-Meier
# ---------------------------------------------------------------------------

km <- survfit(Surv(months, Cancelled) ~1,df)

ggsurvplot(km,censor=F)

km_df <- tibble(time = km$time,
                surv = km$surv,
                upper = km$upper,
                lower = km$lower)

km_surv_prob_12mo <- round(km_df$surv[km_df$time==12],3)

p1 <- km_df %>% 
  ggplot(aes(time,surv)) + 
  geom_line() +
  ylim(c(0,1)) +
  ggtitle("Kaplan-Meier") +
  geom_segment(aes(x = 0, xend = 12,
                   y= km_surv_prob_12mo,
                   yend = km_surv_prob_12mo), 
               linetype="dashed",data = km_df) +
  geom_segment(aes(x = 12,xend=12,y = 0,yend =km_surv_prob_12mo),
               linetype = "dashed") +
  annotate("text", x = 15, y = km_surv_prob_12mo+.1,
           label = glue::glue("12mo surv. prob.: {km_surv_prob_12mo}"))

# ---------------------------------------------------------------------------
# Weibüll
# ---------------------------------------------------------------------------

wbmod <- survreg(Surv(months, Cancelled) ~ 1, df) 

surv <- seq(.99, .001, by = -.001)

t <- predict(wbmod, type = "quantile", p = 1 - surv, newdata = data.frame(1))

wb_df <- data.frame(time = t, surv = surv)

# Surv rates at specific months
indices <- NULL

for (i in 0:max(df$months)) {
  ind = which(abs(t - i) == min(abs(t - i)))
  indices = c(indices,ind)
}

wb_df <- wb_df[indices,] %>%
  mutate(time2 = row_number()-1)

wb_surv_prob_12mo <- wb_df$surv[wb_df$time2==12]

p2 <- wb_df %>% 
  ggplot(aes(time,surv)) + 
  geom_line() +
  ylim(c(0,1)) +
  ggtitle("Weibüll") +
  geom_segment(aes(x = 0, xend = 12,
                   y=wb_surv_prob_12mo,
                   yend = wb_surv_prob_12mo), 
                    linetype="dashed",data = wb_df) +
  geom_segment(aes(x = 12,xend=12,y = 0,yend =wb_surv_prob_12mo),
               linetype = "dashed") +
  annotate("text", x = 15, y = wb_surv_prob_12mo+.1,
           label = glue::glue("12mo surv. prob.: {wb_surv_prob_12mo}"))

# ---------------------------------------------------------------------------
# Cox PH
# ---------------------------------------------------------------------------

cox_mod <- coxph(Surv(months, Cancelled) ~ 1, df)

cox_sf <- survfit(cox_mod, data = df, conf.type = "none")

cox_sf_sum <- summary(cox_sf)

cox_df <- tibble(time=cox_sf_sum$time,
                 n.risk=cox_sf_sum$n.risk,
                 n.event=cox_sf_sum$n.event,
                 surv=cox_sf_sum$surv,
                 strata=1) 

cox_surv_prob_12mo <- cox_df$surv[cox_df$time==12]

p3 <- cox_df %>%
  ggplot(aes(x=time,y=surv)) +
  geom_line() +
  ylim(c(0,1)) +
  ggtitle("Cox-PH") +
  geom_segment(aes(x = 0, xend = 12,
                   y=cox_surv_prob_12mo,
                   yend = cox_surv_prob_12mo), 
               linetype="dashed",data = cox_df) +
  geom_segment(aes(x = 12,xend=12,y = 0,yend =cox_surv_prob_12mo),
               linetype = "dashed") +
  annotate("text", x = 15, y = cox_surv_prob_12mo+.1,
           label = glue::glue("12mo surv. prob.: {round(cox_surv_prob_12mo,3)}"))

# plot
gridExtra::grid.arrange(p1,p2,p3,ncol=3)

# ---------------------------------------------------------------------------
# use this!

cox_mod <- coxph(Surv(months, Cancelled) ~ start_month, df)

start_month <- c(levels(df$start_month))
                 
newdat <- expand.grid(start_month = start_month)
rownames(newdat) <- as.character(1:nrow(newdat))

# compute survival curves
cox_sf <- survfit(cox_mod, data = df,newdata = newdat)

# create df with survival curves
cox_surv <- surv_summary(cox_sf)
head(cox_surv)
cox_surv_df <- cbind(cox_surv,
                     start_month = newdat[as.character(cox_surv$strata), ]) 

# plot
ggsurvplot_df(cox_surv_df, color = "start_month",   legend.title = NULL, censor = FALSE)
ggsurvplot_df(cox_surv_df, color = "start_month",   legend.title = NULL, censor = FALSE,conf.int = T) +
  facet_wrap(~start_month) +
  theme(legend.position="none")

# ---------------------------------------------------------------

# 2 decide on imaginary data combinations
newdat <- expand.grid(PremiumMembershipSource =levels(df$PremiumMembershipSource))
rownames(newdat) <- letters[1:2]

# compute survival curves
cox_sf <- survfit(cox1, data = df, conf.type = "none")
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
  xlim(c(0,36)) +
  ylim(c(0,1)) +
  theme(legend.position = "top") +
  ggtitle("Cox Proportional Hazard")


# compare with weibull
wbmod <- survreg(Surv(months, Cancelled) ~ PremiumMembershipSource, df) 
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
  xlim(c(0,36)) +
  ylim(c(0,1)) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("Weibüll")

gridExtra::grid.arrange(cox_plot,wb_plot,ncol=2)                  

# extra
library(survival)
library(data.table)
library(ggplot2)

data = fread('https://raw.githubusercontent.com/IBM/invoke-wml-using-cognos-custom-control/master/data/Telco-Customer-Churn.csv')

churn_data <- data[, .(churn_flag = ifelse(Churn == 'Yes', 1, 0), tenure)]
km_curve <- survfit(Surv(tenure, churn_flag) ~ 1, data=churn_data)

# Calculate Survival and Churn Rate
km_curve_df = summary(km_curve)
curve = data.table(time = km_curve_df$time,
                   survival_rate = km_curve_df$surv,
                   churn_rate = km_curve_df$n.event / km_curve_df$n.risk)

# Fit Weibull model
km_curve_fit <- survreg(Surv(tenure, churn_flag) ~ 1, data=churn_data[tenure > 0],
                        dist = 'weibull')
churn_percentage = (1:980) / 1000
survival_tenure <- predict(km_curve_fit, newdata = data.table(x = 1),  
                           p = churn_percentage, type='quantile')

survival_fit <- data.table(survival_tenure = survival_tenure,
                           churn_percentage = churn_percentage)
survival_fit <- survival_fit[survival_tenure >= 1]

# Plot raw survival rate and fitted survival curve
ggplot() + 
  geom_line(aes(x = curve$time,
                y = curve$survival_rate,
                col = 'raw')) + 
  geom_line(aes(x = survival_fit$survival_tenure,
                y = 1 - survival_fit$churn_percentage,
                col = 'fit')) + 
  scale_x_continuous(name = 'cycle', limits = c(0, 500)) + 
  scale_y_continuous(labels = scales::percent, name = 'survival_rate') + 
  theme_minimal()

