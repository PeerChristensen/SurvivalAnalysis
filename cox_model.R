# cox model

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
# Cox model
# ---------------------------------------------------------------------------

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
# ggsurvplot_df(cox_surv_df, color = "start_month",   legend.title = NULL, censor = FALSE)
# ggsurvplot_df(cox_surv_df, color = "start_month",   legend.title = NULL, censor = FALSE,conf.int = T) +
#   facet_wrap(~start_month) +
#   theme(legend.position="none")
