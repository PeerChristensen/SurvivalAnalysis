# BUDGET MODEL - PRODUCTION SCRIPT
# APRIL 2020
# PEER CHRISTENSEN

library(RODBC)
library(tidyverse)
library(lubridate)
library(forecast)
library(tsibble)
library(openxlsx)
library(emayili)
library(fable)
library(feasts)
library(h2o)
library(survival)
library(survminer)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(zoo)

n_months_predict <- month("31-12-2020") - month(today())

# -----------------------------------------------------
# COX-PH MODEL
# -----------------------------------------------------

df <- read_csv("survival_data.csv") %>%
  mutate(months = ceiling(MembershipDays / 30),
         first = factor(zoo::as.yearmon(StartDate)))

df <- df %>%
  mutate_if(is.character,as.factor) # %>%
#filter(MembershipDays > 30)#%>%
# filter(StartDate >= as.Date("2018-01-01"),
#       StartDate <= today()-364)

cox_mod <- coxph(Surv(months, Cancelled) ~ first, df)

first <- c(levels(df$first))

newdat <- expand.grid(first = first)
rownames(newdat) <- as.character(1:nrow(newdat))

# compute survival curves
cox_sf <- survfit(cox_mod, data = df,newdata = newdat)

# create df with survival curves
cox_surv <- surv_summary(cox_sf)
head(cox_surv)
cox_df <- cbind(cox_surv,
                first = newdat[as.character(cox_surv$strata), ]) %>%
  as_tibble() %>%
  select(time,surv,cohort = strata, first)

# another cox mod for future cohorts
cox_mod_future <- coxph(Surv(months, Cancelled) ~ 1, df)
cox_sf_future <- survfit(cox_mod_future, data = df)
cox_df_future <- surv_summary(cox_sf_future) %>%
  as_tibble() %>%
  select(time,surv)

# -----------------------------------------------------
# FORECASTS
# -----------------------------------------------------

fc_tilgang <- read_csv2("/Users/peerchristensen/Desktop/Projects/forecasts/forecast_data_2020-03-12.csv") %>%
  mutate(M_Buckets = as.numeric(M_Buckets),
         month = yearmonth(month)) %>%
  select(month, Tilgang) %>% 
  as_tsibble() %>%
  fill_gaps()

lambda <- fc_tilgang %>%
  features(Tilgang, features = guerrero) %>%
  pull(lambda_guerrero)

# fit ARIMA model
fit_tilgang <- fc_tilgang %>%
  #filter(Tilgang < mean(Tilgang) +2*sd(Tilgang)) %>% # remove outliers
  #tsibble::fill_gaps() %>% # fill gaps
  model(arima = ARIMA(Tilgang))
#model(arima = ARIMA(box_cox(Tilgang,lambda)))

# 7. calculate forecasts
fc_tilgang <- fit_tilgang %>% 
  forecast(h= n_months_predict+1) %>% # change when updated
  as_tibble() %>%
  select(-.model,-.distribution)

# -----------------------------------------------------
# REGRESSION MODEL
# -----------------------------------------------------

h2o.init()

mod <- h2o.loadModel("model/XGBoost_grid__1_AutoML_20200423_135203_model_4")

newdata <- as.h2o(tibble(time = seq(max(cox_df$time)+1,(max(cox_df$time)+n_months_predict))))

pred <- as_tibble(predict(mod, newdata)) %>% rename(surv = predict)
pred$time = newdata %>% as_tibble() %>% pull(time)

# constant surv .0736
surv_future <- min(pred$surv)

h2o.shutdown(prompt = F)

# -----------------------------------------------------
# COHORTS
# -----------------------------------------------------

dt <- vroom::vroom("cohort_data_2020-04-01.csv")

dt <- lazy_dt(dt)

# create cohorts
df <- dt %>%
  # filter(Customer_Key == 11912164) %>% 
  mutate(date = ymd(EventDate_Key)) %>% 
  filter(date <= floor_date(today(), unit = "months") - 1,
         date >= as.Date("2014-11-01")) %>%
  #filter(date >= floor_date(today()-years(1))) %>%
  #filter(date >= floor_date(today()-months(4))) %>%
  mutate(month = zoo::as.yearmon(date)) %>%
  group_by(Customer_Key) %>%
  mutate(first = min(month)) %>%
  group_by(first, month) %>% 
  summarise(members = n()) %>%
  as_tibble() %>%
  #add_row(first = as.yearmon(today()),month = as.yearmon(today()),members = NA) %>% # add sequence of months
  add_row(first   = as.yearmon(seq(today(),as.Date("2020-12-31"),by="month")),
          month   = as.yearmon(seq(today(),as.Date("2020-12-31"),by="month")),
          members = NA) %>%
  pivot_wider(names_from=month,values_from = members) %>%
  mutate(cohort = row_number()) %>%
  select(first,cohort,everything()) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) #breaks column shifts below

# df <- df %>%
#   mutate(first = as.factor(first),
#          time = rev(cohort)) %>%
#   select(first,time,everything(),-cohort) %>%
#   left_join(cox_df,
#             by= c("first" = "first","time"="time")) %>%
#   select(-contains("."),-upper,-lower,-strata)

# -----------------------------------------------------
# COMBINE DATA
# -----------------------------------------------------

# 1. add forecasts
col <- ncol(df) - n_months_predict
row <- nrow(df) - n_months_predict

for (i in round(fc_tilgang$Tilgang)) {
  
  df[row, col] <- i
  
  row = row + 1
  col = col + 1
}

# 2. get start values for each cohort

cohort_start_values <- tibble()
col <- 3
for (i in 1:(nrow(df))) {
  
  cohort = i
  start_value = df[[col]][i]
  
  col = col + 1
  
  d <- cbind(cohort,start_value)
  cohort_start_values <- rbind(cohort_start_values,d)
}

# 3. insert survival predictions from ml model - upper

col <- ncol(df)-n_months_predict
for (i in 1:n_months_predict) {
  
  df[i,col:ncol(df)] = round(cohort_start_values$start_value[i] * surv_future)
  col = col + 1
}

# 4. add survival values based on cox_df strata
cox <- cox_df

cohort_start_values <- cohort_start_values %>% filter(cohort != 1)

col <- ncol(df) - n_months_predict
row <- 2
cols_to_fill <- c(1:(n_months_predict+1),
                  rep(n_months_predict+1,(nrow(df)-(2*n_months_predict+3))))
counter <- 1
for (i in cols_to_fill) {
  
  cox_subset <- cox %>% filter(cohort == row, time > max(time)-i)
  start_val = cohort_start_values$start_value[counter]
  df[row,seq(col,col+i-1)] = round(start_val * cox_subset$surv[1:i])
  
  if (counter == length(cols_to_fill)) {
    df[row,seq(col,col+i-1)] = round(start_val * cox_df_future$surv[1:i])
    
  }
  row = row + 1
  counter = counter + 1
  
  if (i == max(cols_to_fill)) {
    cox <- cox %>% filter(time != max(time))
  }
}

# 5. add survival values -lower part based on cox_df_future
col <- ncol(df)-n_months_predict+1
row <- nrow(df)-(n_months_predict) # change later

cols_to_fill <- seq(n_months_predict,1)
counter = 1
for (i in cols_to_fill) { 
  
  df[row,seq(col,col+i-1)] = cox_df_future$surv[1:i]
  
  col = col + 1
  row = row + 1
}

# -----------------------------------------------------
# Totals
# -----------------------------------------------------

df <- df %>% janitor::adorn_totals("row")
df[nrow(df),1] <- "Total"
df[nrow(df),2] <- NA

df <- df %>% 
  as_tibble()

# -----------------------------------------------------
# Output
# -----------------------------------------------------

write_csv2(df,glue::glue("premium_kohorter_alle_{today()}.csv"))


