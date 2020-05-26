# cohorts

library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(zoo)

dt <- vroom::vroom("cohort_data_2020-04-01.csv")

dt <- lazy_dt(dt)

# -------------------------------------------------------------------
# method 1
# -------------------------------------------------------------------
df <- dt                             %>%
  #filter(Customer_Key == 940318 ) %>% 
  select(Customer_Key,EventDate_Key)   %>%
  mutate(date = ymd(EventDate_Key))   %>%
  #filter(date >= floor_date(today()-years(1))) %>%
  mutate(month = floor_date(date,unit = "month")) %>%
  group_by(Customer_Key)                    %>%
  mutate(cohortMonth = min(month)) %>%
  mutate(cohortIndex = round(as.numeric((as.yearmon(month) - as.yearmon(cohortMonth)) * 12) + 1),0) %>%
  ungroup()

retention_table <- df                       %>%
  group_by(cohortMonth,cohortIndex)         %>%
  summarise(n = length(unique(Customer_Key))) %>%
  ungroup()                                 %>%
  as_tibble() %>%
  pivot_wider(names_from = cohortIndex, values_from = n)

cohort_sizes <- retention_table$`1`

#for (col in 1:ncol(retention_rate)) {
#    retention_rate[,col] = (retention_rate[,col] / cohort_sizes) * 100
#  }

# calculate percentages for each cohort

retention_rate <- retention_table  %>%
  select(-cohortMonth)             %>%
  apply(2,function(x) round((x / cohort_sizes) * 100,0)) %>%
  as_tibble()                      %>%
  mutate(cohortMonth = retention_table$cohortMonth) %>%
  select(cohortMonth, everything())

# melt data frame

retention_rate_melt <- retention_rate   %>%
  melt(id.vars = 'cohortMonth',na.rm=T) %>%
  mutate(date_col    = glue::glue("{months(cohortMonth,abbreviate = T)} {year(cohortMonth)}"),
         cohortMonth = as.numeric(cohortMonth))

# # plot retention heatmap
# 
# retention_rate_melt     %>%
#   filter(variable != 1,value>0) %>% # remove 1st month
#   ggplot(aes(x = variable, y = reorder(cohortMonth,desc(cohortMonth)))) +
#   geom_raster(aes(fill = log(value))) +
#   coord_equal(ratio = 1) +
#   geom_text(aes(label = glue::glue("{round(value,0)}%")), size = 3, color = "snow") +
#   scale_y_discrete(breaks = retention_rate_melt$cohortMonth,
#                    labels = retention_rate_melt$date_col) +
#   theme_light() +
#   theme(axis.text    = element_text(size = 14),
#         axis.title   = element_text(size = 16),
#         panel.grid   = element_blank(),
#         panel.border = element_blank()) +
#   labs(y= "cohort start month",
#        x = "month #") +
#   guides(fill=FALSE)
# 
# # retention line chart
# 
# retention_rate_melt %>% 
#   filter(value>0) %>%
#   rename(Month = variable) %>%
#   ggplot(aes(x=Month,y=value, colour = factor(date_col), group=date_col)) +
#   geom_line(size=1.5, alpha=.8) +
#   geom_point(size=1.5, alpha=.8) +
#   theme_light() +
#   theme(axis.text  = element_text(size = 14),
#         axis.title = element_text(size = 16))

# -------------------------------------------------------------------
# method 2
# -------------------------------------------------------------------

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
  add_row(first = as.yearmon(today()),month = as.yearmon(today()),members = NA) %>%
  pivot_wider(names_from=month,values_from = members) %>%
  mutate(cohort = row_number()) %>%
  select(first,cohort,everything()) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) #breaks column shifts below

#new_col <- ncol(df) -2

cox_subset <- cox_df %>% 
 # filter(time <= new_col) %>%
  mutate(surv = rev(surv)) %>%
  select(cohort = time, surv)

df <- df %>% 
  inner_join(cox_subset) 

col <- 3
row <- 1
for (i in 1:(nrow(df)-1)) {
 
  df[i,ncol(df)-1] = df[row,col] * df[i,ncol(df)]
  row = row + 1
  col = col +1
}

df <- df %>% select(-surv)

# med tilgang
df[nrow(df),ncol(df)] <- 3408 # fra forecast script (1 md)

df[,ncol(df)] <- round(df[,ncol(df)]) # afrund estimat

# indsæt række med totaler
df <- df %>% janitor::adorn_totals("row")
df[nrow(df),1] <- "Total"
df[nrow(df),2] <- NA

df <- df %>% 
  as_tibble()

# small output 

df_small <- df %>% 
  filter(cohort >= max(df$cohort,na.rm=T)-12 | is.na(cohort)) %>%
  select(first,
         as.character(as.yearmon(today()-months(12))):as.character(as.yearmon(today()))) %>%
  rename(kohorte = first)

write_csv2(df_small,glue::glue("premium_kohorter_{today()}.csv"))
write_csv2(df,glue::glue("premium_kohorter_alle_{today()}.csv"))

# shift columns
shift <- df %>% as.data.frame(df[-ncol(df),]) #create new data frame
totcols <- ncol(shift) #count number of columns in data set
for (i in 1:nrow(shift)) { #for loop for shifting each row
  d <- shift[i,] #select row from data frame
  d <- d[ , !is.na(d[])] #remove columns with zeros
  partcols <- ncol(d) #count number of columns in row (w/o zeros)
  #fill columns after values by zeros
  if (partcols < totcols) d[, c((partcols+1):totcols)] <- 0
  shift[i,] <- d #replace initial row by new one
}

# percentages

x <- shift[,c(3:ncol(shift))]
y <- shift[,3]

retention <- apply(x, 2, function(x) round(x/y * 100,1))
retention <- data.frame(cohort=df$first, retention)
retention <- retention[-nrow(retention),]
retention <- as_tibble(retention)

names(retention)[2:ncol(retention)] <- 0:(ncol(retention)-2)

retention <- retention %>% 
  pivot_longer(-cohort) %>%
  mutate(name = as.numeric(name)) %>%
  filter(value != 0)

names(retention) <- c('cohort', 'month', 'retention')

mean_retention <- retention %>%
  group_by(month) %>%
  summarise(m = mean(retention)) %>%
  mutate(month = as.numeric(month)) 

retention %>% 
  ggplot(aes(x=month,y=retention, colour = factor(cohort), group=factor(cohort))) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  ylim(c(0,100)) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = "none") +
  scale_colour_viridis_d(option="A", alpha=.8,name = "cohort") #+
  #facet_wrap(~year(retentionRate$cohort),scales="free",ncol=1)

mean_retention %>%
  ggplot(aes(month,m)) +
  geom_line()

# retention table
retention %>%
  filter(month>0) %>%
  ggplot(aes(x = as.numeric(month), y = reorder(cohort, desc(cohort)))) +
  geom_raster(aes(fill = log(retention))) +
  coord_equal(ratio = 1) +
  geom_text(aes(label = glue::glue("{round(retention,0)}%")), size = 2, color = "snow") +
  scale_fill_gradient(low="#330425",high="#C83488", guide=F) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  labs(y= "cohort")


# tic()
# d2 <- df %>%
#   group_by(Customer_Key) %>%
#   mutate(first = min(month)) %>%
#   group_by(first, month) %>% 
#   summarise(members = n()) %>%
#   spread(month, members) %>%
#   ungroup() 
# toc() 
# beep("mario") # 1072 sec
# 
# d <- d %>%
#   mutate(first = glue::glue("{1:nrow(d)}_{first}")) 




