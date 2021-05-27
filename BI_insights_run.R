# BI customer Insights script

library(tidyverse)
library(DBI)
library(odbc)

con <- dbConnect(odbc(), driver = "SQL Server", server = "saxo034", 
                 database = "EDW", UID = "R", PWD = "sqlR2017", Port = 1433)


df <- dbGetQuery(con,"select top 100000 * from EDW.edw.OrderFactCombined")

customers <- df %>%
  distinct(Customer_Key) %>%
  pull()

new_dat <- NULL
count   <- 1

for (i in customers) {
  
  new_dat$customer <- i
  Sys.sleep(10)
  if (count == 1 | count %% 100 == 0) {
    print("processing..")
  }
  
}
