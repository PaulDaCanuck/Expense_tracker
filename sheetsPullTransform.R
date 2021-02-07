# intended to interact with google sheets
# access sheets, transform and create or update sheets

library(googlesheets4)
library(lubridate)
library(dplyr)
library(tidyr)

# read transactions data and basic manipulation

transactions <- 
  read_sheet(ss= "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
             sheet = "transactionLanding", 
             col_types = "_cD____ddcc__") %>% 
  replace_na( list("transactionDebit" = 0, "transactionCredit" = 0)) %>% 
  mutate("transactionBalance" = transactionCredit + transactionDebit) %>%
  mutate("trDate" = format(as.Date(transactionDate), "%Y-%m")) %>%
  within(rm(transactionCredit, transactionDebit, transactionDate))
  
# read budget sheet

budget <- 
  read_sheet(ss = "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
             sheet = "budgetAnnual", 
             col_types = "cdc")

budget %>%
  mutate("budgetMonthly" = annualValueEst / 12)

# create monthly summary table and compare to budget

monthlyTracking <- transactions %>%
  group_by(trDate, transactionType, transactionSubType) %>%
  summarise(monthlyAmt = sum(transactionBalance)) %>%
  pivot_wider(names_from = trDate,values_from = monthlyAmt, values_fill= 0)

