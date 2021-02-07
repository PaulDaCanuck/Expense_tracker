# intended to interact with google sheets
# access sheets, transform and create or update sheets

library(googlesheets4)

# read transactions data and basic manipulation

transactions <- 
  read_sheet(ss= "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
             sheet = "transactionLanding", 
             col_types = "_cD_c__ddcc__")

transactions %>% 
  replace_na( list("transactionDebit" = 0, "transactionCredit" = 0)) %>% 
  mutate("transactionBalance" = transactionCredit + transactionDebit) 
  
# read budget sheet

budget <- 
  read_sheet(ss = "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
             sheet = "budgetAnnual", 
             col_types = "cdc")

budget %>%
  mutate("budgetMonthly" = annualValueEst / 12)

# create monthly summary table and compare to budget

monthlyTracking %>% group_by(transactions,)

