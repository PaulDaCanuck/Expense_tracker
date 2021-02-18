# intended to interact with google sheets
# access sheets, transform and create or update sheets

library(googlesheets4)
library(googledrive)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal())

# read transactions data and basic manipulation

transactions <- 
  read_sheet(ss= "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
             sheet = "transactionLanding", 
             col_types = "_cD____ddcc") %>% 
  replace_na( list("transactionDebit" = 0, "transactionCredit" = 0)) %>% 
  mutate("transactionBalance" = transactionCredit + transactionDebit) %>%
  mutate("trDate" = format(as.Date(transactionDate), "%Y-%m")) %>%
  within(rm(transactionCredit, transactionDebit, transactionDate))
  
# read budget sheet

budget <- 
  read_sheet(ss = "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
             sheet = "budgetAnnual", 
             col_types = "cdc")  %>%
  mutate("budgetMonthly" = annualValueEst / 12)

# create monthly summary table grouped by transaction categories and month  Detail df includes subType

monthlyTracking <- transactions %>%
  group_by(trDate, transactionType) %>%
  summarise(monthlyAmt = sum(transactionBalance)) 


# calculate the number of months for which there is data

elapsedMonths <- as.numeric(length(unique(monthlyCompare$trDate))) 


# similar monthly summary table to monthlyTracking but preserving transactionSubType 

monthlyTrackingDetail <- transactions %>%
  group_by(trDate, transactionType, transactionSubType) %>%
  summarise(monthlyAmt = sum(transactionBalance))  


# merge and manipulate monthly transactions with budget 

monthlyCompare <- merge(x = monthlyTracking, y = budget, by = "transactionType") %>%
  filter(transactionTypeCat == "Expense") %>%
  within(rm(annualValueEst, transactionTypeCat)) %>%
  mutate("absDiff" = monthlyAmt + budgetMonthly) %>%
  mutate("pctDiff" = 100 * (1 - ((monthlyAmt + budgetMonthly) / budgetMonthly)))


# create breaks in a categorical variable to help with colouring the charts

monthlyCompare$pctDiffCat <- cut(monthlyCompare$pctDiff,
                     breaks=c(-Inf, 75, 125, Inf),
                     labels=c("low","medium","high"))

# pivot differences to compare by month

monthlyAbsDiff <- monthlyCompare %>%
  within(rm(monthlyAmt, pctDiff)) %>% 
  pivot_wider(names_from = trDate, values_from = "absDiff", values_fill = 0)

monthlyPctDiff <- monthlyCompare %>%
  within(rm(monthlyAmt, absDiff)) %>%
  pivot_wider(names_from = trDate, values_from = "pctDiff", values_fill = 0)

# visualize monthly % diffences from budget in facet by month

my_colours <- c("green", "blue", "orange") # vector of colours used to fill bars based on % of monthly budget spent

bc <- ggplot(monthlyCompare) +
  geom_hline(yintercept = 100, colour = "grey", linetype = "longdash") +
  geom_bar(aes(x=transactionType, y=pctDiff, fill=pctDiffCat), stat='identity', show.legend = FALSE) +
  geom_text(aes( 0, 100, label = "100%", vjust = -1, hjust = -0.1), size = 3, colour = "grey") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  facet_wrap(~ trDate) +
  labs(title = "Comparing Expenses to Budget", y = "Percent Difference", x = "") + 
  scale_fill_manual(values = my_colours) + 
  coord_flip()
bc 
    
# visualize YTD spending against budget

ytdCompare <- merge(x = monthlyTracking, y = budget, by = "transactionType") %>%
  group_by(transactionType) %>%
  summarise(ytdAmount = sum(monthlyAmt), meanBudget = mean(budgetMonthly)) %>%
  mutate("ytdBudget" = meanBudget * elapsedMonths) %>%
  mutate("ytdPctDiff" = abs(100 * (1 - ((ytdAmount + ytdBudget) / ytdBudget))))



