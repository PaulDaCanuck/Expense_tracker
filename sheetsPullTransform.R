# intended to interact with google sheets
# access sheets, transform and create or update sheets

library(googlesheets4)
library(googledrive)
library(lubridate)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(forcats)

theme_set(theme_minimal())

# read transactions data and basic manipulation
transactions <- 
  read_sheet(ss= "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
             sheet = "transactionLanding", 
             col_types = "_cD____ddcc_") %>% 
  replace_na( list("transactionDebit" = 0, "transactionCredit" = 0)) %>% 
  mutate("transactionBalance" = transactionCredit + transactionDebit) %>%
  mutate("trDate" = format(as.Date(transactionDate), "%Y-%m"))  %>%
  within(rm(transactionCredit, transactionDebit, transactionDate))
  
# read budget sheet
budget <- 
  read_sheet(ss = "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
             sheet = "budgetAnnual", 
             col_types = "cdc")  %>%
  mutate("budgetMonthly" = if_else(transactionTypeCat == "Income", annualValueEst, -1 * annualValueEst) / 12)

# create monthly summary table grouped by transaction categories and month  
# monthlyTrackingDetail includes subType

monthlyTracking <- transactions %>%
  group_by(trDate, transactionType) %>%
  summarise(monthlyAmt = sum(transactionBalance)) 

monthlyTrackingDetail <- transactions %>%
  group_by(trDate, transactionType, transactionSubType) %>%
  summarise(monthlyAmt = sum(transactionBalance))  

# calculate the number of months for which there is data
elapsedMonths <- as.numeric(length(unique(monthlyTracking$trDate)))

# identify the last month of data
lastMonth = max(transactions$trDate)

# vector of colours used to fill bars based on % of monthly budget spent
my_colours <- c("green", "blue", "orange") 

# merge and manipulate monthly transactions with budget 
monthlyCompare <- merge(x = monthlyTracking, y = budget, by = "transactionType") %>%
  filter(transactionTypeCat == "Expense") %>% 
  filter(transactionType != "Misc") %>%
  within(rm(annualValueEst, transactionTypeCat)) %>%
  mutate("absDiff" = monthlyAmt + budgetMonthly) %>%
  mutate("pctDiff" = -100 * (1 - ((monthlyAmt + budgetMonthly) / budgetMonthly))) 


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

# month vis % diffences from budget in facet by month
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
    
# ytd df similar to monthly
ytdCompare <- merge(x = monthlyTracking, y = budget, by = "transactionType") %>%
  filter(transactionType != "Misc") %>%
  group_by(transactionType) %>%
  summarise(ytdAmount = sum(monthlyAmt), meanBudget = mean(budgetMonthly)) %>%
  mutate("ytdBudget" = meanBudget * elapsedMonths) %>%
  mutate("ytdPctDiff" = -100 * (1 - ((ytdAmount + ytdBudget) / ytdBudget))) %>%
  arrange(abs(ytdBudget)) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(transactionType = factor(transactionType, levels=transactionType))  

# create breaks in a categorical variable to help with colouring the charts
ytdCompare$pctDiffCat <- cut(ytdCompare$ytdPctDiff,
                                 breaks=c(-Inf, 75, 125, Inf),
                                 labels=c("low","medium","high"))


## ytd vis

# subtitle label identifying the number of months and last months
stLT <- paste(elapsedMonths, "months up to", lastMonth, sep=" " )

# label for bars in YTD chart
ytdPDlabel <- percent(ytdCompare$ytdPctDiff / 100, accuracy = 1)

ytdPCtVis <- ggplot(ytdCompare) +
  geom_hline(yintercept = 100, colour = "grey", linetype = "longdash") +
  geom_bar(aes(x=transactionType, y=ytdPctDiff, fill=pctDiffCat), stat='identity', show.legend = FALSE) +
  geom_text(aes( 0, 100, label = "100%", vjust = 0, hjust = -0.1), size = 3, colour = "grey") +
  geom_text(aes(x=transactionType, y=1, label= ytdPDlabel), vjust=0.3, colour="white", size=4, fontface=2, hjust=0) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(),
        plot.title.position = "plot") +  # left justify title
  labs(title = "Year-to-date Expenses to Budget",
       subtitle = stLT,
       y = "Percent Difference", 
       x = "") + 
  scale_fill_manual(values = my_colours) + 
  coord_flip()
ytdPCtVis


# write dfs back to google sheets
write_sheet(monthlyCompare, ss= "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
            sheet = "monthlyCompare")
write_sheet(ytdCompare, ss= "1rsdSUAK7aN5_XxvlgcxuEpgyJNhbN8QJdAzJDxLhKbQ", 
            sheet = "ytdCompare")
