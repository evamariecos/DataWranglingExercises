# Data Wrangling Exercise 2 - Cleaning Titanic Data
library(tidyr)
library(dplyr)
## 1. Change missing values in embarked column to S
titanic_df <- tbl_df(titanic3)
titanic_df$embarked[is.na(titanic_df$embarked)] <- "S" ##replace missing values with S

## 2. Replace missing values in the Age column with the mean of the Age column
titanic_df$age[is.na(titanic_df$age)] <- mean(titanic_df$age, na.rm = TRUE)

## 3. Fill missing values in the boat column with None
titanic_df$boat[is.na(titanic_df$boat)] <- "None"

## 4. Add a new column has_cabin_number which is 1 if there is a cabin number, 0 if not
titanic_df <- titanic_df %>% 
  mutate(has_cabin_number = ifelse(is.na(titanic_df$cabin), 0, 1))

## 5. Write file to CSV 
write.csv(titanic_df, "titanic_clean.csv")


