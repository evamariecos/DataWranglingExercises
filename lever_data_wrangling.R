### Eva-Marie Costello - Applying Data Wrangling to your Capstone Project
## I cleaned up data from the Springboard ATS, Lever. Originally the data had ~10,500 rows. Through wrangling the data the data set now has 923 rows with information for only mentor candidates screened during my time as a mentor recruiter (~November 2017 > July 2018)

### Import the data from Lever
data <- read.csv(LeverC.csv)
## Import Tidyr and Dplyr Libraries
library(tidyr)
library(dplyr)

dframe <- LeverC

## Keep only the job postings (referred to as Tags) that contain the word "Mentor"
dframe <- LeverC[grep("Mentor", LeverC$Tags),]

## Remove Tags which mention Mentor Recruiter, Mentor Relationship Manager, Community Manager and Blockchain Mentor (these tags are irrelevant as they are not mentor postings)
dframe1 <- dframe[- grep("Mentor Recruiter", dframe$Tags),]
dframe1 <- dframe1[- grep("Mentor Relationship Manager", dframe1$Tags),]
dframe1 <- dframe1[- grep("Community Manager", dframe1$Tags),]
dframe1 <- dframe1[- grep("Blockchain Mentor", dframe1$Tags),]

## Remove unnecessary columns - Candidate Owner Email, Stage - New Lead, Stage = Reached out
dframe1 = subset(dframe1, select = -c(5,16,17)) 

## To avoid confusion, renaming columns with NA as Posting Archive Reason
dframe1$`Posting Archive Reason`[is.na(dframe1$`Posting Archive Reason`)] <- "Not Stated"
dframe1 <- dframe1[- grep("Not Stated", dframe1$`Posting Archive Reason`),]

## Renaming missing values in Location, Company and Schools column to "Not Stated"
dframe1$Location[is.na(dframe1$Location)] <- "Not Stated"
dframe1$Company[is.na(dframe1$Company)] <- "Not Stated"
dframe1$Schools[is.na(dframe1$Schools)] <- "Not Stated"

## Write file to CSV
write.csv(dframe1, "lever_mentor_clean.csv")



