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
> dframe1$Location[is.na(dframe1$Location)] <- "Not Stated"
> dframe1$Company[is.na(dframe1$Company)] <- "Not Stated"
> dframe1$Schools[is.na(dframe1$Schools)] <- "Not Stated"

## Create New Column which removes schools names duplicates and add it to data frame
Schools_nodups <- sapply(dframe1$Schools, function(Schools) paste(unique(unlist(strsplit(Schools,", "))), collapse = ", "))
dframe1 <- cbind(dframe1, Schools_nodups)

## Count no.of schools applicant states and add as new column
school_no <- sapply(strsplit(as.character(dframe1$Schools_nodups),","),FUN=function(x){length(x[x!="Null"])})
dframe1 <- cbind(dframe1, school_no)

## Remove previous School column (as it contains duplicates)
dframe1 = subset(dframe1, select = -c(3)) 

## If Profile Archive Reason does not equal "Hired" then change all other reasons to "Not Hired"
## New column 
Hired <- 
  
## Categorize location by country to understand distribution
### Install Maps package and world.cities data
  install.packages("maps")
data(world.cities)
aa <- c(dataf5$Location)
## Remove punctuation 
ab <- gsub( ',', '', dataf5$Location)
# Split data at word boundaries
abc <- strsplit(ab, " ")

# Match on country in world.countries

# Match on cities in world.cities

## What if a city is found but a country isn't, one can look up the country for the found city in world.cities

## Sub all categories except "Hired" as "Not Hired" in Profile.Archive.Reason column
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Experience Does Not Match Needs",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Passive/Unresponsive",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Poor Communication Skills",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Timing",replacement="Not Hired")) 
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Withdrew",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Underqualified",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Recruiterbox - On Hold for Future",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Geography",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Failed Screening Test",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Recruiterbox archive",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Poor problem Solving / Aptitude",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Position Filled",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Failed Skills Assessment",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Compensation",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Poor Techincal Skills",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Overqualified",replacement="Not Hired"))
dataf5$Profile.Archive.Reason <-as.data.frame(sapply(dataf5$Profile.Archive.Reason,gsub,pattern="Did not pass skills assessment",replacement="Not Hired"))

data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Experience Does Not Match Needs",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Passive/Unresponsive",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Poor Communication Skills",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Timing",replacement="Not Hired")) 
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Withdrew",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Underqualified",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Recruiterbox - On Hold for Future",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Geography",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Failed Screening Test",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Recruiterbox archive",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Poor problem Solving / Aptitude",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Position Filled",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Failed Skills Assessment",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Compensation",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Poor Techincal Skills",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Overqualified",replacement="Not Hired"))
data$Profile.Archive.Reason <-as.data.frame(sapply(data$Profile.Archive.Reason,gsub,pattern="Did not pass skills assessment",replacement="Not Hired"))



## Upload list of states as CSV file 
U.S_States <- read.csv("~/Downloads/U.S_States.csv", header=FALSE)
View(U.S_States)

## Add new column called country - where country of applicant is placed
data123["country"] <- NA

## If candidate's 'location' is present in U.S states column
## add all location values to lowercase
dataf5 <-mutate(dataf5,Location=tolower(Location))

dataf5$country <- ifelse(grepl("alabama", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("al", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("alaska", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ak", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("arizona", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("az", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("arkansas", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ar", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("california", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ca", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("colorado", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("co", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("connecticut", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ct", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("delaware", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("de", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("district of columbia", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("dc", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("florida", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("fl", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("georgia", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ga", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("hawaii", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("hi", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("idago", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("idaho", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("id", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("illinois", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("il", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("indiana", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("in", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("iowa", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ia", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("kansas", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ks", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("kentucky", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ky", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("louisiana", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("la", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("maine", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("me", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("maryland", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("md", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("massachusetts", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ma", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("michigan", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("mi", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("minnesota", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("mn", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("mississippi", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ms", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("missouri", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("mo", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("montana", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("mt", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("nebraska", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ne", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("nevada", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("nv", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("new hampshire", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("nh", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("new jersey", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("nj", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("new mexico", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("new york", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("nm", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ny", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("north carolina", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("nc", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("nd", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("north dakota", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ohio", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("oh", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("oklahoma", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ok", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("oregon", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("or", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("pennsylvania", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("pa", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("rhode island", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ri", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("south carolina", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("sc", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("south dakota", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("sd", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("tennessee", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("tn", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("texas", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("tx", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("utah", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("ut", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("vermont", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("vt", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("virginia", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("va", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("washington", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("wa", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("west virginia", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("wv", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("wisconsin", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("wi", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("wyoming", dataf5$Location), "United States", "NA")
dataf5$country <- ifelse(grepl("wy", dataf5$Location), "United States", "NA")



## Write file to CSV
write.csv(dframe1, "lever_mentor_clean.csv")



