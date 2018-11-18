#Data Story - Your Capstone Project Milestone Report

# Eva Marie Costello - Sept 3
## Optimizing the resume screen to reduce time spent scanning and ensure priortization of candidates who are of most likelihood to fit the hiring criteria. 

## An introduction to the problem:

### For my capstone project I focused on reducing the effort and time spent scanning resumes for my current role. 
### Currently, the time spent scanning resumes takes ~20 seconds per candidate. My current role requires me to hire ~25 candidates per month. 
### The recruiting funnel requires me to screen 10 candidates for every hire I make. Therefore  the time spent monthly to screen candidates is 10 secs x 20 x 25 = 83.3 hrs
### The aim of my capstone project is to significantly reduce this screening time by building a model which prioritized interviewing certain new applicants based on historical hiring data from the last 12 months.

## A deeper dive into the data set:

### The data was collected through the applicant tracking software we use at my employer. I added it to R in the format I was given it in.

### Import the data from Lever
data <- read.csv(LeverC.csv)
## Import Tidyr and Dplyr Libraries
library(tidyr)
library(dplyr)

dframe <- LeverC

### This data set included alot of information about the candidate which had many redundent columns. The dataset also had information on about 10,000 candidates and only ~900 were of interest to this analysis.
### My first task was to keep only the job postings (referred to as Tags) that contain the word "Mentor". This word was included in the job postings that are of interest in this study (which has around ~900 candidates). Below is the code I used to eliminate all jobs other than those with "Mentor"

## Remove Tags which mention Mentor Recruiter, Mentor Relationship Manager, Community Manager and Blockchain Mentor (these tags are irrelevant as they are not mentor postings)
dframe <- LeverC[grep("Mentor", LeverC$Tags),]
dframe1 <- dframe[- grep("Mentor Recruiter", dframe$Tags),]
dframe1 <- dframe1[- grep("Mentor Relationship Manager", dframe1$Tags),]
dframe1 <- dframe1[- grep("Community Manager", dframe1$Tags),]
dframe1 <- dframe1[- grep("Blockchain Mentor", dframe1$Tags),]

### As mentioned, the data had alot of redundant columns and below is how I removed these.

## Remove unnecessary columns - Candidate Owner Email, Stage - New Lead, Stage = Reached out
dframe1 = subset(dframe1, select = -c(5,16,17)) 

### Another step in cleaning the data was renaming the NA values as Not Stated, in many of the columns. Below is how I did that: 

## Renaming missing values in Location, Company and Schools column to "Not Stated"
dframe1$Location[is.na(dframe1$Location)] <- "Not Stated"
dframe1$Company[is.na(dframe1$Company)] <- "Not Stated"
dframe1$Schools[is.na(dframe1$Schools)] <- "Not Stated"
dframe1$`Posting Archive Reason`[is.na(dframe1$`Posting Archive Reason`)] <- "Not Stated"
dframe1 <- dframe1[- grep("Not Stated", dframe1$`Posting Archive Reason`),]


### For my model, my hypothesis is that I can run an ML model on information about the candidate. In partilcuar the data of most interest is their school/university name, number of schools/university they attended, location of the candidate and if they were hired or not.
### Later on I'd like to gather more information on the 'rankings' of the school's the candidate attended, which will help me to categorize if the school was ranked as a school which I was of a high probability to hire a candidate from. Firstly I wanted to just pull the info about the schools. See below:
### There was alot of school duplicates in the data, I removed those below and created a new column which counted the number of schools.

## Create New Column which removes schools names duplicates and add it to data frame
Schools_nodups <- sapply(dframe1$Schools, function(Schools) paste(unique(unlist(strsplit(Schools,", "))), collapse = ", "))
dframe1 <- cbind(dframe1, Schools_nodups)

## Count no.of schools applicant states and add as new column
school_no <- sapply(strsplit(as.character(dframe1$Schools_nodups),","),FUN=function(x){length(x[x!="Null"])})
dframe1 <- cbind(dframe1, school_no)

## Remove previous School column (as it contains duplicates)
dframe1 = subset(dframe1, select = -c(3)) 


### Next step was to clean the column which indicated if an applicant was hired or not.
### So if Profile Archive Reason does not equal "Hired" then change all other reasons to "Not Hired" (instead of all the other reasons e.g geography, compensartion)

dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Experience Does Not Match Needs",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Passive/Unresponsive",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Poor Communication Skills",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Timing",replacement="Not Hired")) 
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Withdrew",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Underqualified",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Recruiterbox - On Hold for Future",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Geography",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Failed Screening Test",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Recruiterbox archive",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Poor problem Solving / Aptitude",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Position Filled",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Failed Skills Assessment",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Compensation",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Poor Techincal Skills",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Overqualified",replacement="Not Hired"))
dframe1$Profile.Archive.Reason <-as.data.frame(sapply(dframe1$Profile.Archive.Reason,gsub,pattern="Did not pass skills assessment",replacement="Not Hired"))

### Next I wanted to clean up the applicants location. The location column was a mixture of counties, states and countries and I wanted to categorize the data into states/major cities and then create a column which indicated what country this state/or major city was in.
### Firstly, I updated a dataset which listed all U.S states.

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

## In Sources column, choose only one channel. Firstly remove 'job site', then comma 
data$Sources <- gsub('Job site', '', data$Sources)


a <- dataf5

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#removing any special character
a$Tags <- gsub("[[:punct:]]", "", a$Tags)

#apply trim function 
a$Tags <- gsub("\\s+", " ", trim(a$Tags))
a$Tags <- trim(a$Tags)

#Create a data.frame with all the course names
courses <- data.frame(courses = c("Cybersecurity Mentor",
                                  "Digital Marketing Course Mentor",
                                  "UX Design Course Mentor",
                                  "Business Analytics Course Mentor",
                                  "Data Science Course Mentor",
                                  "Data Analytics for Business Course Mentor"))

a$Tags <- as.character(a$Tags)
courses$courses <- as.character(courses$courses)
b$institution <-as.character(b$institution)
a$Names.of.Universities.Attended <- as.character(a$Names.of.Universities.Attended
)

#Library for fuzzy join
install.packages("fuzzyjoin")
library(fuzzyjoin)

# Add in data of world university rankings 'UniData.csv'
b <- read.csv("UniData.csv")
b$world_rank <- NULL

#Left fuzzy join for merging the courses.
a <- a %>% regex_left_join(courses, by = c(Tags = "courses"))

#Left fuzzy join for merging the top university list.
a <- a %>% regex_left_join(b, by = c(Names.of.Universities.Attended = "institution"))

write.csv(a, "abc.csv")






### For my data I have yet to get distribution info of the countries from the candidate pool. 
### In addition to getting this distribution I plan on get an evenly correlated distribution of highest ranked schools in the countries that applicants came from.
### With the data full cleaned I plan to build a model which will prioritize applicants that I should interview based on the previous hiring data.


## Categorize location by country to understand distribution

