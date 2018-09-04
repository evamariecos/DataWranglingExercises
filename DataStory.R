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

### For non U.S residents I used a data set called world cities, which included information on cities globally and the countries these cities corresponded to.
## Categorize location by country to understand distribution
### Install Maps package and world.cities data
install.packages("maps")
data(world.cities)
aa <- c(dataf5$Location)
## Remove punctuation 
ab <- gsub( ',', '', dataf5$Location)
# Split data at word boundaries
abc <- strsplit(ab, " ")

### For my data I have yet to get distribution info of the countries from the candidate pool. 
### In addition to getting this distribution I plan on get an evenly correlated distribution of highest ranked schools in the countries that applicants came from.
### With the data full cleaned I plan to build a model which will prioritize applicants that I should interview based on the previous hiring data.


## Categorize location by country to understand distribution

