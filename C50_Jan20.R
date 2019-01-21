## C5.0 model - December 9
C5.0(a, a$`Candidate Hired?`, trials = 1, rules = FALSE, weights = NULL,
     +      control = C5.0Control(), costs = NULL, ...)

## install c5.0 
install.packages("C50")
## Create training data set, a = dataframe
b <- a[1:700,]
View(b)
b <- training_data
## Moving hired column to front so it can be Y
training_data <- b
d <- b$`Candidate Hired?`
View(d)
## Combine b and d
cbind(b, d)
View(b)
df <- cbind(d, b)
View(df)
## Get sample selection of rows
a <- a[ sample( nrow( a ) ), ]
## Remove unnecessary columns, that won't be used in model
df = subset(df, select = -c(2,3,4,5,6,10,13,14,15)) 
training <- df
## Remove commas from Location column (as it caused an error when model was previously run)
location_nocomma <- gsub( ',', "", df$Location)
training <- cbind(df, location_nocomma)
## Delete old location column
 training = subset(training, select = .c(1))
## Create training and testing data 
X <- training1[,2:5]
Y <- training1[,1]
trainX <- X[1:600,]
trainy <- y[1:600]
testX <- X[601:700,]
testy <- y[601:700]
## Run c5.0 model
model <- C50::C5.0( trainX, trainy )
## See summary of model
summary( model )
