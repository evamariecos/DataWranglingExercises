# DS1 Data Wrangling Exercise 1 - Eva-Marie Costello
## Load the data into R
## 1. Clean Up the Brand Names 
df<-mutate(df,company=tolower(company)) ## put all company names to lower
### Fix spelling mistakes

df<-as.data.frame(sapply(df,gsub,pattern="phillips",replacement="philips"))
df<-as.data.frame(sapply(df,gsub,pattern="phlips",replacement="philips"))
df<-as.data.frame(sapply(df,gsub,pattern="fillips",replacement="philips"))
df<-as.data.frame(sapply(df,gsub,pattern="unilver",replacement="unilever"))
df<-as.data.frame(sapply(df,gsub,pattern="akz0",replacement="akzo"))
df<-as.data.frame(sapply(df,gsub,pattern="ak zo",replacement="akzo"))

### 2. Seperate Product Code and Number
df <- separate(df, Product.code...number,c("product_code","product_number"),sep = "-")

### 3. Add product Categories
df["product_category"] <- NA

df$product_category[df$product_code=="v"] <- "tv"  
df$product_category[df$product_code=="p"] <- "smartphone"  
df$product_category[df$product_code=="x"] <- "laptop"  
df$product_category[df$product_code=="q"] <- "tablet"  

### 4. Add full address for Geocoding

df["full_address"] <- NA
df<-unite(df,"full_address",address,city,country,sep = ",")

### 5. Create dummy variables for company and product category
### Create columns

df$company_philips <- 0
df$company_azko <- 0
df$company_van_houten <- 0
df$company_unilever <- 0

df$product_smartphone <- 0
df$product_tv <- 0
df$product_laptop <- 0
df$product_tablet <- 0

### Create Binary Columns
df$company_philips[df$company =="philips"] <- 1
df$company_azko[df$company =="azko"] <- 1
df$company_van_houten[df$company =="van houten"] <- 1
df$company_unilever[df$company =="unilever"] <- 1
df$product_smartphone[df$product =="smartphone"] <- 1
df$product_tv[df$product =="tv"] <- 1
df$product_tv[df$product =="TV"] <- 1
df$product_laptop[df$product =="Laptop"] <- 1
df$product_tablet[df$product =="Tablet"] <- 1

### Write CSV file 
write.csv(df, file = "EMC_DS1_1.csv")