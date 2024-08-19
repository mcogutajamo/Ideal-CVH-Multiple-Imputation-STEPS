library(haven)
library(data.table)
library(tidyverse)
library (foreign)

library(psych)### For tabulating descriptive statistics into a dataframe using describe function

# Importing the data ------------------------------------------------------

setwd ("U:/ManWin/My Documents/James Oguta/Manuscripts/Ideal Cardiovascular Health/Revision 1/Imputation")

data <- read_dta("subset_imput_ICVH.dta")
data <- as.data.table(data)


# Browsing the data -------------------------------------------------------


str(data)
head(data)
glimpse (data)



# Transforming variables-numeric --------------------------------------------------
data$X1 <- as.numeric(data$X1)
data$X2a <- as.numeric(data$X2a)
data$residence <- as.numeric(data$residence)
data$region <- as.numeric(data$region)
data$sex <- as.numeric(data$sex)
data$ethnicity2 <- as.numeric(data$ethnicity2)
data$agegrps <- as.numeric(data$agegrps)
data$maritalstatus <- as.numeric(data$maritalstatus)
data$education <- as.numeric(data$education)
data$occupation <- as.numeric(data$occupation)
data$occupation2 <- as.numeric(data$occupation2)
data$fruitvegintake <- as.numeric(data$fruitvegintake)
data$smoking_status <- as.numeric(data$smoking_status)
data$alcohol <- as.numeric(data$alcohol)
data$cvd <- as.numeric(data$cvd)


columns <- colnames(data)
new_column_names <- paste ("c", 1:34, sep= "_")
data1 <- data 

names(data1)[1:34] <- new_column_names

# Imputation --------------------------------------------------------------

#Loading mice library
library(mice)

# Checking pattern of missingness for the 2 groups

plot1 <- md.pattern(data1) # helps to look at the pattern of the missing data for intervention group
plot1

# Impute the missing values, starting with 20 imputed data sets. 
impute_int_20 <- mice(data , m=20, seed = 1234)
summary(impute_int_20) 
plot2 <- plot(impute_int_20) ###Looks like there could be correlation, impute 60 more
plot2

impute_int_80 <- mice.mids(impute_int_20, maxit=60, seed = 1234, print=F)
plot3 <- plot(impute_int_80)
plot3



#For extra imputations we decide to use 80 rather than 20
impute.data.extra <- mice(data , m=80, seed = 1234)
summary(impute.data.extra) 
plot(impute.data.extra)

# # Obtaining the mean across the 80 imputations allowing for within and between variability 
# impdat_int <- complete(impute.int.extra,action="long",include = FALSE)
# pool_mean_cost_int <- with(impdat_int, by(impdat_int, .imp, function(x) c(mean(x$Costs24),sd(x$Costs24))))
# pool_mean_cost_int
# 
# Reduce("+",pool_mean_cost_int)/length(pool_mean_cost_int)

#We average the imputed dataset to obtain the final dataset
impute_mean_data <- rep(0, length(complete(impute.data.extra, 1)))
for(i in 1:80){impute_mean_data <- impute_mean_data + complete(impute.data.extra, i)}
imputed_data <- impute_mean_data/80

data <- imputed_data
# Transforming variables --------------------------------------------------
data$X1 <- as.factor(data$X1)
data$X2a <- as.factor(data$X2a)
data$residence <- as.factor(data$residence)
data$region <- as.factor(data$region)
data$sex <- as.factor(data$sex)
data$ethnicity2 <- as.factor(data$ethnicity2)
data$agegrps <- as.factor(data$agegrps)
data$maritalstatus <- as.factor(data$maritalstatus)
data$education <- as.factor(data$education)
data$occupation <- as.factor(data$occupation)
data$occupation2 <- as.factor(data$occupation2)
data$fruitvegintake <- as.factor(data$fruitvegintake)
data$smoking_status <- as.factor(data$smoking_status)
data$alcohol <- as.factor(data$alcohol)
data$cvd <- as.factor(data$cvd)
write.csv(data, "imputed_STEPS_dataset.csv")
write.dta(data, "imputed_STEPS_dataset.dta")
