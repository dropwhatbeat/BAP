
install.packages('devtools')
library(devtools)
install_github("eamoncaddigan/GenderGuesser")
library("GenderGuesser")
#genderize.io limits each IP address to 100 (free) queries per day, will not be using this
install_github("ropensci/genderdata")
library(genderdata)
install_github("ropensci/gender")
library(gender)
library(dplyr)
library(stringr)

df = read.csv("Orders_Cleaned_Final.csv")

#replace all NA values in one-hot encode  & repeat status with 0.
df[is.na(df)]<-0

#remove rows where both email and names are missing
df <- df[!(nchar(df$Email)<2 & nchar(df$Billing.Name)<2), ]

#take first name
df$Billing.Name <- word(df$Billing.Name, 1)

#replace firstname with email name if firstname is less than 2 characters / empty
df$Billing.Name <- ifelse(nchar(df$Billing.Name) < 3, gsub("@.*$", "", df$Email),df$Billing.Name)

df$Billing.Name <- gsub('[0-9]+', '', df$Billing.Name)

df$Gender <- ""

for ( i in 1:length(df$Billing.Name)) {
  try(df$Gender[i] <- gender(df$Billing.Name[i],method="ssa")$gender)
}

#imputation: for gender = undefined, take random value
df$Gender[df$Gender==""]<-sample(c("male", "female"))

table(df$Gender)

df$Gender <- ifelse(df$Gender == "'-" | df$Gender == "", gender(df$First.Name, method = "ssa")$gender ,df$Gender)
df$Gender <- ifelse(df$Gender == "Male" | df$Gender == "male", "M", df$Gender)
df$Gender <- ifelse(df$Gender == "Female" | df$Gender == "female", "F", df$Gender)

write.csv(df, "Order_Cleaned_Gender.csv")
