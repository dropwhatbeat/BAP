
install.packages('devtools')
library(devtools)
install_github("ropensci/genderdata")
library(genderdata)
install_github("ropensci/gender")
library(gender)
library(dplyr)

df = read.csv("customer.csv")
#replace firstname with email name if firstname is less than 2 characters / empty
df$First.Name <- ifelse(nchar(df$First.Name) < 2, gsub("@.*$", "", df$Email.Address),df$First.Name)


table(df$Gender)

df$Gender <- ifelse(df$Gender == "'-" | df$Gender == "", gender(df$First.Name, method = "ssa")$gender ,df$Gender)
df$Gender <- ifelse(df$Gender == "Male" | df$Gender == "male", "M", df$Gender)
df$Gender <- ifelse(df$Gender == "Female" | df$Gender == "female", "F", df$Gender)

write.csv(df, "customer_gender.csv")
