
setwd("C:/Users/Seth/Desktop/BAP Practice  Module/GNB/Set A (order level)")

library(caTools)
library(dplyr)
library(caret)

# Read in data
data <- read.csv('Modeling_Dataset_20210910_v2.csv', stringsAsFactors = FALSE)


#Factorise data
#data$Rating <- as.factor(data$Rating)
data$Payment_Method <- as.factor(data$Payment_Method)
data$Source <- as.factor(data$Source)
#data$orderGender <- as.factor(data$orderGender)
#data$OrderHighLow <- factor(data$OrderHighLow,levels=c(0,1),labels=c("Low","High"))
data[,'OrderHighLow']<-factor(data[,'OrderHighLow'])
data[,'RepeatStatus']<-factor(data[,'RepeatStatus'])
data[,'Accepts_Marketing']<-factor(data[,'Accepts_Marketing'])
#data[,'PersonalizationDone']<-factor(data[,'PersonalizationDone'])
#data[,'Briefcases']<-factor(data[,'Briefcases'])
#data[,'Backpacks']<-factor(data[,'Backpacks'])
#data[,'Clutches']<-factor(data[,'Clutches'])
#data[,'Duffels']<-factor(data[,'Duffels'])
#data[,'Totes']<-factor(data[,'Totes'])
#data[,'Messengers']<-factor(data[,'Messengers'])
#data[,'Bifold_Wallets']<-factor(data[,'Bifold_Wallets'])
#data[,'Coin_Card_Holders']<-factor(data[,'Coin_Card_Holders'])
#data[,'Passport_Wallets']<-factor(data[,'Passport_Wallets'])
#data[,'Long_Wallets']<-factor(data[,'Long_Wallets'])
#data[,'Luggage_Tags']<-factor(data[,'Luggage_Tags'])
#data[,'Lanyard']<-factor(data[,'Lanyard'])
#data[,'Bag_Straps']<-factor(data[,'Bag_Straps'])
#data[,'Bracelets']<-factor(data[,'Bracelets'])
#data[,'Member']<-factor(data[,'Member'])



# Create train and test set
set.seed(100)
splitData = sample.split(data$RepeatStatus, SplitRatio = 0.8)
train_set = data[splitData,]
test_set = data[!splitData,]
nrow = nrow(train_set) / nrow(data)

#trainPredict <- glm(RepeatStatus ~ PersonalizationDone, data = train_set, family = binomial)

# Train model on training set using all variables
trainPredict <- glm(RepeatStatus ~ Accepts_Marketing+Installment+OrderHighLow+Discount_Percentage+NumberItemsTotal+Briefcases+Backpacks+Clutches+Duffels+Totes+Messengers+Bifold_Wallets+Coin_Card_Holders+Passport_Wallets+Long_Wallets+Luggage_Tags+Lanyard+Bag_Straps+Bracelets, data = train_set, family = binomial)
summary(trainPredict)

# Use AIC step to select best model based on AIC
start = glm(RepeatStatus ~ Accepts_Marketing+Installment+OrderHighLow+Discount_Percentage+NumberItemsTotal+Briefcases+Backpacks+Clutches+Duffels+Totes+Messengers+Bifold_Wallets+Coin_Card_Holders+Passport_Wallets+Long_Wallets+Luggage_Tags+Lanyard+Bag_Straps+Bracelets, data = train_set, family = binomial)
smallest = formula(glm(RepeatStatus ~ 1, data = train_set, family = binomial))
model_bwd = step(start, direction = "backward", trace = FALSE, scope = smallest)
summary(model_bwd)


# Predict on the test set
testPredict = predict(model_bwd, test_set, type = "response")

# Assign probabilities to do confusion matrix
p_class = ifelse(testPredict > 0.13, 1, 0)  # 0.5 as cut-off point
matrix_table = table(test_set$RepeatStatus, p_class)
matrix_table

# Accuracy metrics
accuracy = sum(diag(matrix_table))/sum(matrix_table)
recall=matrix_table[4]/(matrix_table[2]+matrix_table[4])

# Show confusion matrix
matrix_table 
accuracy
recall

########################################################################
#Do 2nd model using only significant variables
set.seed(150)

splitData2 = sample.split(data$RepeatStatus, SplitRatio = 0.7)
train_set2 = data[splitData2,]
test_set2 = data[!splitData2,]
nrow = nrow(train_set2) / nrow(data)

# Use AIC step to select best model based on AIC - using only significant variables from above
start = glm(RepeatStatus ~ Lineitem.discount+NumberItemsTotal+Bifold_Wallets+Backpacks+Clutches+Duffels+Coin_Card_Holders+Passport_Wallets+Bag_Straps, data = train_set, family = binomial)
smallest = formula(glm(RepeatStatus ~ 1, data = train_set, family = binomial))
model_bwd = step(start, direction = "backward", trace = FALSE, scope = smallest)
summary(model_bwd)

# Predict on the test set
testPredict = predict(model_bwd, test_set, type = "response")

# Assign probabilities to do confusion matrix
p_class = ifelse(testPredict > 0.12, 1, 0)  # 0.5 as cut-off point
matrix_table = table(test_set$RepeatStatus, p_class)

# Accuracy metrics
accuracy = sum(diag(matrix_table))/sum(matrix_table)
recall=matrix_table[4]/(matrix_table[2]+matrix_table[4])

# Show confusion matrix
matrix_table 
accuracy
recall
