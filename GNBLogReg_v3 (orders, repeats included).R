
setwd("C:/Users/Seth/Desktop/BAP Practice  Module/GNB/Set A (order level)")

library(caTools)
library(dplyr)
library(caret)

# Read in data
data <- read.csv('dataframe_v4.csv', stringsAsFactors = FALSE)

#impute blanks in Payment mode, standardise Sales Source columns, add orderhighlow column for orders above $500
#data$Source[data$Source=="shopify_draft_order"]<-"web"
#data$Source[data$Source!="web"]<-"pos"

########add totalvalue

data<- data %>% 
  mutate(OrderHighLow = if_else(TotalPaid>500, 1, 0))


########installment or cash

#Clean up Payment modes
#data$Payment.Method[data$Payment.Method==""]<-"custom"
#data$Payment.Method[data$Payment.Method=="Exchange Credit + External Credit"]<-"External Credit"
#data$Payment.Method[data$Payment.Method=="Stripe + custom"]<-"Stripe"
#data$Payment.Method[data$Payment.Method=="GrabPay + hoolah"]<-"hoolah"
#data$Payment.Method[data$Payment.Method=="Gift Card + custom"]<-"custom"
#dataPayment.Method[data$Payment.Method=="External Debit + manual"]<-"External Debit"
#data$Payment.Method[data$Payment.Method=="Stripe + manual"]<-"Stripe"
#data$Payment.Method[data$Payment.Method=="custom + Exchange Credit"]<-"External Credit"
#data$Payment.Method[data$Payment.Method=="Cash + Exchange Credit"]<-"Cash"
#data$Payment.Method[data$Payment.Method=="Stripe + PayPal Express Checkout"]<-"PayPal Express Checkout"
#data$Payment.Method[data$Payment.Method=="Paynow (Stripe App)"]<-"Stripe"
#data$Payment.Method[data$Payment.Method=="I-banking (Quanda)"]<-"I-banking"
#data$Payment.Method[data$Payment.Method=="Stripe + Bank Deposit"]<-"Stripe"


#new field Installment
#data$Installment[data$Payment.Method!="Atome - 3 easy payments, 0% interest"|data$Payment.Method!="hoolah"]<-"Cash"
#data$Installment[data$Payment.Method=="Atome - 3 easy payments, 0% interest"]<-"Installment"
#data$Installment[data$Payment.Method=="hoolah"]<-"Installment"





#Factorise data
#data$Rating <- as.factor(data$Rating)
data$Payment_Method <- as.factor(data$Payment_Method)
data$Source <- as.factor(data$Source)
#data$orderGender <- as.factor(data$orderGender)
#data$OrderHighLow <- factor(data$OrderHighLow,levels=c(0,1),labels=c("Low","High"))
data[,'OrderHighLow']<-factor(data[,'OrderHighLow'])
data[,'RepeatStatus']<-factor(data[,'RepeatStatus'])
data[,'Marketing_Consent']<-factor(data[,'Marketing_Consent'])
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
trainPredict <- glm(RepeatStatus ~ Marketing_Consent+Rating+NumberItemsTotal+Installment+OrderHighLow+percentDiscount+TotalPaid+Briefcases+Backpacks+Clutches+Duffels+Totes+Messengers+Bifold_Wallets+Coin_Card_Holders+Passport_Wallets+Long_Wallets+Luggage_Tags+Lanyard+Bag_Straps+Bracelets, data = train_set, family = binomial)
summary(trainPredict)

# Use AIC step to select best model based on AIC
start = glm(RepeatStatus ~ Marketing_Consent+Rating+NumberItemsTotal+Installment+OrderHighLow+percentDiscount+TotalPaid+Briefcases+Backpacks+Clutches+Duffels+Totes+Messengers+Bifold_Wallets+Coin_Card_Holders+Passport_Wallets+Long_Wallets+Luggage_Tags+Lanyard+Bag_Straps+Bracelets, data = train_set, family = binomial)
smallest = formula(glm(RepeatStatus ~ 1, data = train_set, family = binomial))
model_bwd = step(start, direction = "backward", trace = FALSE, scope = smallest)
summary(model_bwd)


# Predict on the test set
testPredict = predict(model_bwd, test_set, type = "response")

# Assign probabilities to do confusion matrix
p_class = ifelse(testPredict > 0.25, 1, 0)  # 0.5 as cut-off point
matrix_table = table(test_set$RepeatStatus, p_class)

# Accuracy metrics
accuracy = ssum(diag(matrix_table))/sum(matrix_table)
recall=matrix_table[4]/(matrix_table[2]+matrix_table[4])

# Show confusion matrix
matrix_table 
accuracy
recall

