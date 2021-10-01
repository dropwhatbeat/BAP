

df = read.csv("dataframe_v2.csv")
df$percentDiscount <- df$Discount_Amount/df$Total

df$totalPaid <- df$Discount_Amount+df$Total
df$percentDiscount<- df$Discount_Amount/df$totalPaid

df$MEMBER_RATING <- ifelse(df$in_member_list==0, 0 ,df$MEMBER_RATING)

df$RepeatStatus <- ifelse(df$RepeatStatus>1, 1, df$RepeatStatus)

table(df$Marketing_Consent)
df$Marketing_Consent <- ifelse(df$Marketing_Consent=="yes", 1, 0)

table(df$RepeatStatus)

df$Payment <- ifelse(df$Payment==NA, "", df$Payment)
df$Source <- ifelse(df$Source==NA, "", df$Source)
df$percentDiscount <- ifelse(df$percentDiscount==NA, 0, df$percentDiscount)

write.csv(df, "dataframe_v3.csv")
