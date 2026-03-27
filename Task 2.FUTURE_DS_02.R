library(tidyverse)

WA_Fn.UseC_.Telco.Customer.Churn

telco <- read_csv("C:/Users/nokom/Downloads/WA_Fn-UseC_-Telco-Customer-Churn.csv")

dim(telco)

table(telco$Churn)

head(telco)
str(telco)
summary(telco)

telco$TotalCharges <- as.numeric(telco$TotalCharges)

colSums(is.na(telco))

telco <- na.omit(telco)

colSums(is.na(telco))

dim(telco)

telco$Churn <- ifelse(telco$Churn == "Yes", 1, 0)
table(telco$Churn)

mean(telco$Churn)


install.packages("dplyr")
library(dplyr)
telco %>%
  group_by(Contract) %>%
  summarise(churn_rate = mean(Churn))

telco %>%
  group_by(Churn) %>%
  summarise(avg_tenure = mean(tenure))

telco %>%
  group_by(Churn) %>%
  summarise(avg_monthly_charge = mean(MonthlyCharges))

model <- glm(Churn ~ tenure + MonthlyCharges + Contract +
               InternetService + TechSupport,
             data = telco,
             family = "binomial")

summary(model)

exp(coef(model))

write.csv(telco, "telco_clean_data.csv", row.names = FALSE)
write.csv(telco, "C:/Users/nokom/Documents/telco_clean_data.csv", row.names = FALSE)

