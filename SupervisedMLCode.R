library(knitr)
library(tidyr)
library(dplyr)
library(ggplot2)

# Importing dataset
init_data <- read.csv('C:/Data Analysis/datasets/customer_churn.csv')
head(init_data)

dim(init_data)
summary(init_data)
str(init_data)

# The TotalCharges column has 11 missing values
colSums(is.na(init_data))

filter(init_data, is.na(TotalCharges))

# Change the TotalCharges with NA as their value to 0, it's still reasonable to do so because 
# they all have a tenure of 0, which means that they still don't have a record of payment yet
# and that they were new subscribers who had not yet accumulated charges. Note that these are 11
# customers

# init_data$TotalCharges[is.na(init_data$TotalCharges)] <- 0
# filter(init_data, TotalCharges == 0)

# Another option would be to delete the rows entirely... so unsa man... HAHAHAHHAHA
# In the end, kay i'll remove nalang because only a small number of observations were affected
# Therefore, these records were removed from the analysis

init_data <- filter(init_data, !is.na(TotalCharges))

# Checking class imbalance
table(init_data$Churn)
prop.table(table(init_data$Churn))

ggplot(init_data, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = c("red", "green"))
  labs(
    title = "Distribution of Customer Churn",
    x = "Churn",
    y = "Count"
  )

# Detecting outliers  (tenure = no outliers)
boxplot(init_data$tenure,
        horizontal = TRUE)$out

# MonthlyCharges (no outliers)
boxplot(init_data$MonthlyCharges,
        horizontal = TRUE)$out

# TotalCharges (no outliers)
boxplot(init_data$TotalCharges,
        horizontal = TRUE)$out

# The numerical variables don't have any outliers, which is good

# Checking payment method
unique(init_data$PaymentMethod)
table(init_data$PaymentMethod)

ggplot(init_data, aes(x = PaymentMethod, fill = PaymentMethod)) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "green", "pink", "violet"))
  labs(
    title = "Distribution of Customer Churn",
    x = "Churn",
    y = "Count"
  )
  
