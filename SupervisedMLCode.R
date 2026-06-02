library(knitr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(psych)
library(rcompanion)

# Importing dataset
init_data <- read.csv('C:/Data Analysis/datasets/customer_churn.csv')
head(init_data)

dim(init_data)
summary(init_data)
str(init_data)
typeof(init_data)
class(init_data)

# The TotalCharges column has 11 missing values
colSums(is.na(init_data))

filter(init_data, is.na(TotalCharges))

# Change the TotalCharges with NA as their value to 0, it's still reasonable to do so because 
# they all have a tenure of 0, which means that they still don't have a record of payment yet
# and that they were new subscribers who had not yet accumulated charges. Note that these are 11
# customers

# Code for changing the NA values to 0
# init_data$TotalCharges[is.na(init_data$TotalCharges)] <- 0
# filter(init_data, TotalCharges == 0)

# Another option would be to delete the rows entirely...
# In the end, I removed the rows because only a small number of observations were affected
# Therefore, these records were removed from the analysis

init_data <- filter(init_data, !is.na(TotalCharges))

# Checking class imbalance
# In this dataset, there are generally more people who DON'T churn...
# There is imbalance in the dataset
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
  
# Descriptive statistics for numerical variables
result <- describe(init_data[, c("tenure", "MonthlyCharges", "TotalCharges")])
kable(result, digits = 2, caption = "Descriptive Statistics of Churn Dataset")

# Conditional Probabilities
# Customers are less likely to churn if they have a two year or one year contract
# This is in contrast to people who have a month-to-month contract
prop.table(
  table(init_data$Contract, init_data$Churn),
  margin = 1
)

# Less likely to churn if naay partner
prop.table(
  table(init_data$Partner, init_data$Churn),
  margin = 1
)

# If they have DSL or No internet service, they are less likely to churn
prop.table(
  table(init_data$InternetService, init_data$Churn),
  margin = 1
)

# No internet service and has tech support: less likely to churn
prop.table(
  table(init_data$TechSupport, init_data$Churn),
  margin = 1
)

# If not paperless billing, less likely to churn
prop.table(
  table(init_data$PaperlessBilling, init_data$Churn),
  margin = 1
)

# Many customers use electronic check (according to the barplot)
# but customers who prefer bank transfer, credit card, and mailed check 
# are the ones who are less likely to churn.
prop.table(
  table(init_data$PaymentMethod, init_data$Churn),
  margin = 1
)

# using histograms for EDA
ggplot(init_data, aes(x = tenure)) +
  geom_histogram(bins = 30)

ggplot(init_data, aes(x = MonthlyCharges)) +
  geom_histogram(bins = 30)

ggplot(init_data, aes(x = TotalCharges)) +
  geom_histogram(bins = 30)

ggplot(init_data,
       aes(x = Churn,
           y = tenure,
           fill = Churn)) +
  geom_boxplot()

ggplot(init_data,
       aes(x = Churn,
           y = MonthlyCharges,
           fill = Churn)) +
  geom_boxplot()

ggplot(init_data,
       aes(x = Churn,
           y = TotalCharges,
           fill = Churn)) +
  geom_boxplot()

# Doing chi-square test to test associativity between categorical predictors and the target variable
# Also, maybe we'll use Cramer's V to see how strong their relationship is, if there is any.

contract_churn <- table(
  init_data$Contract,
  init_data$Churn
)
chisq.test(contract_churn)
cramerV(contract_churn)

partner_churn <- table(
  init_data$Partner,
  init_data$Churn
)
chisq.test(partner_churn)
cramerV(partner_churn)

internetservice_churn <- table(
  init_data$InternetService,
  init_data$Churn
)
chisq.test(internetservice_churn)
cramerV(internetservice_churn)

techsupport_churn <- table(
  init_data$TechSupport,
  init_data$Churn
)
chisq.test(techsupport_churn)
cramerV(techsupport_churn)

paperless_churn <- table(
  init_data$PaperlessBilling,
  init_data$Churn
)
chisq.test(paperless_churn)
cramerV(paperless_churn)

paymethod_churn <- table(
  init_data$PaymentMethod,
  init_data$Churn
)
chisq.test(paymethod_churn)
cramerV(paymethod_churn)