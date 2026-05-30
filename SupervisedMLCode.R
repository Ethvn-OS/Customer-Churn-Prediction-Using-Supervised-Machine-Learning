library(knitr)
library(tidyr)
library(dplyr)

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