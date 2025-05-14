# Load Necessary Libraries
library(tidyverse)
library(dplyr)
library(readr)
library(MASS)
library(ggplot2)
library(GGally)

# =====================================
# 1. Data Understanding
# =====================================

# Load Dataset
insurance_data = read_csv("C:/Users/LENOVO/Downloads/insurance.zip")
View(insurance_data)

# Summarization of dataset
glimpse(insurance_data)
summary(insurance_data)

# =====================================
# 2. Data Cleaning
# =====================================

# Check for Missing Values
sum(is.na(insurance_data))

# Remove Duplicate Records
insurance_data = distinct(insurance_data)

# =====================================
# 3. Descriptive Statistics
# =====================================

# Average age of policyholders
average_age = mean(insurance_data$Age)
print(paste("Average Age:", average_age))

# Proportion of senior citizens
proportion_senior = mean(insurance_data$Is_Senior)
print(paste("Proportion of Senior Citizens:", proportion_senior))

# Distribution of marital status
marital_status_dist = insurance_data %>%
  group_by(Marital_Status) %>%
  summarise(count = n())
print(marital_status_dist)

# Average premium amount
average_premium = mean(insurance_data$Premium_Amount)
print(paste("Average Premium Amount:", average_premium))

# Average premium amount for each region
average_premium_region = insurance_data %>%
  group_by(Region) %>%
  summarise(average_premium = mean(Premium_Amount))
print(average_premium_region)

# Distribution of claims frequency
claims_frequency_dist = insurance_data %>%
  group_by(Claims_Frequency) %>%
  summarise(count = n())
print(claims_frequency_dist)

# Distribution of claims severity
claims_severity_dist = insurance_data %>%
  group_by(Claims_Severity) %>%
  summarise(count = n())
print(claims_severity_dist)

# Average age by marital status
average_age_marital_status = insurance_data %>%
  group_by(Marital_Status) %>%
  summarise(average_age = mean(Age))
print(average_age_marital_status)

# Most common claims severity
common_claims_severity = insurance_data %>%
  group_by(Claims_Severity) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)
print(common_claims_severity)

# =====================================
# 4. Correlation and Relationship Analysis
# =====================================

# Correlation between Age and Premium Amount
correlation_age_premium = cor(insurance_data$Age, insurance_data$Premium_Amount)
print(paste("Correlation between Age and Premium Amount:", correlation_age_premium))

# Correlation between Prior Insurance Premium Adjustment and Current Premium Amount
correlation_prior_current_premium = cor(
  insurance_data$Prior_Insurance_Premium_Adjustment,
  insurance_data$Premium_Amount
)
print(paste("Correlation between Prior Insurance Premium Adjustment and Current Premium Amount:", correlation_prior_current_premium))

# Effect of Prior Insurance on Claims Frequency and Severity
prior_insurance_effect = insurance_data %>%
  group_by(Prior_Insurance) %>%
  summarise(
    average_claims_frequency = mean(Claims_Frequency),
    average_claims_severity = mean(as.numeric(Claims_Severity))
  )
print(prior_insurance_effect)

# Relationship between Policy Type and Premium Amount
policy_type_premium = insurance_data %>%
  group_by(Policy_Type) %>%
  summarise(average_premium = mean(Premium_Amount))
print(policy_type_premium)

# Categorizing Credit Score Ranges
insurance_data = insurance_data %>%
  mutate(Credit_Score_Range = case_when(
    Credit_Score >= 750 ~ "Excellent (750+)",
    Credit_Score >= 700 & Credit_Score < 750 ~ "Good (700-749)",
    Credit_Score >= 650 & Credit_Score < 700 ~ "Fair (650-699)",
    Credit_Score < 650 ~ "Poor (<650)"
  ))

# Average premium amount by Credit Score Range
average_premium_credit_score = insurance_data %>%
  group_by(Credit_Score_Range) %>%
  summarise(average_premium = mean(Premium_Amount))
print(average_premium_credit_score)

# Premium differences by Marital Status
premium_marital_status = insurance_data %>%
  group_by(Marital_Status) %>%
  summarise(average_premium = mean(Premium_Amount))
print(premium_marital_status)

# Claims Frequency differences by Policy Type
claims_frequency_policy = insurance_data %>%
  group_by(Policy_Type) %>%
  summarise(average_claims_frequency = mean(Claims_Frequency))
print(claims_frequency_policy)

# Claims Adjustment by Policy Type
claims_adjustment_policy = insurance_data %>%
  group_by(Policy_Type) %>%
  summarise(average_claims_adjustment = mean(Claims_Adjustment))
print(claims_adjustment_policy)

# Effect of Prior Insurance Premium Adjustment on Current Premium Amount
effect_prior_insurance = insurance_data %>%
  group_by(Prior_Insurance_Premium_Adjustment) %>%
  summarise(average_current_premium = mean(Premium_Amount))
print(effect_prior_insurance)

# Effect of Claims Adjustment on Premium Amount
effect_claims_adjustment = insurance_data %>%
  group_by(Claims_Adjustment) %>%
  summarise(average_premium_amount = mean(Premium_Amount))
print(effect_claims_adjustment)

# =====================================
# 5. Data Regression
# =====================================

#Predict Premium Amount based on Age and Credit Score
Predict_Premium_Amount = lm(Premium_Amount ~ Age + Credit_Score, data = insurance_data)
summary(Predict_Premium_Amount)

#Predict Claims Frequency based on Age, Credit Score, and Prior Insurance
Predict_Claims_Frequency = lm(Claims_Frequency ~ Age + Credit_Score + Prior_Insurance, data = insurance_data)
summary(Predict_Claims_Frequency)

# =====================================
# 6. ANOVA Test
# =====================================
#Premium Amount difference across Marital Status
Premium_Amount_Across_Marital_Status = aov(Premium_Amount ~ Marital_Status, data = insurance_data)
summary(Premium_Amount_Across_Marital_Status)

#Claims Frequency difference across Policy Type
Claims_Frequency_across_Policy_Type = aov(Claims_Frequency ~ Policy_Type, data = insurance_data)
summary(Claims_Frequency_across_Policy_Type)

#Premium Amount difference across Regions
Premium_Amount_difference_across_Regions = aov(Premium_Amount ~ Region, data = insurance_data)
summary(Premium_Amount_difference_across_Regions)

# =====================================
# 7. Data Visualization
# =====================================

##Website Visits vs Premium Amount 
ggplot(insurance_data, aes(x = Website_Visits, y = Premium_Amount)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Website Visits and Premium Amount",
       x = "Website Visits", y = "Premium Amount") +
  theme_minimal()

# Boxplot of Age by Marital Status
ggplot(insurance_data, aes(x = Marital_Status, y = Age, fill = Marital_Status)) +
  geom_boxplot(outlier.color = "red", notch = TRUE) +
  labs(title = "Boxplot of Age by Marital Status", x = "Marital Status", y = "Age") +
  theme_minimal()

# Histogram of Credit Score
ggplot(insurance_data, aes(x = Credit_Score)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Histogram of Credit Score", x = "Credit Score") +
  theme_minimal()

#Customer Distribution across Regions
insurance_data %>%
  group_by(Region) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = "", y = Count, fill = Region)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Customers Across Regions") +
  theme_void()

#Claims Severity Distribution
insurance_data %>%
  group_by(Claims_Severity) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = "", y = Count, fill = Claims_Severity)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Claims Severity") +
  theme_void()

#Premium Amount by Credit Score Range 
insurance_data %>%
  group_by(Credit_Score_Range) %>%
  summarise(Average_Premium = mean(Premium_Amount, na.rm = TRUE)) %>%
  ggplot(aes(x = Credit_Score_Range, y = Average_Premium, fill = Credit_Score_Range)) +
  geom_col() +
  labs(title = "Average Premium by Credit Score Range", x = "Credit Score Range", y = "Average Premium") +
  theme_minimal()

#Senior vs Non-Senior Citizens Distribution
insurance_data %>%
  mutate(Senior_Status = ifelse(Is_Senior == 1, "Senior Citizen", "Non-Senior")) %>%
  group_by(Senior_Status) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = "", y = Count, fill = Senior_Status)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Senior Citizens vs Non-Senior Citizens") +
  theme_void()

#Claims Severity Distribution
insurance_data %>%
  group_by(Claims_Severity) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Claims_Severity, y = Count, fill = Claims_Severity)) +
  geom_col() +
  labs(title = "Distribution of Claims Severity", x = "Claims Severity Level", y = "Number of Policyholders") +
  theme_minimal()

# Pairplot of Selected Features
insurance_subset <- insurance_data[, c("Premium_Amount", "Total_Discounts", "Credit_Score", "Region")]
ggpairs(insurance_subset,
        columns = 1:4,
        aes(color = Region),
        title = "Pairplot of Selected Features from Insurance Dataset")


        
