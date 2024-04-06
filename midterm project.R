# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
hospital_data <- read.csv("hospitals.csv")

# 1. Perform exploratory analysis
# a) How big is the dataset?
dim(hospital_data)
## size is 2000

# b) What are the names of the columns?
colnames(hospital_data)

# c) What data types are each column?
column_types <- sapply(hospital_data, class)
print("Column data types:")
print(column_types)

# d) Are there missing values?
missing_values <- colSums(is.na(hospital_data))
print("Missing values:")
print(missing_values)

# e) Which hospital has the lowest number of beds?
hospital_lowest_beds <- hospital_data %>% 
  filter(Beds == min(Beds))
print("Hospital with the lowest number of beds:")
print(hospital_lowest_beds)

# f) Which hospital has the lowest expense?
hospital_lowest_expense <- hospital_data %>% 
  filter(Total.Expense == min(Total.Expense))
print("Hospital with the lowest expense:")
print(hospital_lowest_expense)

# g) How many hospitals deliver babies?
hospitals_deliver_babies <- hospital_data %>% 
  filter(Births > 0) %>%
  nrow()
print("Number of hospitals that deliver babies:")
print(hospitals_deliver_babies)       

# h) Using ggplot, scatterplot number of beds vs Total Expense
ggplot(hospital_data, aes(x = Beds, y = Total.Expense, col=Region)) +
  geom_point() +
  labs(x = "Number of Beds", y = "Total Expense") +
  ggtitle("Scatterplot of Number of Beds vs Total Expense") 

# i) Using ggplot, scatterplot Admissions vs Total Expense
ggplot(hospital_data, aes(x = Admissions, y = Total.Expense, col=Region)) +
  geom_point() +
  labs(x = "Admissions", y = "Total Expense") +
  ggtitle("Scatterplot of Admissions vs Total Expense")

# j) Using dplyr and ggplot, scatterplot beds vs Total Expense but only for hospitals that deliver babies
ggplot(hospital_data %>% filter(Births > 0), aes(x = Beds, y = Total.Expense, col=Region)) +
  geom_point() +
  labs(x = "Number of Beds", y = "Total Expense") +
  ggtitle("Scatterplot of Number of Beds vs Total Expense for Hospitals that Deliver Babies")

# k) One more question that you believe would be useful.
# For example, let's find out the distribution of hospitals by region
hospital_data %>%
  count(Region) %>%
  ggplot(aes(x = Region, y = n, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Number of Hospitals") +
  ggtitle("Distribution of Hospitals by Region")

###########
# Descriptive Analysis
# i. For Pie Chart: Admissions and Births
admissions_sum <- sum(hospital_data$Admissions)
Beds_sum <- sum(hospital_data$Beds)
pie_data <- data.frame(Category = c("Admissions", "Beds"),
                       Value = c(admissions_sum, births_sum))
ggplot(pie_data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  labs(title = "Admissions vs Beds Pie Chart") +
  theme_void()

# ii. For the column/bar charts: Admissions and Outpatient.Visits
hospital_data %>%
  summarise(Admissions = sum(Admissions), Outpatient.Visits = sum(Outpatient.Visits)) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  ggplot(aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Value") +
  ggtitle("Total Admissions vs Total Outpatient Visits")

# iii. For Line Chart: Expense and Payroll Expense
hospital_data$Date <- as.Date(hospital_data$Date)

ggplot(hospital_data, aes(x = Total.Expense)) +
  geom_line(aes(y = Payroll.Expense, colour = "Payroll Expense")) +
  geom_line(aes(y = Total.Expense, colour = "Total expense")) +
  labs(title = "Trend of Payroll Expense and Total Expense Over Time", x = "Date", y = "Value") +
  theme_minimal() +
  scale_colour_manual(values = c("Payroll Expense" = "purple", "Total expense" = "red"))

# 3. Simple Regression
simple_regression <- lm(Total.Expense ~ Beds, data = hospital_data)

summary(simple_regression)

# ii. The value of the R^2 is 0.6043

# iv. R-squared (R²) measures the proportion of the variance in the dependent variable 
 # (Total Expense) that is explained by the independent variable (Beds). A value of 0.6043 
 # means that approximately 60.43% of the variability in Total Expense can be explained by 
 # the variation in Beds.

# v. There is one p-value reported in the summary, corresponding to the coefficient estimate 
 # for Beds. The p-value is shown as < 2e-16. This p-value represents the probability of 
 # observing the given result (or more extreme results) under the null hypothesis that the 
 # coefficient for Beds is zero. Since the p-value is extremely small, we reject the null 
 # hypothesis, indicating that the coefficient for Beds is significantly different from zero 
 # and that Beds has a significant impact on Total Expense.

# vi. R-squared (R²) is a statistical measure that represents the proportion of the variance 
 # in the dependent variable (Total Expense) that is explained by the independent variable 
 # (Beds). A higher R² suggests a better fit of the regression model to the data, indicating 
 # that the independent variable is more effective at explaining the variability in the 
 # dependent variable. In this case, R² of 0.6043 means that Beds explains approximately 
 # 60.43% of the variability in Total Expense.

# vii. 
 # 55,000,000 = 1,084.56x - 16,060.93 
 # x=50,720.37 (50) 
 # 75,000,000 = 1,084.56x - 16,060.93
 # x=69,201.77 (69)


# 4. Multivariate Regression
multivariate_regression <- lm(Total.Expense ~ Beds + Admissions, data = hospital_data)

summary(multivariate_regression)

# ii. The value of the R^2 is 0.7398.

# iii. In this case, the R-squared (R²) measures the percentage of variation 
 # in Total Expense that is explained by the independent variables 
 # (Outpatient Visits and Births). It indicates the goodness of fit of the 
 # regression model. For example, an R² of 0.7398 means that approximately 
 # 74% of the variability in Total Expense can be explained by the variation 
 # in Outpatient Visits and Births.

# iv. The p-value for Admissions is less than 2e-16, indicating that it is 
 # highly statistically significant.
# The p-value for Beds is 7.25e-13, also indicating high statistical 
 # significance.
# These p-values represent the probability of observing the given result 
 # (or more extreme results) under the null hypothesis that the coefficient 
 # is zero. Since both p-values are much smaller than the conventional 
 # significance level of 0.05, we reject the null hypothesis for both 
 # Admissions and Beds, indicating that both variables have a significant 
 # effect on Total Expense.

# v. R-squared (R²):
 # R-squared measures how well the independent variables explain the variability 
 # in the dependent variable.
 # In this case, R-squared is 0.7398, indicating that about 73.98% of the 
 # variability in Total Expense is explained by Admissions and Beds.

# P-values:
 # P-values indicate the significance of each independent variable's 
 # contribution to the model.
 # Both Admissions and Beds have extremely low p-values, < 2e-16 and 
 # 7.25e-13 respectively, indicating high statistical significance.
 # This means both Admissions and Beds have a significant impact on Total Expense.


