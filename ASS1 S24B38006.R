# ALMUZAHIM NASEEF MUHAMAD S24B38/006 B30296
# Loading necessary libraries for data manipulation and date handling
library(tidyverse)
library(lubridate)
library(naniar)
library(ggplot2)

# Importing the dataset
data <- read.csv("C:/Users/nasee/Downloads/ida_credits_to_uganda.csv")

# Set working directory for saving outputs
setwd("C:/Users/nasee/OneDrive/Desktop/ASS1 BIG DATA S24B38006")


# Checking structure of dataset to understand variable types
str(data)

# Checking for missing values
sum(is.na(data))  # Total number of missing values
missing_summary <- data %>% summarise_all(~ sum(is.na(.)))  # Missing values per column
print(missing_summary)

# Separating numeric variables for visualization
numeric_vars <- data %>% select_if(is.numeric)

# Visualizing distribution of numeric variables
for (var_name in names(numeric_vars)) {
  # Histogram
  hist_plot <- ggplot(data, aes_string(x = var_name)) +
    geom_histogram(binwidth = 500000, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", var_name), x = var_name, y = "Frequency") +
    theme_minimal()
  print(hist_plot)
  
  # Boxplot
  box_plot <- ggplot(data, aes_string(y = var_name)) +
    geom_boxplot(fill = "orange", color = "darkred") +
    labs(title = paste("Boxplot of", var_name), y = var_name) +
    theme_minimal()
  print(box_plot)
}

# Imputing missing values using median (appropriate for skewed data)
data_imputed <- data %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.), median(., na.rm = TRUE), .)))

# Removing outliers using IQR method across all numeric columns
data_no_outliers <- data_imputed
for (var_name in names(numeric_vars)) {
  Q1 <- quantile(data_no_outliers[[var_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data_no_outliers[[var_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data_no_outliers <- data_no_outliers %>%
    filter(.data[[var_name]] >= lower_bound & .data[[var_name]] <= upper_bound)
}

# Re-checking boxplots after outlier removal
for (var_name in names(numeric_vars)) {
  box_plot <- ggplot(data_no_outliers, aes_string(y = var_name)) +
    geom_boxplot(fill = "orange", color = "darkred") +
    labs(title = paste("Boxplot of", var_name), y = var_name) +
    theme_minimal()
  print(box_plot)
}

# Extracting year from Board Approval Date for time series analysis
data_no_outliers <- data_no_outliers %>%
  mutate(Board.Approval.Date = dmy(Board.Approval.Date),
         Year = year(Board.Approval.Date))# categorizing by year helps us understand when the loans were taken , giving us a better time period

# Summarizing total disbursement per year
disbursement_summary <- data_no_outliers %>%
  group_by(Year) %>%
  summarise(Total_Disbursed = sum(`Disbursed.Amount..US..`, na.rm = TRUE))

# Plotting time series trend of disbursements
ggplot(disbursement_summary, aes(x = Year, y = Total_Disbursed)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred") +
  labs(title = "Trend of World Bank Disbursements to Uganda",
       x = "Year",
       y = "Total Disbursed Amount (US$)") +
  theme_minimal()# - Disbursements were minimal before the 1990s, suggesting limited financial engagement.
# - A sharp increase occurred in the early 1990s, likely due to major development programs.
# - The peak in 1995 indicates a high concentration of funding, possibly for infrastructure or recovery.
# - After 1995, disbursements declined and fluctuated, reflecting changing priorities or project cycles.


# Analyzing overall credit status distribution
unique(data_no_outliers$Credit.Status)  # View unique statuses
data_no_outliers %>% count(Credit.Status)  # Frequency table of credit statuses, from these statistics we can see that Uganda as a country is very good at taking the loans as most of them were fully repaid showing showing Ugandas reliability in finanaces

# Analyzing original principal amount borrowed
summary(data_no_outliers$`Original.Principal.Amount..US..`)  # Summary stats

# Visualizing distribution of original principal amounts
ggplot(data_no_outliers, aes(x = `Original.Principal.Amount..US..`)) +
  geom_histogram(binwidth = 500000, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Original Principal Amounts",
       x = "Original Principal Amount (US$)",
       y = "Frequency") +
  theme_minimal()# from the distribution we can see that the data in the variable is positively skewed meaning most of the loans borrowed were minimal and very few that were big that brought up the average

