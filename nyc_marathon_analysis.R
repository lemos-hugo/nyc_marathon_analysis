# NYC Marathon Performance Analytics & Training Services Analysis
# Business Focus: Insights for training programs, coaches, and performance optimization

# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(scales)
library(gridExtra)
library(viridis)
library(corrplot)
library(randomForest)
library(caret)
library(broom)

# Set theme for consistent plotting
theme_set(theme_minimal() + 
            theme(plot.title = element_text(size = 14, face = "bold"),
                  plot.subtitle = element_text(size = 12),
                  axis.title = element_text(size = 11),
                  legend.title = element_text(size = 10)))

# Load and preprocess data
# Load the NYC Marathon dataset with actual column names from the sample:
# 'Year', 'Race', 'Name', 'Gender', 'Age', 'State', 'Country', 'Overall', 'Finish Time', 'Finish;'
marathon_data <- read_csv("/Users/hugolemos/Documents/NYCDSA/R Project/NYC Marathon Results.csv")

original_gender_dist <- marathon_data %>%
  filter(!is.na(Gender)) %>%
  count(Gender) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

print("Original gender distribution:")
print(original_gender_dist)

# IMPROVED SAMPLING: Stratified sampling to maintain representativeness
if(nrow(marathon_data) > 100000) {
  print("Large dataset detected. Using stratified sampling to maintain distributions...")
  
  # Calculate sampling proportions to maintain gender balance
  target_sample_size <- 200000
  
  # Stratified sampling by Gender and Year to maintain temporal and gender balance
  marathon_data <- marathon_data %>%
    filter(!is.na(Gender), !is.na(Year)) %>%  # Remove NAs first
    group_by(Gender, Year) %>%
    slice_sample(prop = target_sample_size / nrow(marathon_data)) %>%
    ungroup()
  
  # If still too large, do proportional sampling by gender
  if(nrow(marathon_data) > target_sample_size) {
    marathon_data <- marathon_data %>%
      group_by(Gender) %>%
      slice_sample(n = round(target_sample_size * (n() / nrow(marathon_data)))) %>%
      ungroup()
  }
  
  print(paste("Working with stratified sample of", nrow(marathon_data), "rows"))
  
  # Check sampled gender distribution
  sampled_gender_dist <- marathon_data %>%
    filter(!is.na(Gender)) %>%
    count(Gender) %>%
    mutate(Percentage = round(n / sum(n) * 100, 1))
  
  print("Sampled gender distribution:")
  print(sampled_gender_dist)
  
  # Verify we maintained the distribution
  if(abs(sampled_gender_dist$Percentage[1] - original_gender_dist$Percentage[1]) > 5) {
    print("WARNING: Sample may not be fully representative. Consider adjusting sampling strategy.")
  } else {
    print("✓ Stratified sampling maintained representative gender distribution")
  }
}


# Data preprocessing using exact column names
marathon_clean <- marathon_data %>%
  # Remove rows with missing critical data
  filter(!is.na(Age), !is.na(Gender), !is.na(`Finish Time`)) %>%
  # Create derived variables working directly with 'Finish Time' string
  mutate(
    # Standardize Gender values
    Gender = case_when(
      str_to_upper(Gender) %in% c("M", "MALE", "MEN") ~ "M",
      str_to_upper(Gender) %in% c("W", "FEMALE", "WOMEN") ~ "F",
      TRUE ~ as.character(Gender)
    ),
    # Create age groups
    Age_group = case_when(
      Age < 25 ~ "Under 25",
      Age >= 25 & Age < 35 ~ "25-34",
      Age >= 35 & Age < 45 ~ "35-44",
      Age >= 45 & Age < 55 ~ "45-54",
      Age >= 55 & Age < 65 ~ "55-64",
      Age >= 65 ~ "65+"
    ),
    # Create performance categories directly from 'Finish Time' string (HH:MM:SS format)
    Performance_category = case_when(
      # Elite: Sub-3:00 (times starting with 01: or 02:0, 02:1, 02:2, 02:3, 02:4, 02:5)
      str_detect(`Finish Time`, "^01:") | 
        str_detect(`Finish Time`, "^02:[0-5]") ~ "Elite (Sub-3:00)",
      
      # Competitive: 3:00-3:30 (02:6-02:9 and 03:0-03:2)
      str_detect(`Finish Time`, "^02:[6-9]") | 
        str_detect(`Finish Time`, "^03:[0-2]") ~ "Competitive (3:00-3:30)",
      
      # Good: 3:30-4:00 (03:3-03:5 and some 04:0)
      str_detect(`Finish Time`, "^03:[3-5]") | 
        str_detect(`Finish Time`, "^04:0[0-0]") ~ "Good (3:30-4:00)",
      
      # Average: 4:00-4:30 (04:0-04:2)
      str_detect(`Finish Time`, "^04:[0-2]") ~ "Average (4:00-4:30)",
      
      # Recreational: 4:30-5:00 (04:3-04:5 and some 05:0)
      str_detect(`Finish Time`, "^04:[3-5]") | 
        str_detect(`Finish Time`, "^05:0[0-0]") ~ "Recreational (4:30-5:00)",
      
      # Survival: 5:00+ (05: and above)
      TRUE ~ "Survival (5:00+)"
    ),
    # Create binary variables for modeling
    Gender_binary = ifelse(Gender == "M", 1, 0)
  ) %>%
  # Add a separate mutate for time parsing to avoid variable reference issues
  mutate(
    # Extract hours and minutes for numerical analysis (simpler approach)
    Hours = as.numeric(str_extract(`Finish Time`, "^\\d+")),
    Minutes_part = as.numeric(str_extract(`Finish Time`, "(?<=:)\\d+(?=:)")),
    # Create a simple numeric proxy for time (hours + minutes/60)
    Time_proxy = Hours + Minutes_part / 60
  ) %>%
  # Final mutate for variables that depend on Time_proxy
  mutate(
    # Calculate age-adjusted performance (optional - only if needed for advanced analysis)
    Age_adjusted_time = Time_proxy - (Age - 30) * 0.5
  ) %>%
  # Filter out extreme outliers and data errors - filter after Time_proxy is created
  filter(
    !is.na(Time_proxy),                   # Remove rows where time parsing failed
    Time_proxy > 1.5 & Time_proxy < 8,   # 1.5 to 8 hours reasonable range
    Age >= 16 & Age <= 90,               # Reasonable age range
    Gender %in% c("M", "F")              # Valid gender values
  ) %>%
  # Order performance categories and ensure we have enough data
  mutate(Performance_category = factor(Performance_category, 
                                       levels = c("Elite (Sub-3:00)", "Competitive (3:00-3:30)", 
                                                  "Good (3:30-4:00)", "Average (4:00-4:30)", 
                                                  "Recreational (4:30-5:00)", "Survival (5:00+)"))) %>%
  # Remove any remaining rows with NA values in key columns
  drop_na(Age, Gender, `Finish Time`)

# Check if we have enough data after cleaning
print("Data summary after cleaning:")
print(paste("Total rows:", nrow(marathon_clean)))

# 1. PREDICTIVE MODELING ANALYSIS
# ============================

# Build predictive model for finish time using Time_proxy
set.seed(123)
train_index <- sample(nrow(marathon_clean), 0.8 * nrow(marathon_clean))
train_data <- marathon_clean[train_index, ]
test_data <- marathon_clean[-train_index, ]

# Multiple regression model using Time_proxy instead of Time_minutes
performance_model <- lm(Time_proxy ~ Age + Gender_binary + I(Age^2), data = train_data)
model_summary <- summary(performance_model)

# Make predictions
test_data$Predicted_time <- predict(performance_model, test_data)

# Calculate model accuracy
model_accuracy <- test_data %>%
  summarise(
    RMSE = sqrt(mean((Time_proxy - Predicted_time)^2, na.rm = TRUE)),
    MAE = mean(abs(Time_proxy - Predicted_time), na.rm = TRUE),
    R_squared = cor(Time_proxy, Predicted_time, use = "complete.obs")^2
  )

# Random Forest for feature importance using Time_proxy
rf_model <- randomForest(Time_proxy ~ Age + Gender_binary, 
                         data = train_data, 
                         importance = TRUE)

# ============================
# 2. VISUALIZATION 1: Model Performance (Only if model exists)
# ============================

if(exists("test_data") && nrow(test_data) > 0) {
  # Prediction accuracy plot
  p1 <- ggplot(test_data, aes(x = Predicted_time, y = Time_proxy)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
    labs(title = "Predictive Model Performance",
         subtitle = paste("R² =", round(model_accuracy$R_squared, 3), 
                          "| RMSE =", round(model_accuracy$RMSE, 2), "hours"),
         x = "Predicted Finish Time (hours)",
         y = "Actual Finish Time (hours)") +
    theme(aspect.ratio = 1)
  
  # Feature importance plot
  importance_df <- data.frame(
    Variable = c("Age", "Gender", "Age²"),
    Importance = c(abs(coef(performance_model)[2]), 
                   abs(coef(performance_model)[3]), 
                   abs(coef(performance_model)[4]))
  )
  
  p2 <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = "darkgreen", alpha = 0.7) +
    coord_flip() +
    labs(title = "Feature Importance in Performance Prediction",
         subtitle = "Absolute coefficient values from linear regression",
         x = "Variables",
         y = "Importance (Absolute Coefficient)")
} else {
  # Create placeholder plots if no model data
  p1 <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for model\nanalysis", 
             size = 8, hjust = 0.5) +
    theme_void() + 
    labs(title = "Predictive Model Performance - Insufficient Data")
  
  p2 <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for feature\nimportance analysis", 
             size = 8, hjust = 0.5) +
    theme_void() + 
    labs(title = "Feature Importance - Insufficient Data")
}

# ============================
# 3. AGE-GROUP PERFORMANCE BENCHMARKING
# ============================

if(nrow(marathon_clean) > 0) {
  # Calculate percentiles by age group and gender using Time_proxy
  percentiles <- marathon_clean %>%
    group_by(Age_group, Gender) %>%
    summarise(
      Count = n(),
      P10 = quantile(Time_proxy, 0.1, na.rm = TRUE),
      P25 = quantile(Time_proxy, 0.25, na.rm = TRUE),
      P50 = quantile(Time_proxy, 0.5, na.rm = TRUE),
      P75 = quantile(Time_proxy, 0.75, na.rm = TRUE),
      P90 = quantile(Time_proxy, 0.9, na.rm = TRUE),
      Mean = mean(Time_proxy, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Age performance decline analysis using Time_proxy
  age_performance <- marathon_clean %>%
    group_by(Age) %>%
    summarise(
      Count = n(),
      Median_time = median(Time_proxy, na.rm = TRUE),
      Mean_time = mean(Time_proxy, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(Count >= 5)  # Lower threshold for small datasets
} else {
  percentiles <- data.frame()
  age_performance <- data.frame()
}

# ============================
# 4. VISUALIZATION 2: Age-Group Benchmarking
# ============================

if(nrow(percentiles) > 0) {
  # Age group performance distribution
  p3 <- ggplot(percentiles, aes(x = Age_group, fill = Gender)) +
    geom_col(aes(y = P50), position = "dodge", alpha = 0.7) +
    geom_errorbar(aes(ymin = P25, ymax = P75), 
                  position = position_dodge(width = 0.9), 
                  width = 0.2) +
    scale_fill_manual(values = c("F" = "#FF6B6B", "M" = "#4ECDC4")) +
    labs(title = "Performance Benchmarks by Age Group",
         subtitle = "Median times with 25th-75th percentile ranges",
         x = "Age Group",
         y = "Finish Time (hours)",
         fill = "Gender") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  p3 <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for age\ngroup analysis", 
             size = 8, hjust = 0.5) +
    theme_void() + 
    labs(title = "Age Group Performance - Insufficient Data")
}

if(nrow(age_performance) > 0) {
  # Age decline curve
  p4 <- ggplot(age_performance, aes(x = Age, y = Median_time)) +
    geom_point(aes(size = Count), alpha = 0.6, color = "darkblue") +
    geom_smooth(method = "loess", se = TRUE, color = "red", size = 1) +
    scale_size_continuous(range = c(1, 5)) +
    labs(title = "Performance Decline with Age",
         subtitle = "Each point represents median time for that age (size = sample size)",
         x = "Age (years)",
         y = "Median Finish Time (hours)",
         size = "Number of\nRunners")
} else {
  p4 <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for age\ndecline analysis", 
             size = 8, hjust = 0.5) +
    theme_void() + 
    labs(title = "Age Performance Decline - Insufficient Data")
}

# ============================
# 5. PERFORMANCE CATEGORY ANALYSIS (Enhanced with geographic data)
# ============================

if(nrow(marathon_clean) > 0) {
  # Performance category distribution
  category_stats <- marathon_clean %>%
    group_by(Performance_category, Gender) %>%
    summarise(
      Count = n(),
      Percentage = n() / nrow(marathon_clean) * 100,
      .groups = 'drop'
    )
  
  # Age distribution within performance categories
  age_by_performance <- marathon_clean %>%
    group_by(Performance_category, Age_group) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    group_by(Performance_category) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Geographic analysis using State/Country columns
  geographic_stats <- marathon_clean %>%
    group_by(Performance_category) %>%
    summarise(
      US_runners = sum(!is.na(State)),
      International_runners = sum(is.na(State) & !is.na(Country)),
      International_pct = ifelse((US_runners + International_runners) > 0,
                                 International_runners / (US_runners + International_runners) * 100, 0),
      .groups = 'drop'
    )
} else {
  category_stats <- data.frame()
  age_by_performance <- data.frame()
  geographic_stats <- data.frame()
}

# ============================
# 6. VISUALIZATION 3: Performance Categories
# ============================

if(nrow(category_stats) > 0) {
  # Performance category distribution
  p5 <- ggplot(category_stats, aes(x = Performance_category, y = Count, fill = Gender)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c("F" = "#FF6B6B", "M" = "#4ECDC4")) +
    labs(title = "Runner Distribution by Performance Category",
         subtitle = "Market segmentation for training services",
         x = "Performance Category",
         y = "Number of Runners",
         fill = "Gender") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  p5 <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for performance\ncategory analysis", 
             size = 8, hjust = 0.5) +
    theme_void() + 
    labs(title = "Performance Categories - Insufficient Data")
}

if(nrow(age_by_performance) > 0) {
  # Age composition within performance categories
  p6 <- ggplot(age_by_performance, aes(x = Performance_category, y = Percentage, fill = Age_group)) +
    geom_col(position = "stack", alpha = 0.8) +
    scale_fill_viridis_d(option = "plasma") +
    labs(title = "Age Composition by Performance Level",
         subtitle = "Target demographics for different training programs",
         x = "Performance Category",
         y = "Percentage of Category",
         fill = "Age Group") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  p6 <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for age\ncomposition analysis", 
             size = 8, hjust = 0.5) +
    theme_void() + 
    labs(title = "Age Composition - Insufficient Data")
}

# ============================
# 7. TRAINING ZONE RECOMMENDATIONS
# ============================

if(nrow(marathon_clean) > 0) {
  # Calculate training zones based on performance using Time_proxy (in hours)
  training_zones <- marathon_clean %>%
    group_by(Performance_category) %>%
    summarise(
      Marathon_pace = mean(Time_proxy, na.rm = TRUE),
      Easy_pace = Marathon_pace * 1.3,      # 30% slower than marathon pace
      Tempo_pace = Marathon_pace * 0.9,     # 10% faster than marathon pace
      Interval_pace = Marathon_pace * 0.8,  # 20% faster than marathon pace
      .groups = 'drop'
    ) %>%
    pivot_longer(cols = c(Marathon_pace, Easy_pace, Tempo_pace, Interval_pace),
                 names_to = "Training_zone",
                 values_to = "Pace_hours")
} else {
  training_zones <- data.frame()
}

# ============================
# 8. VISUALIZATION 4: Training Recommendations
# ============================

if(nrow(training_zones) > 0) {
  # Training zone recommendations
  p7 <- ggplot(training_zones, aes(x = Performance_category, y = Pace_hours, fill = Training_zone)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c("Marathon_pace" = "#FF6B6B", 
                                 "Easy_pace" = "#4ECDC4", 
                                 "Tempo_pace" = "#45B7D1", 
                                 "Interval_pace" = "#96CEB4")) +
    labs(title = "Training Zone Recommendations by Performance Level",
         subtitle = "Training paces for different runner categories",
         x = "Performance Category",
         y = "Training Pace (hours)",
         fill = "Training Zone") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  p7 <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for training\nzone analysis", 
             size = 8, hjust = 0.5) +
    theme_void() + 
    labs(title = "Training Zones - Insufficient Data")
}

# ============================
# 9. BUSINESS VALUE SUMMARY (Enhanced with temporal and geographic insights)
# ============================

if(nrow(marathon_clean) > 0) {
  # Calculate key business metrics
  business_metrics <- marathon_clean %>%
    group_by(Performance_category) %>%
    summarise(
      Count = n(),
      Market_share = n() / nrow(marathon_clean) * 100,
      Avg_age = mean(Age, na.rm = TRUE),
      Gender_ratio_male = sum(Gender == "M") / n() * 100,
      Avg_overall_rank = mean(Overall, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Temporal trends using Year column
  temporal_trends <- marathon_clean %>%
    group_by(Year, Performance_category) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    group_by(Year) %>%
    mutate(Yearly_percentage = Count / sum(Count) * 100)
} else {
  business_metrics <- data.frame()
  temporal_trends <- data.frame()
}

# Market opportunity analysis (enhanced)
market_opportunity <- data.frame(
  Category = c("Beginner Programs", "Intermediate Coaching", "Advanced Training", 
               "Age-Group Specialists", "International Coaching"),
  Target_segment = c("Recreational & Survival", "Average & Good", "Competitive & Elite", 
                     "55+ Age Groups", "International Participants"),
  Market_size_pct = c(45, 35, 15, 20, 25),
  Revenue_potential = c("High Volume, Low Price", "Medium Volume, Medium Price", 
                        "Low Volume, High Price", "Specialized Premium", "Premium Online")
)

# ============================
# 10. DISPLAY ALL VISUALIZATIONS
# ============================

# Display model performance plots
print("=== PREDICTIVE MODEL PERFORMANCE ===")
print(p1)
print(p2)

# Display age-group benchmarking
print("=== AGE-GROUP PERFORMANCE BENCHMARKING ===")
print(p3)
print(p4)

# Display performance categories
print("=== PERFORMANCE CATEGORY ANALYSIS ===")
print(p5)
print(p6)

# Display training recommendations
print("=== TRAINING ZONE RECOMMENDATIONS ===")
print(p7)