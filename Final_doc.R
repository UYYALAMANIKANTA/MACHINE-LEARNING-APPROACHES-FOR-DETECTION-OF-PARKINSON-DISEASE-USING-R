# ---- SECTION 1: Datawizard - Magic Potions to Clean and Transform Your Data ----

# 1. Load necessary libraries
library(datawizard)  # For various data preparation and exploration functions
library(easystats)   # For statistical analysis and easy access to data exploration functions

# 2. Load the dataset
train_data <- read.table("C:\\Users\\asus\\Desktop\\train_data.txt", header = FALSE, sep = ',')

# 3. Assign column names to the dataset
colnames(train_data) <- c("Subject_id", "Jitter_local", "Jitter_local_absolute", "Jitter_rap", "Jitter_ppq5", "Jitter_ddp", 
                          "Shimmer_local", "Shimmer_local_dB", "Shimmer_apq3", "Shimmer_apq5", "Shimmer_apq11", "Shimmer_dda", 
                          "AC", "NTH", "HTN", "Median_pitch", "Mean_pitch", "Standard_deviation_pitch", "Minimum_pitch", "Maximum_pitch", 
                          "Number_of_pulses", "Number_of_periods", "Mean_period", "Standard_deviation_period", 
                          "Fraction_locally_unvoiced_frames", "Number_of_voice_breaks", "Degree_of_voice_breaks", "UPDRS", "Class")

# 4. Convert 'Class' column to a factor variable (as it is categorical)
train_data$Class <- as.factor(train_data$Class)

# 5. Standardize numeric columns, excluding non-numeric columns like 'Subject_id' and 'Class'
# This can be done using the `standardize` function from the `datawizard` package for all numeric columns
train_data_standardized <- standardize(train_data[, sapply(train_data, is.numeric)])

# 6. Calculate and print skewness and kurtosis for important variables
# Skewness measures asymmetry, and kurtosis measures the "tailedness" of the distribution

# Jitter_local variable analysis
skewness_jitter_local <- skewness(train_data$Jitter_local)
kurtosis_jitter_local <- kurtosis(train_data$Jitter_local)

# Shimmer_local variable analysis
skewness_shimmer_local <- skewness(train_data$Shimmer_local)
kurtosis_shimmer_local <- kurtosis(train_data$Shimmer_local)

# Print the results
cat("Skewness of Jitter_local:", as.numeric(skewness_jitter_local), "\n")
cat("Kurtosis of Jitter_local:", as.numeric(kurtosis_jitter_local), "\n")
cat("Skewness of Shimmer_local:", as.numeric(skewness_shimmer_local), "\n")
cat("Kurtosis of Shimmer_local:", as.numeric(kurtosis_shimmer_local), "\n")

# 7. Peek into the dataset to check for missing values using datawizard function
# The `data_peek` function helps visualize potential issues like missing data
data_peek(train_data)

# 8. Check for duplicate rows in the dataset
# The `data_duplicated` function will identify any exact duplicate rows in the dataset
data_duplicated(train_data)

# 9. View the first few rows of the dataset to inspect the changes
head(train_data)

# 10. Calculate and print basic statistics (mean, standard deviation) for 'Jitter_local' and 'Shimmer_local'
# `mean_sd` will provide the mean and standard deviation of the specified variables
mean_sd(train_data$Jitter_local)
mean_sd(train_data$Shimmer_local)

# 11. Display the distribution of 'Shimmer_local' using the describe_distribution function
# This will provide insights into the distribution of the 'Shimmer_local' variable
describe_distribution(train_data$Shimmer_local)

# 12. Create a data codebook to inspect the variables in the dataset
# This generates a summary of the dataset, including variable names and types
data_codebook(train_data)


# ---- SECTION 2: Correlation - Your All-in-One Package to Run Correlations ----


# We select only the numeric columns from the dataset using sapply(train_data, is.numeric)
# Spearman correlation: non-parametric, based on rank
spearman_corr <- correlation(train_data[, sapply(train_data, is.numeric)], method = "spearman")

# Pearson correlation: parametric, assumes linear relationships between variables
pearson_corr <- correlation(train_data[, sapply(train_data, is.numeric)], method = "pearson")

# Step 2: View the correlation results

# Print the Spearman correlation matrix
cat("Spearman correlation matrix:\n")
print(spearman_corr)

# Print the Pearson correlation matrix
cat("\nPearson correlation matrix:\n")
print(pearson_corr)

# Step 3: Filter for high correlations (absolute value > 0.7)
# We filter for correlation values greater than 0.7 (in absolute value)
# Also exclude self-correlation (where Parameter1 == Parameter2)
high_corr_spearman <- spearman_corr[abs(spearman_corr$r) > 0.7 & spearman_corr$Parameter1 != spearman_corr$Parameter2, ]
high_corr_pearson <- pearson_corr[abs(pearson_corr$r) > 0.7 & pearson_corr$Parameter1 != pearson_corr$Parameter2, ]

# Step 4: Print the filtered results

# Print the highly correlated variable pairs for Spearman correlation
cat("\nHighly correlated variable pairs (Spearman):\n")
print(high_corr_spearman)

# Print the highly correlated variable pairs for Pearson correlation
cat("\nHighly correlated variable pairs (Pearson):\n")
print(high_corr_pearson)


# ---- SECTION 3: Modelbased - Estimate Effects, Group Averages, and Contrasts Between Groups Based on Statistical Models ----

# Load necessary libraries
library(ggplot2)       # For creating plots
library(easystats)     # For estimating means and confidence intervals

# Create a vector of column names for which you want to apply the analysis
columns_of_interest <- c("Jitter_local", "Jitter_local_absolute", "Jitter_rap", "Jitter_ppq5", "Jitter_ddp", 
                         "Shimmer_local", "Shimmer_local_dB", "Shimmer_apq3", "Shimmer_apq5", "Shimmer_apq11", 
                         "Shimmer_dda", "AC", "NTH", "HTN", "Median_pitch", "Mean_pitch", 
                         "Standard_deviation_pitch", "Minimum_pitch", "Maximum_pitch", 
                         "Number_of_pulses", "Number_of_periods", "Mean_period", 
                         "Standard_deviation_period", "Fraction_locally_unvoiced_frames", 
                         "Number_of_voice_breaks", "Degree_of_voice_breaks")

# Loop over each column of interest
for (col in columns_of_interest) {
  
  # Step 1: Fit GLM model for each variable
  model <- glm(as.formula(paste(col, "~ Class")), data = train_data)
  
  # Step 2: Obtain estimated means and confidence intervals
  means <- estimate_means(model)
  
  # Step 3: Create the ggplot for each column
  p <- ggplot(train_data, aes_string(x = "Class", y = col)) + 
    geom_violin(aes(fill = Class)) +                          # Violin plot, colored by Class
    geom_jitter2(width = 0.05, alpha = 0.5) +                  # Add jittered points for better visualization
    geom_line(data = means, aes(y = Mean, group = 1), linewidth = 1) + # Add a line for the estimated means
    geom_pointrange(data = means, aes(y = Mean, ymin = CI_low, ymax = CI_high), size = 1) + # Confidence interval for the means
    scale_fill_material() +                                    # Apply a color palette
    theme_modern() +                                           # Apply a modern theme
    ggtitle(paste("Violin plot for", col))                      # Add a title to the plot
  
  # Step 4: Display the plot
  print(p)
  
  # Step 5: Save the plot to a file (e.g., PNG)
  ggsave(paste0("violin_plot_", col, ".png"), plot = p)
}


# ---- SECTION 4: Effectsize - Compute, Convert, Interpret, and Work with Indices of Effect Size and Standardized Parameters ----

library(effectsize)
library(dplyr)

# Ensure 'Class' is a factor
train_data$Class <- as.factor(train_data$Class)

# List of columns to analyze
columns_to_analyze <- c("Jitter_local", "Jitter_local_absolute", "Jitter_rap", "Jitter_ppq5", "Jitter_ddp", 
                        "Shimmer_local", "Shimmer_local_dB", "Shimmer_apq3", "Shimmer_apq5", "Shimmer_apq11", 
                        "Shimmer_dda", "AC", "NTH", "HTN", "Median_pitch", "Mean_pitch", 
                        "Standard_deviation_pitch", "Minimum_pitch", "Maximum_pitch", 
                        "Number_of_pulses", "Number_of_periods", "Mean_period", 
                        "Standard_deviation_period", "Fraction_locally_unvoiced_frames", 
                        "Number_of_voice_breaks", "Degree_of_voice_breaks")

# Create an empty data frame to store results
effect_sizes_table <- data.frame(
  Variable = character(),
  Cohens_d = numeric(),
  CI_Cohens_d = character(),
  Hedges_g = numeric(),
  CI_Hedges_g = character(),
  Glass_delta = numeric(),
  CI_Glass_delta = character(),
  stringsAsFactors = FALSE
)

# Loop through columns and calculate effect sizes
for (col in columns_to_analyze) {
  # Calculate effect sizes with 95% CI
  cohen_d_result <- cohens_d(as.formula(paste(col, "~ Class")), data = train_data, ci = 0.95)
  hedges_g_result <- hedges_g(as.formula(paste(col, "~ Class")), data = train_data, ci = 0.95)
  glass_delta_result <- glass_delta(as.formula(paste(col, "~ Class")), data = train_data, ci = 0.95)
  
  # Append results to the table
  effect_sizes_table <- effect_sizes_table %>%
    add_row(
      Variable = col,
      Cohens_d = cohen_d_result$Cohens_d,
      CI_Cohens_d = paste0("[", round(cohen_d_result$CI_low, 2), ", ", round(cohen_d_result$CI_high, 2), "]"),
      Hedges_g = hedges_g_result$Hedges_g,
      CI_Hedges_g = paste0("[", round(hedges_g_result$CI_low, 2), ", ", round(hedges_g_result$CI_high, 2), "]"),
      Glass_delta = glass_delta_result$Glass_delta,
      CI_Glass_delta = paste0("[", round(glass_delta_result$CI_low, 2), ", ", round(glass_delta_result$CI_high, 2), "]")
    )
}

# Print the table
print(effect_sizes_table)


# ---- SECTION 5: Parameters - Obtain a Table Containing All Information About the Parameters of Your Models ----

# Load necessary libraries
library(parameters)
library(dplyr)
library(psych)

# 1. **ANOVA for Numeric Columns (Effect Size Calculation)**

# Identify numeric columns in the dataset
numeric_columns <- names(train_data)[sapply(train_data, is.numeric)]

# Initialize a list to store ANOVA results
anova_results <- list()

# Loop through numeric columns to perform ANOVA and extract effect sizes
for (col in numeric_columns) {
  # Fit ANOVA model for each numeric column
  anova_model <- aov(as.formula(paste(col, "~ Class")), data = train_data)
  
  # Extract effect sizes (omega, eta, epsilon)
  anova_params <- parameters(anova_model, es_type = c("omega", "eta", "epsilon"))
  
  # Store results in the list
  anova_results[[col]] <- anova_params
}

# Print ANOVA results for all numeric columns
print("ANOVA Results:")
print(anova_results)

# 2. **Principal Component Analysis (PCA)**

# Exclude non-numeric and categorical columns for PCA
numeric_data <- train_data %>% select(-Subject_id, -Class)

# Perform PCA with 12 factors and Varimax rotation
pca_result <- psych::pca(numeric_data, nfactors = 12, rotate = "varimax")

# Print PCA results
print("PCA Results:")
print(pca_result)

# Extract the PCA loadings
loadings <- as.data.frame(unclass(pca_result$loadings))

# Calculate the importance of each feature (sum of absolute loadings across components)
feature_importance <- rowSums(abs(loadings))

# Select the top 12 most important features based on feature importance
selected_features <- names(sort(feature_importance, decreasing = TRUE)[1:12])

# Print selected features based on PCA importance
print("Selected Features Based on PCA:")
print(selected_features)

# 3. **PCA Analysis - Loadings, Communality, and Uniqueness**

# Add Communality and Uniqueness to the PCA loadings
loadings$Communality <- pca_result$communality  # Communality
loadings$Uniqueness <- 1 - loadings$Communality  # Uniqueness (1 - Communality)

# Print loadings, communality, and uniqueness for each component
print("PCA Loadings, Communality, and Uniqueness:")
print(loadings)

# 4. **PCA Eigenvalues and Variance Explained**

# Extract eigenvalues from PCA result
eigenvalues <- pca_result$values

# Calculate the proportion of variance explained by each component
variance_explained <- eigenvalues / sum(eigenvalues)

# Calculate cumulative variance explained
cumulative_variance <- cumsum(variance_explained)

# Create a table for eigenvalues, variance explained, and cumulative variance
eigenvalues_table <- data.frame(
  Component = paste0("Component_", seq_along(eigenvalues)),
  Eigenvalue = eigenvalues,
  Proportion = variance_explained,
  Cumulative_Proportion = cumulative_variance
)

# Print eigenvalues and variance explained
print("Eigenvalues and Variance Explained:")
print(eigenvalues_table)

# ---- SECTION 6: Insight - For Developers, a Package to Help You Work with Different Models and Packages ----

train_data$Subject_id <- NULL
# Split the dataset into training and testing sets (80% training, 20% testing)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(train_data$Class, p = 0.8, list = FALSE)
train_set <- train_data[train_index, ]
test_set <- train_data[-train_index, ]

# Build a Support Vector Machine (SVM) model using e1071 package
svm_model <- svm(Class ~ ., data = train_set, kernel = "radial")  # Use radial basis function kernel (RBF)

# Make predictions on the test set
predictions_svm <- predict(svm_model, test_set)




# ---- SECTION 7: Performance - Models’ Quality and Performance Metrics (R2, ICC, LOO, AIC, BF, ...) ----

# Confusion Matrix and Accuracy
confusion_matrix_svm <- confusionMatrix(predictions_svm, test_set$Class)
print(confusion_matrix_svm)


# ---- SECTION 8: See - The Plotting Companion to Create Beautiful Results Visualizations ----

# Plot ROC Curve for binary classification
# Convert the factor levels of the target variable into numeric (0 or 1)
test_set$Class_numeric <- ifelse(test_set$Class == levels(test_set$Class)[1], 0, 1)

# Get decision values from SVM model for ROC curve
pred_prob <- attr(predict(svm_model, test_set, decision.values = TRUE), "decision.values")

# Generate and plot the ROC curve
roc_curve <- roc(test_set$Class_numeric, pred_prob)
plot(roc_curve, main = "ROC Curve for SVM Model", col = "blue", lwd = 2)

# Calculate and print AUC value for the ROC curve
auc_value <- auc(test_set$Class_numeric, pred_prob)
print(paste("AUC: ", auc_value))

# Extracting Accuracy, Sensitivity, and Specificity from confusion matrix
accuracy <- confusion_matrix_svm$overall['Accuracy']
sensitivity <- confusion_matrix_svm$byClass['Sensitivity']
specificity <- confusion_matrix_svm$byClass['Specificity']

# Create a dataframe for plotting performance metrics
performance_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity"),
  Value = c(accuracy, sensitivity, specificity)
)

# Visualizing the performance metrics
library(ggplot2)
ggplot(performance_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  labs(title = "SVM Model Performance Metrics", x = "Metric", y = "Value") +
  theme_minimal()



# ---- SECTION 9: Report - Automated Statistical Reporting of Objects in R ----

# Load necessary packages

model <- glm(Class ~ ., data = train_set, family = "binomial")
report(model)









