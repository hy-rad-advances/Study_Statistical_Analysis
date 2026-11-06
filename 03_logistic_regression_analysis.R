# ============================================================
# YEAR-ADJUSTED LOGISTIC REGRESSION: LOW- VS HIGH-YIELD RADIOGRAPHS
# MODEL 1: YEAR CONTINUOUS
# MODEL 2: YEAR CATEGORICAL
# ============================================================
# Run from the RStudio project root directory.
# Input files are produced or used by the preceding analysis script.

# ============================================================
# 01. LOAD AND STANDARDIZE ANALYSIS DATA
# ============================================================

# Load low-yield data
low_yield <- read.csv("input/low_yield_2019_2022_subset.csv")

# Load high-yield data
high_yield <- read.csv("output/high_yield_rescreened.csv")

# Standardize low-yield CPT codes
low_yield$code <- trimws(as.character(low_yield$code))

# Standardize high-yield CPT codes
high_yield$code <- trimws(as.character(high_yield$code))

# Convert low-yield year to numeric
low_yield$year <- as.numeric(as.character(low_yield$year))

# Convert high-yield year to numeric
high_yield$year <- as.numeric(as.character(high_yield$year))

# ============================================================
# 02. DEFINE THE SEVEN ANATOMICAL COMPARISONS
# ============================================================

# Define the seven comparisons using CPT codes
pairs <- list(
  "Ribs vs Chest" = list(low = c("71100", "71101", "71110", "71111"), high = "71046"),
  "Sternum vs Chest" = list(low = "71120", high = "71046"),
  "SC joint vs Clavicle" = list(low = "71130", high = "73000"),
  "Scapula vs Shoulder" = list(low = "73010", high = "73030"),
  "Sacrum/Coccyx vs Lumbar 2-view" = list(low = c("72200", "72202", "72220"), high = "72100"),
  "Sacrum/Coccyx vs Lumbar 4-view" = list(low = c("72200", "72202", "72220"), high = "72110"),
  "Sacrum/Coccyx vs Pelvis" = list(low = c("72200", "72202", "72220"), high = c("72170", "72190"))
)

# ============================================================
# 03. GENERATE HEDGE-TERM CATEGORY FLAGS
# ============================================================

# Identify low-yield uncertainty
low_yield$uncertainty_flag <- grepl("uncertainty", low_yield$hedge_term_category, ignore.case = TRUE)

# Identify low-yield limitation
low_yield$limitation_flag <- grepl("limitation", low_yield$hedge_term_category, ignore.case = TRUE)

# Identify low-yield follow-up/recommendation
low_yield$followup_flag <- grepl("follow-up/\\s*recommendation", low_yield$hedge_term_category, ignore.case = TRUE)

# Identify high-yield uncertainty
high_yield$uncertainty_flag <- grepl("uncertainty", high_yield$hedge_term_category, ignore.case = TRUE)

# Identify high-yield limitation
high_yield$limitation_flag <- grepl("limitation", high_yield$hedge_term_category, ignore.case = TRUE)

# Identify high-yield follow-up/recommendation
high_yield$followup_flag <- grepl("follow-up/\\s*recommendation", high_yield$hedge_term_category, ignore.case = TRUE)

# Preserve missing low-yield outcomes
low_yield$uncertainty_flag[is.na(low_yield$term_flag)] <- NA

# Preserve missing low-yield limitation
low_yield$limitation_flag[is.na(low_yield$term_flag)] <- NA

# Preserve missing low-yield follow-up
low_yield$followup_flag[is.na(low_yield$term_flag)] <- NA

# Preserve missing high-yield outcomes
high_yield$uncertainty_flag[is.na(high_yield$term_flag)] <- NA

# Preserve missing high-yield limitation
high_yield$limitation_flag[is.na(high_yield$term_flag)] <- NA

# Preserve missing high-yield follow-up
high_yield$followup_flag[is.na(high_yield$term_flag)] <- NA

# ============================================================
# 04. DEFINE THE FOUR HEDGE-TERM OUTCOMES
# ============================================================

# Match each outcome name to its column
outcomes <- c(
  "Any hedge" = "term_flag",
  "Uncertainty" = "uncertainty_flag",
  "Limitation" = "limitation_flag",
  "Follow-up/Recommendation" = "followup_flag"
)

# ============================================================
# 05. FIT CONTINUOUS- AND CATEGORICAL-YEAR MODELS
# ============================================================

# Create an empty list for continuous-year results
results_continuous <- list()

# Create an empty list for categorical-year results
results_categorical <- list()

# Loop through the four hedge outcomes
for (outcome_name in names(outcomes)) {

  # Identify the outcome column
  flag <- outcomes[[outcome_name]]

  # Loop through the seven anatomical comparisons
  for (pair_name in names(pairs)) {

    # Select low-yield reports with matching CPT codes and known outcomes
    low <- low_yield[
      low_yield$code %in% pairs[[pair_name]]$low &
        low_yield$year %in% c(2019, 2022) &
        !is.na(low_yield[[flag]]),
      ,
      drop = FALSE
    ]

    # Select high-yield reports with matching CPT codes and known outcomes
    high <- high_yield[
      high_yield$code %in% pairs[[pair_name]]$high &
        high_yield$year %in% c(2019, 2022) &
        !is.na(high_yield[[flag]]),
      ,
      drop = FALSE
    ]

    # Create a binary outcome for low-yield reports
    low$outcome <- as.integer(low[[flag]])

    # Create a binary outcome for high-yield reports
    high$outcome <- as.integer(high[[flag]])

    # Label low-yield reports as 1
    low$low_yield_group <- 1

    # Label high-yield reports as 0
    high$low_yield_group <- 0

    # Combine the two groups
    data <- rbind(
      low[, c("outcome", "low_yield_group", "year")],
      high[, c("outcome", "low_yield_group", "year")]
    )

    # Create a continuous year variable starting at 2019
    data$year_continuous <- data$year - 2019

    # Create a categorical year variable with 2019 as reference
    data$year_categorical <- factor(data$year, levels = c(2019, 2022))

    # Fit logistic regression with continuous year
    model_continuous <- glm(
      outcome ~ low_yield_group + year_continuous,
      data = data,
      family = binomial
    )

    # Fit logistic regression with categorical year
    model_categorical <- glm(
      outcome ~ low_yield_group + year_categorical,
      data = data,
      family = binomial
    )

    # Extract the low-yield coefficient from the continuous-year model
    beta_cont <- coef(summary(model_continuous))["low_yield_group", ]

    # Extract the low-yield coefficient from the categorical-year model
    beta_cat <- coef(summary(model_categorical))["low_yield_group", ]

    # Save the continuous-year adjusted odds ratio and statistics
    results_continuous[[paste(outcome_name, pair_name)]] <- data.frame(
      outcome = outcome_name,
      comparison = pair_name,
      low_n = nrow(low),
      high_n = nrow(high),
      adjusted_or = exp(beta_cont["Estimate"]),
      ci_lower = exp(beta_cont["Estimate"] - 1.96 * beta_cont["Std. Error"]),
      ci_upper = exp(beta_cont["Estimate"] + 1.96 * beta_cont["Std. Error"]),
      p_value = beta_cont["Pr(>|z|)"]
    )

    # Save the categorical-year adjusted odds ratio and statistics
    results_categorical[[paste(outcome_name, pair_name)]] <- data.frame(
      outcome = outcome_name,
      comparison = pair_name,
      low_n = nrow(low),
      high_n = nrow(high),
      adjusted_or = exp(beta_cat["Estimate"]),
      ci_lower = exp(beta_cat["Estimate"] - 1.96 * beta_cat["Std. Error"]),
      ci_upper = exp(beta_cat["Estimate"] + 1.96 * beta_cat["Std. Error"]),
      p_value = beta_cat["Pr(>|z|)"]
    )
  }
}

# ============================================================
# 06. COMBINE REGRESSION RESULTS
# ============================================================

# Combine continuous-year results into one table
continuous_table <- do.call(rbind, results_continuous)

# Remove continuous-year row names
rownames(continuous_table) <- NULL

# Combine categorical-year results into one table
categorical_table <- do.call(rbind, results_categorical)

# Remove categorical-year row names
rownames(categorical_table) <- NULL

# ============================================================
# 07. VERIFY MODEL RESULTS
# ============================================================

# Confirm that each model produced 28 results
stopifnot(nrow(continuous_table) == 28)

# Confirm that each categorical-year model produced 28 results
stopifnot(nrow(categorical_table) == 28)

# Check that both models used identical sample sizes
stopifnot(all(continuous_table$low_n == categorical_table$low_n))

# Check that both models used identical high-yield sample sizes
stopifnot(all(continuous_table$high_n == categorical_table$high_n))

# Check that both models produce the same adjusted odds ratios
stopifnot(isTRUE(all.equal(
  continuous_table$adjusted_or,
  categorical_table$adjusted_or
)))

# Display confirmation
print("Both models produced matching results.")

# ============================================================
# 08. SAVE AND DISPLAY REGRESSION RESULTS
# ============================================================

# Create the output folder if needed
dir.create("output", recursive = TRUE, showWarnings = FALSE)

# Save continuous-year logistic regression results
write.csv(
  continuous_table,
  "output/logistic_regression_year_continuous.csv",
  row.names = FALSE
)

# Save categorical-year logistic regression results
write.csv(
  categorical_table,
  "output/logistic_regression_year_categorical.csv",
  row.names = FALSE
)

# Display continuous-year results
print(continuous_table)

# Display categorical-year results
print(categorical_table)
