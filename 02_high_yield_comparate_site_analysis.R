# ============================================================
# HIGH-YIELD AND LOW-YIELD RADIOGRAPH COMPARISON ANALYSES
# ============================================================
# Run from the RStudio project root directory.
# External source CSV files are stored locally in input/.
# Intermediate datasets and results are written locally to output/.
# Requires output/year_extracted_data.csv from the preceding analysis.
# No statistical calculations or verification thresholds were changed.

dir.create("output", recursive = TRUE, showWarnings = FALSE)


# 01. SELECT 2019 AND 2022 LOW-YIELD RECORDS
# Read the dataset containing corrected years and anatomical classifications
data <- read.csv("output/year_extracted_data.csv")


# Keep only records from 2019 and 2022
data_hy <- data[data$year %in% c(2019, 2022), , drop = FALSE]

# Display the first 10 dates in the filtered dataset
head(data_hy$date, 10)

# Check the number of unique dates in each selected year
tapply(data_hy$date, data_hy$year, function(x) length(unique(x)))

# Save the filtered data to the high-yield output folder
write.csv(
  data_hy,
  "output/low_yield_2019_2022_subset.csv",
  row.names = FALSE
)




# 02. COMBINE HIGH-YIELD ANATOMICAL DATASETS
# Read the chest radiograph dataset
chest <- read.csv("input/chest_data.csv")

# Read the clavicle radiograph dataset
clavicle <- read.csv("input/clavicle_data.csv")

# Read the two-view lumbar spine dataset
lumbar2 <- read.csv("input/lumbar_2view_data.csv")

# Read the four-view lumbar spine dataset
lumbar4 <- read.csv("input/lumbar_4view_data.csv")

# Read the pelvis radiograph dataset
pelvis <- read.csv("input/pelvis_data.csv")

# Read the shoulder radiograph dataset
shoulder <- read.csv("input/shoulder_data.csv")

# Add an anatomical site label to chest radiographs
chest$anatomical_site <- "Chest"

# Add an anatomical site label to clavicle radiographs
clavicle$anatomical_site <- "Clavicle"

# Add an anatomical site label to two-view lumbar spine radiographs
lumbar2$anatomical_site <- "Lumbar spine 2 view"

# Add an anatomical site label to four-view lumbar spine radiographs
lumbar4$anatomical_site <- "Lumbar spine 4 view"

# Add an anatomical site label to pelvis radiographs
pelvis$anatomical_site <- "Pelvis"

# Add an anatomical site label to shoulder radiographs
shoulder$anatomical_site <- "Shoulder"

# Combine all six datasets into one master dataframe
high_yield <- rbind(chest, clavicle, lumbar2, lumbar4, pelvis, shoulder)

# Display the number of radiographs at each anatomical site
table(high_yield$anatomical_site)

# Display the number of radiographs from 2019 and 2022
table(high_yield$year)

# Display the total number of radiographs
nrow(high_yield)


# Save all high-yield radiographs into one master CSV
write.csv(
  high_yield,
  "output/high_yield_combined.csv",
  row.names = FALSE
)


# 03. FILTER HIGH-YIELD RECORDS TO 2019 AND 2022
# Keep only radiographs from 2019 and 2022
high_yield <- high_yield[high_yield$year %in% c(2019, 2022), , drop = FALSE]

# Check that only 2019 and 2022 remain
table(high_yield$year)

# Check the number of radiographs in each anatomical site
table(high_yield$anatomical_site)

# Check radiograph counts by anatomical site and year
table(high_yield$anatomical_site, high_yield$year)

# Check the total number of remaining radiographs
nrow(high_yield)

# Save the filtered dataset as a new CSV file
write.csv(high_yield, "output/high_yield_filtered.csv", row.names = FALSE)





# Read the filtered high-yield dataset
high_yield <- read.csv("output/high_yield_filtered.csv")

# 04. EXTRACT REPORT TEXT FOLLOWING COMPARISON

# Find the word COMPARISON regardless of capitalization
comparison_position <- regexpr("\\bCOMPARISON\\b", high_yield$text, ignore.case = TRUE)

# Copy the original report text into the new column
high_yield$text_after_comparison <- high_yield$text

# Identify reports containing the word COMPARISON
has_comparison <- !is.na(comparison_position) & comparison_position > 0

# For reports with COMPARISON, extract everything after that word
high_yield$text_after_comparison[has_comparison] <- substring(
  high_yield$text[has_comparison],
  comparison_position[has_comparison] + attr(comparison_position, "match.length")[has_comparison]
)

# Remove leading and trailing spaces
high_yield$text_after_comparison <- trimws(high_yield$text_after_comparison)

# Count reports containing COMPARISON
sum(has_comparison)

# Count reports without COMPARISON
sum(!has_comparison & !is.na(high_yield$text))

# Display the first six extracted reports
head(high_yield$text_after_comparison)

# Save the updated dataset
write.csv(
  high_yield,
  "output/high_yield_text_extracted.csv",
  row.names = FALSE
)



# 05. EXCLUDE RECORDS WITH EMPTY REPORT TEXT

# Load the dataset with text_after_comparison already extracted
high_yield <- read.csv("output/high_yield_text_extracted.csv")

# Count the original number of records
nrow(high_yield)

# Keep only records with populated radiology reports in the text column
high_yield <- high_yield[!is.na(high_yield$text) & trimws(high_yield$text) != "", , drop = FALSE]

# Count the number of remaining records
nrow(high_yield)

# Check the number of reports by anatomical site
table(high_yield$anatomical_site)

# Check the number of reports by year
table(high_yield$year)

# Save the cleaned dataset as a new CSV file
write.csv(
  high_yield,
  "output/high_yield_populated_reports.csv",
  row.names = FALSE
)




# 06. RESCREEN HIGH-YIELD REPORTS FOR HEDGE TERMS
# Read the cleaned high-yield dataset
high_yield <- read.csv("output/high_yield_populated_reports.csv")

# Identify the old hedge-term screening columns
old_columns <- c(
  "term_found",
  "uncertainty_term_found",
  "limitation_term_found",
  "followup_term_found",
  "term_flag",
  "uncertainty_flag",
  "limitation_flag",
  "followup_flag",
  "hedge_term_category"
)

# Remove the old screening columns while preserving all other variables
high_yield <- high_yield[, !names(high_yield) %in% old_columns, drop = FALSE]

# Confirm the report text column exists
stopifnot("text_after_comparison" %in% names(high_yield))



# Define uncertainty-related hedge terms
uncertainty_terms <- c(
  "possibly", "possible", "possibility", "suspicious", "could",
  "suggestive", "suggests", "may", "might", "cannot exclude",
  "cannot rule out", "difficult to exclude", "difficult to rule out",
  "not excluded", "concerning", "nonspecific", "equivocal",
  "indeterminate", "unclear", "questionable"
)

# Define limitation-related hedge terms
limitation_terms <- c(
  "limited", "suboptimal", "degraded", "poorly", "poor",
  "incomplete", "incompletely", "artifact", "positioning",
  "habitus", "obscured", "overlap", "overlaps", "overlapping",
  "overlapped", "overlying"
)

# Define follow-up and recommendation hedge terms
followup_terms <- c(
  "correlation", "correlate", "follow-up", "followup", "follow up",
  "repeat", "consider", "recommend", "recommends", "recommended",
  "recommending", "recommendation", "recommendations",
  "recommend CT", "recommend MRI", "further imaging",
  "further evaluation"
)



# Combine all hedge terms into one dictionary
all_terms <- c(uncertainty_terms, limitation_terms, followup_terms)

# Assign a category to each hedge term
all_categories <- c(
  rep("Uncertainty", length(uncertainty_terms)),
  rep("Limitation", length(limitation_terms)),
  rep("Follow-up/Recommendation", length(followup_terms))
)

# Create regular expressions for whole-word matching
patterns <- paste0("\\b", gsub(" ", "\\\\s+", all_terms), "\\b")

# Create an empty column for detected hedge terms
high_yield$term_found <- NA_character_

# Create an empty column for hedge-term presence
high_yield$term_flag <- NA

# Create an empty column for detected hedge-term categories
high_yield$hedge_term_category <- NA_character_

# Loop through each radiology report
for (i in seq_len(nrow(high_yield))) {
  
  # Get the extracted radiology report text
  report <- high_yield$text_after_comparison[i]
  
  # Skip reports with missing or empty extracted text
  if (is.na(report) || trimws(report) == "") next
  
  # Identify all hedge terms present in the report
  matches <- vapply(patterns, grepl, logical(1), x = report, ignore.case = TRUE, perl = TRUE)
  
  # Record whether any hedge term was found
  high_yield$term_flag[i] <- any(matches)
  
  # Record matching terms and categories when hedge terms are present
  if (any(matches)) {
    
    # Save all unique matched hedge terms
    high_yield$term_found[i] <- paste(unique(all_terms[matches]), collapse = "; ")
    
    # Save all unique matched hedge-term categories
    high_yield$hedge_term_category[i] <- paste(unique(all_categories[matches]), collapse = "; ")
  }
}



# Count reports with and without hedge terms
table(high_yield$term_flag, useNA = "always")

# Count reports containing each combination of hedge-term categories
table(high_yield$hedge_term_category, useNA = "always")

# Count hedge-positive reports by anatomical site
table(high_yield$anatomical_site, high_yield$term_flag, useNA = "always")

# Count hedge-positive reports by year
table(high_yield$year, high_yield$term_flag, useNA = "always")

# Display the first six screening results
head(high_yield[, c("text_after_comparison", "term_found", "term_flag", "hedge_term_category")])



# Replace missing values in term_found with blank cells
high_yield$term_found[is.na(high_yield$term_found)] <- ""

# Replace missing values in hedge_term_category with blank cells
high_yield$hedge_term_category[is.na(high_yield$hedge_term_category)] <- ""

# Save the updated dataset
write.csv(
  high_yield,
  "output/high_yield_rescreened.csv",
  row.names = FALSE
)




# ============================================================
# 07. UNADJUSTED ANY-HEDGE COMPARISONS
# ============================================================

# Clear all objects from the R environment
rm(list = ls())


# Read the comparison low-yield dataset
low_yield <- read.csv("input/low_yield_2019_2022_subset.csv")

# Read the comparison high-yield dataset
high_yield <- read.csv("output/high_yield_rescreened.csv")

# Standardize CPT codes in both datasets
low_yield$code <- trimws(as.character(low_yield$code))
high_yield$code <- trimws(as.character(high_yield$code))

# Standardize years in both datasets
low_yield$year <- as.integer(as.character(low_yield$year))
high_yield$year <- as.integer(as.character(high_yield$year))

# Define all seven anatomical comparisons
pairs <- list(
  "Ribs vs Chest" = list(low = c("71100", "71101", "71110", "71111"), high = "71046"),
  "Sternum vs Chest" = list(low = "71120", high = "71046"),
  "SC joint vs Clavicle" = list(low = "71130", high = "73000"),
  "Scapula vs Shoulder" = list(low = "73010", high = "73030"),
  "Sacrum/Coccyx vs Lumbar 2-view" = list(low = c("72200", "72202", "72220"), high = "72100"),
  "Sacrum/Coccyx vs Lumbar 4-view" = list(low = c("72200", "72202", "72220"), high = "72110"),
  "Sacrum/Coccyx vs Pelvis" = list(low = c("72200", "72202", "72220"), high = c("72170", "72190"))
)

# Create an empty list to store the results
results <- list()

# Calculate results for each anatomical comparison
for (pair_name in names(pairs)) {
  
  # Select eligible low-yield reports
  low <- low_yield[
    low_yield$code %in% pairs[[pair_name]]$low &
      low_yield$year %in% c(2019, 2022) &
      !is.na(low_yield$term_flag),
    ,
    drop = FALSE
  ]
  
  # Select eligible high-yield reports
  high <- high_yield[
    high_yield$code %in% pairs[[pair_name]]$high &
      high_yield$year %in% c(2019, 2022) &
      !is.na(high_yield$term_flag),
    ,
    drop = FALSE
  ]
  
  # Count hedge-positive and hedge-negative reports
  low_yes <- sum(low$term_flag == TRUE)
  low_no <- sum(low$term_flag == FALSE)
  high_yes <- sum(high$term_flag == TRUE)
  high_no <- sum(high$term_flag == FALSE)
  
  # Construct the contingency table
  tab <- matrix(
    c(low_yes, high_yes, low_no, high_no),
    nrow = 2
  )
  
  # Check that both groups contain eligible reports
  if (any(rowSums(tab) == 0)) {
    stop(paste("No eligible reports for:", pair_name))
  }
  
  # Calculate the unadjusted odds ratio
  or <- (low_yes * high_no) / (low_no * high_yes)
  
  # Calculate expected counts
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  
  # Choose Fisher's exact test when expected counts are small
  use_fisher <- any(expected < 5) || any(tab == 0)
  
  # Calculate the p-value
  if (use_fisher) {
    p_value <- fisher.test(tab)$p.value
    test_name <- "Fisher exact"
  } else {
    p_value <- chisq.test(tab, correct = FALSE)$p.value
    test_name <- "Pearson chi-square"
  }
  
  # Calculate the 95% confidence interval
  if (all(tab > 0)) {
    se <- sqrt(sum(1 / tab))
    ci_lower <- exp(log(or) - 1.96 * se)
    ci_upper <- exp(log(or) + 1.96 * se)
  } else {
    ci <- fisher.test(tab)$conf.int
    ci_lower <- ci[1]
    ci_upper <- ci[2]
  }
  
  # Store the comparison results
  results[[pair_name]] <- data.frame(
    comparison = pair_name,
    low_n = low_yes + low_no,
    low_hedge_n = low_yes,
    low_hedge_percent = round(100 * low_yes / (low_yes + low_no), 2),
    high_n = high_yes + high_no,
    high_hedge_n = high_yes,
    high_hedge_percent = round(100 * high_yes / (high_yes + high_no), 2),
    odds_ratio = round(or, 3),
    ci_lower = round(ci_lower, 3),
    ci_upper = round(ci_upper, 3),
    test = test_name,
    p_value = p_value
  )
}

# Combine all seven results into one data frame
results_table <- do.call(rbind, results)

# Remove row names
rownames(results_table) <- NULL

# Display the full corrected results
print(results_table, row.names = FALSE)

# Save unadjusted comparison results
write.csv(
  results_table,
  "output/any_hedge_comparisons.csv",
  row.names = FALSE
)


# ============================================================
# 08. UNADJUSTED HEDGE-CATEGORY COMPARISONS
# UNCERTAINTY, LIMITATION, FOLLOW-UP/RECOMMENDATION
# ============================================================


# STEP 1: LOAD THE DATA
# Load the low-yield dataset
low_yield <- read.csv("input/low_yield_2019_2022_subset.csv")

# Load the high-yield dataset
high_yield <- read.csv("output/high_yield_rescreened.csv")

# Standardize low-yield CPT codes
low_yield$code <- trimws(as.character(low_yield$code))

# Standardize high-yield CPT codes
high_yield$code <- trimws(as.character(high_yield$code))


# STEP 2: DEFINE THE SEVEN ANATOMICAL COMPARISONS

# Define the low-yield and high-yield CPT codes for each pair
pairs <- list(
  "Ribs vs Chest" = list(low = c("71100", "71101", "71110", "71111"), high = "71046"),
  "Sternum vs Chest" = list(low = "71120", high = "71046"),
  "SC joint vs Clavicle" = list(low = "71130", high = "73000"),
  "Scapula vs Shoulder" = list(low = "73010", high = "73030"),
  "Sacrum/Coccyx vs Lumbar 2-view" = list(low = c("72200", "72202", "72220"), high = "72100"),
  "Sacrum/Coccyx vs Lumbar 4-view" = list(low = c("72200", "72202", "72220"), high = "72110"),
  "Sacrum/Coccyx vs Pelvis" = list(low = c("72200", "72202", "72220"), high = c("72170", "72190"))
)


# STEP 3: CREATE FLAGS FOR THE THREE HEDGE CATEGORIES


# Identify uncertainty in low-yield reports
low_yield$uncertainty_flag <- grepl("uncertainty", low_yield$hedge_term_category, ignore.case = TRUE)

# Identify limitation in low-yield reports
low_yield$limitation_flag <- grepl("limitation", low_yield$hedge_term_category, ignore.case = TRUE)

# Identify follow-up/recommendation in low-yield reports
low_yield$followup_flag <- grepl("follow-up/\\s*recommendation", low_yield$hedge_term_category, ignore.case = TRUE)

# Identify uncertainty in high-yield reports
high_yield$uncertainty_flag <- grepl("uncertainty", high_yield$hedge_term_category, ignore.case = TRUE)

# Identify limitation in high-yield reports
high_yield$limitation_flag <- grepl("limitation", high_yield$hedge_term_category, ignore.case = TRUE)

# Identify follow-up/recommendation in high-yield reports
high_yield$followup_flag <- grepl("follow-up/\\s*recommendation", high_yield$hedge_term_category, ignore.case = TRUE)

# Preserve missing outcomes in low-yield reports
low_yield$uncertainty_flag[is.na(low_yield$term_flag)] <- NA
low_yield$limitation_flag[is.na(low_yield$term_flag)] <- NA
low_yield$followup_flag[is.na(low_yield$term_flag)] <- NA

# Preserve missing outcomes in high-yield reports
high_yield$uncertainty_flag[is.na(high_yield$term_flag)] <- NA
high_yield$limitation_flag[is.na(high_yield$term_flag)] <- NA
high_yield$followup_flag[is.na(high_yield$term_flag)] <- NA


# STEP 4: CHECK THE CATEGORY COUNTS

# Check uncertainty in low-yield reports
table(low_yield$uncertainty_flag, useNA = "always")

# Check limitation in low-yield reports
table(low_yield$limitation_flag, useNA = "always")

# Check follow-up/recommendation in low-yield reports
table(low_yield$followup_flag, useNA = "always")

# Check uncertainty in high-yield reports
table(high_yield$uncertainty_flag, useNA = "always")

# Check limitation in high-yield reports
table(high_yield$limitation_flag, useNA = "always")

# Check follow-up/recommendation in high-yield reports
table(high_yield$followup_flag, useNA = "always")

# Stop if any hedge-positive report has a missing category label
stopifnot(!any(
  low_yield$term_flag %in% TRUE &
    (is.na(low_yield$hedge_term_category) |
       trimws(low_yield$hedge_term_category) == "")
))

# Repeat the check for high-yield reports
stopifnot(!any(
  high_yield$term_flag %in% TRUE &
    (is.na(high_yield$hedge_term_category) |
       trimws(high_yield$hedge_term_category) == "")
))


# STEP 5: DEFINE THE THREE CATEGORIES TO ANALYZE

# Match each category name to its TRUE/FALSE flag
categories <- c(
  "Uncertainty" = "uncertainty_flag",
  "Limitation" = "limitation_flag",
  "Follow-up/Recommendation" = "followup_flag"
)


# STEP 6: RUN CHI-SQUARED OR FISHER'S EXACT TESTS

# Create an empty list to store results
results <- list()

# Loop through the three hedge-term categories
for (category_name in names(categories)) {
  
  # Identify the flag for the current category
  flag <- categories[[category_name]]
  
  # Loop through the seven anatomical comparisons
  for (pair_name in names(pairs)) {
    
    # Select low-yield radiographs with matching CPT codes
    low <- low_yield[low_yield$code %in% pairs[[pair_name]]$low, ]
    
    # Select high-yield radiographs with matching CPT codes
    high <- high_yield[high_yield$code %in% pairs[[pair_name]]$high, ]
    
    # Keep low-yield reports from 2019 and 2022 with known outcomes
    low <- low[low$year %in% c(2019, 2022) & !is.na(low[[flag]]), ]
    
    # Keep high-yield reports from 2019 and 2022 with known outcomes
    high <- high[high$year %in% c(2019, 2022) & !is.na(high[[flag]]), ]
    
    # Count low-yield reports containing the category
    low_yes <- sum(low[[flag]] == TRUE)
    
    # Count high-yield reports containing the category
    high_yes <- sum(high[[flag]] == TRUE)
    
    # Count low-yield reports without the category
    low_no <- sum(low[[flag]] == FALSE)
    
    # Count high-yield reports without the category
    high_no <- sum(high[[flag]] == FALSE)
    
    # Create the 2-by-2 contingency table
    tab <- matrix(c(low_yes, high_yes, low_no, high_no), nrow = 2)
    
    # Label the rows and columns
    dimnames(tab) <- list(
      c("Low-yield", "High-yield"),
      c("Category positive", "Category negative")
    )
    
    # Stop if either group contains no eligible reports
    if (any(rowSums(tab) == 0)) {
      stop(paste("No eligible reports:", pair_name, category_name))
    }
    
    # Calculate expected cell counts
    expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
    
    # Use Fisher's exact test if any expected count is below 5
    use_fisher <- any(expected < 5)
    
    # Calculate Fisher's exact test
    fisher_result <- fisher.test(tab)
    
    # Calculate the p-value using the appropriate test
    p_value <- if (use_fisher) {
      fisher_result$p.value
    } else {
      chisq.test(tab, correct = FALSE)$p.value
    }
    
    # Calculate the unadjusted sample odds ratio
    odds_ratio <- (low_yes * high_no) / (low_no * high_yes)
    
    # Calculate the standard error when all four cells are positive
    se <- if (all(tab > 0)) sqrt(sum(1 / tab)) else NA_real_
    
    # Calculate the lower 95% confidence limit
    ci_lower <- if (is.na(se)) {
      fisher_result$conf.int[1]
    } else {
      exp(log(odds_ratio) - 1.96 * se)
    }
    
    # Calculate the upper 95% confidence limit
    ci_upper <- if (is.na(se)) {
      fisher_result$conf.int[2]
    } else {
      exp(log(odds_ratio) + 1.96 * se)
    }
    
    # Store the results for this comparison
    results[[paste(category_name, pair_name)]] <- data.frame(
      category = category_name,
      comparison = pair_name,
      low_n = nrow(low),
      low_positive_n = low_yes,
      low_percent = round(100 * low_yes / nrow(low), 2),
      high_n = nrow(high),
      high_positive_n = high_yes,
      high_percent = round(100 * high_yes / nrow(high), 2),
      odds_ratio = round(odds_ratio, 3),
      ci_lower = round(ci_lower, 3),
      ci_upper = round(ci_upper, 3),
      test = if (use_fisher) "Fisher exact" else "Pearson chi-square",
      p_value = p_value
    )
  }
}


# STEP 7: COMBINE AND DISPLAY RESULTS

# Combine all 21 comparisons into one dataframe
results_table <- do.call(rbind, results)

# Remove row names
rownames(results_table) <- NULL

# Display the complete results
print(results_table)


# STEP 8: SAVE RESULTS

# Save the results to a CSV file
write.csv(
  results_table,
  "output/hedge_category_comparisons.csv",
  row.names = FALSE
)







# 09. INSPECT CATEGORY LABELS AND SCREENING RESULTS
# Display the names of all columns in the low-yield dataset
names(low_yield)

# Display the unique values in the hedge-term category column
unique(low_yield$hedge_term_category)

# Count the different category values
table(low_yield$hedge_term_category, useNA = "always")

# Display the first 10 hedge-positive reports and their categories
head(
  low_yield[low_yield$term_flag == TRUE,
            c("term_found", "hedge_term_category")],
  10
)









# 10. VERIFY CATEGORY COMPARISON RESULTS
# ============================================================
# VERIFY UNCERTAINTY, LIMITATION, AND FOLLOW-UP RESULTS
# ============================================================


# STEP 1: VERIFY THE NUMBER OF RESULTS

# Confirm that there are 21 results (3 categories x 7 comparisons)
stopifnot(nrow(results_table) == 21)

# Confirm that each category has seven comparisons
table(results_table$category)

# Confirm that each anatomical comparison appears three times
table(results_table$comparison)


# STEP 2: VERIFY CATEGORY FLAGS AGAINST OVERALL HEDGE STATUS

# Check that low-yield category-positive reports are hedge-positive
stopifnot(!any(
  low_yield$term_flag == FALSE &
    (low_yield$uncertainty_flag == TRUE |
       low_yield$limitation_flag == TRUE |
       low_yield$followup_flag == TRUE),
  na.rm = TRUE
))

# Check that high-yield category-positive reports are hedge-positive
stopifnot(!any(
  high_yield$term_flag == FALSE &
    (high_yield$uncertainty_flag == TRUE |
       high_yield$limitation_flag == TRUE |
       high_yield$followup_flag == TRUE),
  na.rm = TRUE
))

# Check that every low-yield hedge-positive report has a category
stopifnot(!any(
  low_yield$term_flag == TRUE &
    !(low_yield$uncertainty_flag |
        low_yield$limitation_flag |
        low_yield$followup_flag),
  na.rm = TRUE
))

# Check that every high-yield hedge-positive report has a category
stopifnot(!any(
  high_yield$term_flag == TRUE &
    !(high_yield$uncertainty_flag |
        high_yield$limitation_flag |
        high_yield$followup_flag),
  na.rm = TRUE
))


# STEP 3: VERIFY MISSING OUTCOMES

# Confirm that low-yield missing outcomes remain missing
stopifnot(all(is.na(low_yield$uncertainty_flag[is.na(low_yield$term_flag)])))

# Confirm that low-yield limitation outcomes remain missing
stopifnot(all(is.na(low_yield$limitation_flag[is.na(low_yield$term_flag)])))

# Confirm that low-yield follow-up outcomes remain missing
stopifnot(all(is.na(low_yield$followup_flag[is.na(low_yield$term_flag)])))

# Confirm that high-yield missing outcomes remain missing
stopifnot(all(is.na(high_yield$uncertainty_flag[is.na(high_yield$term_flag)])))

# Confirm that high-yield limitation outcomes remain missing
stopifnot(all(is.na(high_yield$limitation_flag[is.na(high_yield$term_flag)])))

# Confirm that high-yield follow-up outcomes remain missing
stopifnot(all(is.na(high_yield$followup_flag[is.na(high_yield$term_flag)])))


# STEP 4: VERIFY SAMPLE SIZES AND POSITIVE COUNTS

# Loop through all 21 results
for (i in seq_len(nrow(results_table))) {
  
  # Identify the category and anatomical comparison
  category_name <- results_table$category[i]
  pair_name <- results_table$comparison[i]
  
  # Identify the category flag
  flag <- categories[[category_name]]
  
  # Select eligible low-yield reports
  low <- low_yield[
    low_yield$code %in% pairs[[pair_name]]$low &
      low_yield$year %in% c(2019, 2022) &
      !is.na(low_yield[[flag]]),
    ,
    drop = FALSE
  ]
  
  # Select eligible high-yield reports
  high <- high_yield[
    high_yield$code %in% pairs[[pair_name]]$high &
      high_yield$year %in% c(2019, 2022) &
      !is.na(high_yield[[flag]]),
    ,
    drop = FALSE
  ]
  
  # Verify the low-yield sample size
  stopifnot(nrow(low) == results_table$low_n[i])
  
  # Verify the high-yield sample size
  stopifnot(nrow(high) == results_table$high_n[i])
  
  # Verify the low-yield positive count
  stopifnot(sum(low[[flag]]) == results_table$low_positive_n[i])
  
  # Verify the high-yield positive count
  stopifnot(sum(high[[flag]]) == results_table$high_positive_n[i])
  
  # Verify the low-yield percentage
  stopifnot(
    round(100 * mean(low[[flag]]), 2) == results_table$low_percent[i]
  )
  
  # Verify the high-yield percentage
  stopifnot(
    round(100 * mean(high[[flag]]), 2) == results_table$high_percent[i]
  )
}

# Print confirmation when all count checks pass
print("All sample sizes, positive counts, and percentages match.")


# STEP 5: VERIFY ODDS RATIOS, CONFIDENCE INTERVALS, AND P-VALUES

# Loop through all 21 results
for (i in seq_len(nrow(results_table))) {
  
  # Retrieve positive counts
  a <- results_table$low_positive_n[i]
  c <- results_table$high_positive_n[i]
  
  # Calculate negative counts
  b <- results_table$low_n[i] - a
  d <- results_table$high_n[i] - c
  
  # Recreate the 2-by-2 table
  tab <- matrix(c(a, c, b, d), nrow = 2)
  
  # Recalculate the unadjusted odds ratio
  check_or <- (a * d) / (b * c)
  
  # Verify the reported odds ratio, including zero or infinite values
  stopifnot(isTRUE(all.equal(
    round(check_or, 3),
    results_table$odds_ratio[i]
  )))
  
  # Calculate expected cell counts
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  
  # Determine whether Fisher's exact test is required
  use_fisher <- any(expected < 5)
  
  # Verify which statistical test was selected
  check_test <- if (use_fisher) "Fisher exact" else "Pearson chi-square"
  
  # Compare the test label
  stopifnot(check_test == results_table$test[i])
  
  # Recalculate the p-value
  check_p <- if (use_fisher) {
    fisher.test(tab)$p.value
  } else {
    chisq.test(tab, correct = FALSE)$p.value
  }
  
  # Verify the reported p-value
  stopifnot(isTRUE(all.equal(
    check_p,
    results_table$p_value[i]
  )))
  
  # Recalculate the 95% confidence interval
  if (all(tab > 0)) {
    
    # Calculate the standard error
    se <- sqrt(sum(1 / tab))
    
    # Calculate the Wald confidence interval
    check_ci <- exp(log(check_or) + c(-1, 1) * 1.96 * se)
    
  } else {
    
    # Use Fisher's exact confidence interval for zero-cell tables
    check_ci <- fisher.test(tab)$conf.int
  }
  
  

  # Verify the lower confidence limit
  stopifnot(isTRUE(all.equal(
    round(check_ci[1], 3),
    results_table$ci_lower[i]
  )))
  
  # Verify the upper confidence limit
  stopifnot(isTRUE(all.equal(
    round(check_ci[2], 3),
    results_table$ci_upper[i]
  )))
}

# Print confirmation when all statistical checks pass
print("All odds ratios, confidence intervals, tests, and p-values match.")


# STEP 6: FINAL VERIFICATION MESSAGE

# Print confirmation that all checks completed
print("VERIFICATION COMPLETE: All 21 results passed.")







# Verify expected low-yield uncertainty-positive count
stopifnot(sum(low_yield$uncertainty_flag, na.rm = TRUE) == 1118)

# Verify expected low-yield limitation-positive count
stopifnot(sum(low_yield$limitation_flag, na.rm = TRUE) == 581)

# Verify expected low-yield follow-up-positive count
stopifnot(sum(low_yield$followup_flag, na.rm = TRUE) == 759)

# Verify expected low-yield missing outcome count
stopifnot(sum(is.na(low_yield$term_flag)) == 1568)

# Confirm the overall low-yield category counts
print("Low-yield category totals match the original data.")



# Identify comparisons with zero positive or zero negative counts
zero_cells <- results_table[
  results_table$low_positive_n == 0 |
    results_table$high_positive_n == 0 |
    results_table$low_positive_n == results_table$low_n |
    results_table$high_positive_n == results_table$high_n,
  ,
  drop = FALSE
]

# Display any zero-cell comparisons
print(zero_cells)



