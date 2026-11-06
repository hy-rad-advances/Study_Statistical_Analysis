# R ANALYSIS CODE — RADIOLOGY HEDGE TERMS AND TEMPORAL TRENDS

# ======================================================================
# 01 | Load source dataset, demographics and screen radiology reports for hedge terms
# ======================================================================
# Read the CSV file and store it as a dataset called data
data <- read.csv("input/source_data.csv")

# Display the first six rows to check that the file loaded correctly
head(data)

# Display the number of rows and columns in the dataset
dim(data)



# ======================================================================
# 01A | Patient demographics: legal sex and age
# ======================================================================

# Display legal sex counts, including missing values
table(data$legal_sex, useNA = "always")

# Calculate legal sex percentages, excluding missing values
round(
  100 * prop.table(table(data$legal_sex)),
  2
)

# Convert age to numeric
data$age <- as.numeric(as.character(data$age))

# Calculate mean age
mean(data$age, na.rm = TRUE)

# Calculate standard deviation of age
sd(data$age, na.rm = TRUE)

# Display mean age and standard deviation together
sprintf(
  "Mean age: %.2f ± %.2f years",
  mean(data$age, na.rm = TRUE),
  sd(data$age, na.rm = TRUE)
)

# Count records with available age
sum(!is.na(data$age))

# ======================================================================
# 01B | Screening for Hedge Terms
# ======================================================================

# Define the uncertainty hedge terms
uncertainty <- c(
  "possibly", "possible", "possibility", "suspicious",
  "could", "suggestive", "suggests", "may", "might",
  "cannot exclude", "cannot rule out",
  "difficult to exclude", "difficult to rule out",
  "not excluded", "concerning", "nonspecific",
  "equivocal", "indeterminate", "unclear", "questionable"
)

# Define the limitation hedge terms
limitation <- c(
  "limited", "suboptimal", "degraded", "poorly",
  "poor", "incomplete", "incompletely", "artifact",
  "positioning", "habitus", "obscured", "overlap",
  "overlaps", "overlapping", "overlapped", "overlying"
)

# Define the follow-up and recommendation hedge terms
follow_up <- c(
  "correlation", "correlate", "follow-up", "followup",
  "follow up", "repeat", "consider", "recommend",
  "recommends", "recommended", "recommending",
  "recommendation", "recommendations", "recommend CT",
  "recommend MRI", "further imaging", "further evaluation"
)

# Combine all hedge terms into one vector
all_terms <- c(uncertainty, limitation, follow_up)

# Create a category label for every hedge term
all_categories <- c(
  rep("uncertainty", length(uncertainty)),
  rep("limitation", length(limitation)),
  rep("follow-up/ recommendation", length(follow_up))
)

# Create a function to screen one radiology report
screen_report <- function(report) {
  
  # Check whether the report is missing or empty
  if (is.na(report) || trimws(report) == "") {
    
    # Return missing values if no report is available
    return(c(NA, NA, NA))
  }
  
  # Convert the report to lowercase for case-insensitive matching
  report <- tolower(report)
  
  # Create a pattern for each term with word boundaries
  patterns <- paste0(
    "\\b",
    gsub(" ", "\\\\s+", all_terms),
    "\\b"
  )
  
  # Check which hedge terms are present in the report
  matches <- sapply(
    patterns,
    function(pattern) grepl(pattern, report, perl = TRUE)
  )
  
  # Extract the specific hedge terms that were found
  found_terms <- all_terms[matches]
  
  # Extract the categories corresponding to the found terms
  found_categories <- unique(all_categories[matches])
  
  # Check whether at least one hedge term was found
  term_flag <- length(found_terms) > 0
  
  # Combine matching terms into one cell, or use NA if none
  term_found <- if (term_flag) {
    paste(found_terms, collapse = "; ")
  } else {
    NA_character_
  }
  
  # Combine matching categories into one cell, or use NA if none
  hedge_term_category <- if (term_flag) {
    paste(found_categories, collapse = "; ")
  } else {
    NA_character_
  }
  
  # Return the three screening results
  c(term_found, as.character(term_flag), hedge_term_category)
}

# Apply the screening function to every report
results <- t(sapply(data$text_after_comparison, screen_report))

# Create a column containing the identified hedge terms
data$term_found <- results[, 1]

# Create a logical column indicating hedge-term presence
# TRUE = hedge term present
# FALSE = no hedge term detected
# NA = missing or empty report
data$term_flag <- as.logical(results[, 2])

# Create a column containing the identified hedge-term categories
data$hedge_term_category <- results[, 3]

# Save the updated dataset as a new CSV file in the same folder
write.csv(
  data,
  "output/screened_data.csv",
  row.names = FALSE,
  na = ""
)

# Display the number of TRUE, FALSE, and missing screening results
table(data$term_flag, useNA = "always")

# Display the number of reports in each category combination
table(data$hedge_term_category, useNA = "always")




# ======================================================================
# 02 | Summarize hedge-term screening results
# ======================================================================
# Count the number of TRUE, FALSE, and missing term_flag values
term_flag_summary <- as.data.frame(table(data$term_flag, useNA = "always"))

# Rename the columns for easier interpretation
colnames(term_flag_summary) <- c("term_flag", "count")

# Save the term_flag summary as a CSV file
write.csv(term_flag_summary, "output/term_flag_summary.csv", row.names = FALSE)

# Count the number of reports in each hedge term category combination
category_summary <- as.data.frame(table(data$hedge_term_category, useNA = "always"))

# Rename the columns for easier interpretation
colnames(category_summary) <- c("hedge_term_category", "count")

# Save the hedge term category summary as a CSV file
write.csv(category_summary, "output/hedge_category_summary.csv", row.names = FALSE)




# ======================================================================
# 03 | Classify radiographs by specialty and anatomical site
# ======================================================================
# Read the previously screened CSV file
data <- read.csv("output/screened_data.csv")

# Convert CPT codes to character format
data$code <- trimws(as.character(data$code))

# Define CPT codes for the ENT subgroup
ent_codes <- c(
  "70160", "70100", "70110", "70140", "70150",
  "70210", "70360", "70220", "70250", "70260"
)

# Define CPT codes for the orthopedic subgroup
ortho_codes <- c(
  "71100", "71101", "71110", "71111", "73010",
  "72220", "71120", "71130", "72200", "72202"
)

# Assign each radiograph to ENT, Orthopedic, or Other
data$subgroup <- ifelse(
  data$code %in% ent_codes, "ENT",
  ifelse(data$code %in% ortho_codes, "Orthopedic", "Other")
)

# Create an empty anatomical site column
data$anatomical_site <- NA_character_

# Assign nasal bone radiographs
data$anatomical_site[data$code %in% c("70160")] <- "Nasal bone"

# Assign mandible radiographs
data$anatomical_site[data$code %in% c("70100", "70110")] <- "Mandible"

# Assign facial bone radiographs
data$anatomical_site[data$code %in% c("70140", "70150")] <- "Facial bones"

# Assign sinus radiographs
data$anatomical_site[data$code %in% c("70210", "70220")] <- "Sinus"

# Assign neck soft tissue radiographs
data$anatomical_site[data$code %in% c("70360")] <- "Neck soft tissue"

# Assign skull radiographs
data$anatomical_site[data$code %in% c("70250", "70260")] <- "Skull"

# Assign rib radiographs
data$anatomical_site[data$code %in% c(
  "71100", "71101", "71110", "71111"
)] <- "Ribs"

# Assign scapula radiographs
data$anatomical_site[data$code %in% c("73010")] <- "Scapula"

# Assign sacrum and coccyx radiographs into one group
data$anatomical_site[data$code %in% c(
  "72220", "72200", "72202"
)] <- "Sacrum/Coccyx"

# Assign sternum radiographs
data$anatomical_site[data$code %in% c("71120")] <- "Sternum"

# Assign sternoclavicular joint radiographs
data$anatomical_site[data$code %in% c("71130")] <- "SC joint"

# Display the number of radiographs in each subgroup
table(data$subgroup)

# Display the number of radiographs in each anatomical site
table(data$anatomical_site, useNA = "always")

# Save the classified dataset as a new CSV file
write.csv(
  data,
  "output/classified_data.csv",
  row.names = FALSE,
  na = ""
)



# ======================================================================
# 04 | Calculate hedge-term prevalence by specialty
# ======================================================================
# Define a function to calculate hedge-term statistics
calculate_stats <- function(df) {
  
  # Count all radiographs in the group
  total <- nrow(df)
  
  # Count reports with available screening results
  available <- sum(!is.na(df$term_flag))
  
  # Count reports without available screening results
  missing <- sum(is.na(df$term_flag))
  
  # Count reports positive for hedge terms
  positive <- sum(df$term_flag %in% TRUE)
  
  # Count reports negative for hedge terms
  negative <- sum(df$term_flag %in% FALSE)
  
  # Calculate hedge-term prevalence among available reports
  prevalence <- if (available > 0) {
    round(positive / available * 100, 2)
  } else {
    NA_real_
  }
  
  # Return the calculated statistics as one row
  data.frame(
    total_radiographs = total,
    reports_available = available,
    reports_missing = missing,
    hedge_positive = positive,
    hedge_negative = negative,
    hedge_prevalence_percent = prevalence
  )
}

# Keep only ENT and orthopedic radiographs for analysis
analysis_data <- subset(data, subgroup %in% c("ENT", "Orthopedic"))

# Calculate statistics separately for ENT and Orthopedic
subgroup_stats <- do.call(
  rbind,
  lapply(split(analysis_data, analysis_data$subgroup), calculate_stats)
)

# Convert subgroup names into a regular column
subgroup_stats$subgroup <- rownames(subgroup_stats)

# Remove row names from the summary table
rownames(subgroup_stats) <- NULL

# Display the subgroup statistics
print(subgroup_stats)

# Save subgroup statistics to a CSV file
write.csv(
  subgroup_stats,
  "output/hedge_stats_by_subgroup.csv",
  row.names = FALSE
)



# ======================================================================
# 05 | Chi-square test: ENT versus Orthopedic hedge-term prevalence
# ======================================================================


# Create a 2-by-2 contingency table using the existing subgroup counts
chi_table <- rbind(
  ENT = c(
    Positive = subgroup_stats$hedge_positive[subgroup_stats$subgroup == "ENT"],
    Negative = subgroup_stats$hedge_negative[subgroup_stats$subgroup == "ENT"]
  ),
  Orthopedic = c(
    Positive = subgroup_stats$hedge_positive[subgroup_stats$subgroup == "Orthopedic"],
    Negative = subgroup_stats$hedge_negative[subgroup_stats$subgroup == "Orthopedic"]
  )
)

# Display the contingency table
print(chi_table)

# Perform Pearson chi-square test without Yates correction
chi_result <- chisq.test(chi_table, correct = FALSE)

# Display the full chi-square test results
print(chi_result)

# Display the chi-square statistic
cat("Chi-square:", as.numeric(chi_result$statistic), "\n")

# Display the degrees of freedom
cat("Degrees of freedom:", as.numeric(chi_result$parameter), "\n")

# Display the p-value
cat("P-value:", chi_result$p.value, "\n")

# Check whether expected cell counts are sufficiently large
print(chi_result$expected)

# Warn if any expected cell count is below 5
if (any(chi_result$expected < 5)) {
  cat("Warning: Small expected cell count; consider Fisher's exact test.\n")
  cat("Fisher exact p-value:", fisher.test(chi_table)$p.value, "\n")
}

# Create a results table for export
chi_summary <- data.frame(
  comparison = "ENT vs Orthopedic",
  chi_square = round(as.numeric(chi_result$statistic), 3),
  df = as.numeric(chi_result$parameter),
  p_value = chi_result$p.value
)

# Display the results table
print(chi_summary)

# Save the chi-square test results
write.csv(
  chi_summary,
  "output/hedge_chisquare_ENT_vs_Orthopedic.csv",
  row.names = FALSE
)




# ======================================================================
# 06 | Calculate hedge-term prevalence by anatomical site
# ======================================================================
# Calculate statistics for each anatomical site
site_stats <- do.call(
  rbind,
  lapply(split(analysis_data, analysis_data$anatomical_site), calculate_stats)
)

# Convert anatomical site names into a regular column
site_stats$anatomical_site <- rownames(site_stats)

# Remove row names from the summary table
rownames(site_stats) <- NULL

# Display the anatomical site statistics
print(site_stats)

# Save anatomical site statistics to a CSV file
write.csv(
  site_stats,
  "output/hedge_stats_by_anatomical_site.csv",
  row.names = FALSE
)




# ======================================================================
# 07 | Summarize hedge-term categories by subgroup and site
# ======================================================================
# Define the three hedge-term categories
categories <- c(
  "uncertainty",
  "limitation",
  "follow-up/ recommendation"
)

# Create a function to count each hedge-term category
count_categories <- function(df) {
  
  # Count reports with available screening results
  available <- sum(!is.na(df$term_flag))
  
  # Count reports containing uncertainty terms
  uncertainty_n <- sum(
    grepl("uncertainty", df$hedge_term_category),
    na.rm = TRUE
  )
  
  # Count reports containing limitation terms
  limitation_n <- sum(
    grepl("limitation", df$hedge_term_category),
    na.rm = TRUE
  )
  
  # Count reports containing follow-up or recommendation terms
  follow_up_n <- sum(
    grepl("follow-up/ recommendation", df$hedge_term_category,
          fixed = TRUE),
    na.rm = TRUE
  )
  
  # Return category counts and percentages
  data.frame(
    reports_available = available,
    uncertainty_n = uncertainty_n,
    limitation_n = limitation_n,
    follow_up_n = follow_up_n,
    uncertainty_percent = if (available > 0) round(100 * uncertainty_n / available, 2) else NA_real_,
    limitation_percent = if (available > 0) round(100 * limitation_n / available, 2) else NA_real_,
    follow_up_percent = if (available > 0) round(100 * follow_up_n / available, 2) else NA_real_
  )
}

# Calculate category statistics by main subgroup
subgroup_categories <- do.call(
  rbind,
  lapply(split(analysis_data, analysis_data$subgroup), count_categories)
)

# Add subgroup names as a column
subgroup_categories$subgroup <- rownames(subgroup_categories)

# Remove row names
rownames(subgroup_categories) <- NULL

# Save the subgroup category statistics
write.csv(
  subgroup_categories,
  "output/category_prevalence_by_subgroup.csv",
  row.names = FALSE
)

# Calculate category statistics by anatomical site
site_categories <- do.call(
  rbind,
  lapply(split(analysis_data, analysis_data$anatomical_site), count_categories)
)

# Add anatomical site names as a column
site_categories$anatomical_site <- rownames(site_categories)

# Remove row names
rownames(site_categories) <- NULL

# Save the anatomical site category statistics
write.csv(
  site_categories,
  "output/hedge_categories_by_anatomical_site.csv",
  row.names = FALSE
)


# ======================================================================
# 08 | Count reports containing one, two, or three hedge-term categories
# ======================================================================
#Verifying exactly 1, exactly 2 or more hedge term categories

# Select only reports positive for hedge terms
positive_data <- subset(data, term_flag == TRUE & !is.na(hedge_term_category))

# Count the number of categories in each positive report
positive_data$n_categories <- lengths(strsplit(positive_data$hedge_term_category, ";", fixed = TRUE))

# Count reports containing exactly 1, 2, or 3 categories
category_counts <- table(factor(positive_data$n_categories, levels = 1:3))

# Calculate percentages among all hedge-positive reports
category_percent <- round(100 * category_counts / nrow(positive_data), 1)

# Create a summary table with counts and percentages
category_summary <- data.frame(
  number_of_categories = c("Exactly 1", "Exactly 2", "All 3"),
  count = as.integer(category_counts),
  percentage = as.numeric(category_percent)
)

# Display the summary table
print(category_summary)

# Save the summary table to the working directory
write.csv(category_summary, "output/number_of_hedge_categories.csv", row.names = FALSE)



# ======================================================================
# 09 | Correct study years for temporal analyses
# ======================================================================
# Temporal trend 
# Read the screened and CPT-classified dataset
data <- read.csv("output/classified_data.csv")

# Display the first six rows
head(data)

# Display all column names
names(data)

class(data$date)



# Extract the first four digits from date and overwrite the year column
data$year <- as.integer(substr(data$date, 1, 4))

# Reclassify records from 2009 as 2010
data$year[data$year == 2009] <- 2010

# Reclassify records from 2026 as 2025
data$year[data$year == 2026] <- 2025

# Display the number of radiographs in each corrected year
table(data$year, useNA = "always")

# Verify that no years are missing
sum(is.na(data$year))

# Display the first six dates and corrected years
head(data[, c("date", "year")])

# Save the corrected dataset as a new CSV file
write.csv(
  data,
  "output/year_extracted_data.csv",
  row.names = FALSE
)



# ======================================================================
# 10 | Temporal trends in radiograph volume
# ======================================================================
# Seeking temporal trends for imaging volume 



# Load the dataset with corrected years and anatomical classifications
data <- read.csv("output/year_extracted_data.csv")

# Check the subgroup names
table(data$subgroup, useNA = "always")

# Check the anatomical site names
table(data$anatomical_site, useNA = "always")



# Define a function to calculate annual volume and year-over-year changes
annual_change <- function(years) {
  
  # Count radiographs for every year from 2010 through 2025
  counts <- table(factor(years, levels = 2010:2025))
  
  # Create a results table with year and radiograph count
  result <- data.frame(year = 2010:2025, count = as.integer(counts))
  
  # Get the previous year's count (2010 has no previous year)
  result$previous_year_count <- c(NA, head(result$count, -1))
  
  # Calculate the change in number of radiographs
  result$count_change <- result$count - result$previous_year_count
  
  # Calculate percentage change using the previous year as denominator
  result$percent_change <- round(100 * result$count_change / result$previous_year_count, 2)
  
  # Replace undefined percentage changes with NA
  result$percent_change[!is.finite(result$percent_change)] <- NA
  
  # Return the completed results table
  return(result)
}


# Calculate annual counts and percentage changes for all radiographs
overall_annual <- annual_change(data$year)

# Display the results
print(overall_annual)

# Save the overall annual results
write.csv(overall_annual, "output/annual_volume_overall.csv", row.names = FALSE)





# ======================================================================
# 11 | Annual radiograph volume by subgroup
# ======================================================================
# Keep only radiographs classified as ENT or Orthopedic
subgroup_data <- data[data$subgroup %in% c("ENT", "Orthopedic"), ]

# Calculate annual changes separately for each subgroup
subgroup_results <- lapply(split(subgroup_data$year, subgroup_data$subgroup), annual_change)

# Add subgroup names to each results table
subgroup_results <- Map(function(x, name) transform(x, subgroup = name), subgroup_results, names(subgroup_results))

# Combine both subgroups into one table
subgroup_annual <- do.call(rbind, subgroup_results)

# Put the subgroup column first
subgroup_annual <- subgroup_annual[, c("subgroup", "year", "count", "previous_year_count", "count_change", "percent_change")]

# Display the results
print(subgroup_annual)

# Save the subgroup results
write.csv(subgroup_annual, "output/annual_volume_by_subgroup.csv", row.names = FALSE)





# ======================================================================
# 12 | Annual radiograph volume by anatomical site
# ======================================================================
# Keep radiographs with an assigned anatomical site
site_data <- data[!is.na(data$anatomical_site) & data$anatomical_site != "", ]

# Calculate annual changes separately for each anatomical site
site_results <- lapply(split(site_data$year, site_data$anatomical_site), annual_change)

# Add the anatomical site name to each results table
site_results <- Map(function(x, name) transform(x, anatomical_site = name), site_results, names(site_results))

# Combine all anatomical sites into one table
site_annual <- do.call(rbind, site_results)

# Put the anatomical site column first
site_annual <- site_annual[, c("anatomical_site", "year", "count", "previous_year_count", "count_change", "percent_change")]

# Display the results
print(site_annual)

# Save the anatomical site results
write.csv(site_annual, "output/annual_volume_by_anatomical_site.csv", row.names = FALSE)




# ======================================================================
# 13 | Temporal trends in hedge-term frequency
# ======================================================================
#Seeking temporal trends for Hedge terms 


# Load the dataset with corrected years and hedge-term classifications
data <- read.csv("output/year_extracted_data.csv")

# Create a function to calculate annual counts and year-over-year changes
annual_hedge_trend <- function(years) {
  
  # Count reports in each year from 2010 through 2025
  counts <- table(factor(years, levels = 2010:2025))
  
  # Create a table of years and annual counts
  result <- data.frame(year = 2010:2025, count = as.integer(counts))
  
  # Add the previous year's count
  result$previous_year_count <- c(NA, head(result$count, -1))
  
  # Calculate the difference in counts from the previous year
  result$count_change <- result$count - result$previous_year_count
  
  # Calculate the percentage change using the previous year as denominator
  result$percent_change <- round(100 * result$count_change / result$previous_year_count, 2)
  
  # Replace undefined percentage changes with NA
  result$percent_change[!is.finite(result$percent_change)] <- NA
  
  # Return the results
  return(result)
}


# Select reports containing at least one hedge term
any_hedge <- data[data$term_flag %in% TRUE, ]

# Calculate annual counts and year-over-year changes
any_hedge_annual <- annual_hedge_trend(any_hedge$year)

# Display the results
print(any_hedge_annual)

# Save the results
write.csv(any_hedge_annual, "output/annual_any_hedge_trend.csv", row.names = FALSE)



# ======================================================================
# 14 | Temporal trends by hedge-term category
# ======================================================================
# Select reports containing uncertainty terms
uncertainty <- data[grepl("uncertainty", data$hedge_term_category, ignore.case = TRUE), ]

# Select reports containing limitation terms
limitation <- data[grepl("limitation", data$hedge_term_category, ignore.case = TRUE), ]

# Select reports containing follow-up or recommendation terms
follow_up <- data[grepl("follow", data$hedge_term_category, ignore.case = TRUE), ]

# Calculate annual trends for uncertainty terms
uncertainty_annual <- annual_hedge_trend(uncertainty$year)

# Calculate annual trends for limitation terms
limitation_annual <- annual_hedge_trend(limitation$year)

# Calculate annual trends for follow-up/recommendation terms
follow_up_annual <- annual_hedge_trend(follow_up$year)

# Add the category names
uncertainty_annual$category <- "Uncertainty"
limitation_annual$category <- "Limitation"
follow_up_annual$category <- "Follow-up/Recommendation"

# Combine the three categories into one table
category_annual <- rbind(uncertainty_annual, limitation_annual, follow_up_annual)

# Move the category column to the beginning
category_annual <- category_annual[, c("category", "year", "count", "previous_year_count", "count_change", "percent_change")]

# Display the results
print(category_annual)

# Save the combined category results
write.csv(category_annual, "output/annual_hedge_category_trends.csv", row.names = FALSE)
