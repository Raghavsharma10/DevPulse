# =============================================================================
# 02_data_cleaning_features.R
# Data cleaning + feature engineering for developer behavioral analysis
# =============================================================================

cat("========================================\n")
cat("  DATA CLEANING & FEATURE ENGINEERING\n")
cat("========================================\n\n")

library(dplyr)
library(tidyr)

# ------------------------------------------------------------------
# Load raw data
# ------------------------------------------------------------------
raw <- read.csv("data/raw_commits.csv",
                stringsAsFactors = FALSE)
raw$week <- as.Date(raw$week)

cat(sprintf("Loaded %d raw records\n", nrow(raw)))
cat(sprintf("Missing values per column:\n"))
print(colSums(is.na(raw)))

# ------------------------------------------------------------------
# Step 1: Remove duplicates
# ------------------------------------------------------------------
before <- nrow(raw)
raw <- raw[!duplicated(raw[c("repo", "developer", "week")]), ]
cat(sprintf("\nDuplicates removed: %d → %d rows\n", before, nrow(raw)))

# ------------------------------------------------------------------
# Step 2: Enforce non-negative counts
# ------------------------------------------------------------------
raw$commits       <- pmax(raw$commits, 0L)
raw$lines_added   <- pmax(raw$lines_added, 0L)
raw$lines_deleted <- pmax(raw$lines_deleted, 0L)
raw$files_changed <- pmax(raw$files_changed, 0L)

# ------------------------------------------------------------------
# Step 3: Outlier treatment (IQR-based capping per feature)
# ------------------------------------------------------------------
cap_iqr <- function(x, multiplier = 3) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  pmin(pmax(x, q[1] - multiplier * iqr), q[2] + multiplier * iqr)
}
numeric_cols <- c("commits", "lines_added", "lines_deleted", "files_changed")
for (col in numeric_cols) raw[[col]] <- cap_iqr(raw[[col]])

cat("Outlier capping (3×IQR) applied to:", paste(numeric_cols, collapse=", "), "\n")

# ------------------------------------------------------------------
# Step 4: Feature engineering
# ------------------------------------------------------------------
df <- raw %>%
  mutate(
    # Core churn metrics
    total_churn      = lines_added + lines_deleted,
    churn_ratio      = ifelse(lines_added > 0,
                              lines_deleted / lines_added, 0),
    net_lines        = lines_added - lines_deleted,

    # Productivity signals
    avg_lines_per_commit = ifelse(commits > 0,
                                   total_churn / commits, 0),
    avg_files_per_commit = ifelse(commits > 0,
                                   files_changed / commits, 0),

    # Temporal / circadian patterns
    is_after_hours   = as.integer(mean_hour < 9 | mean_hour > 18),
    hour_normalized  = (mean_hour - 9) / 9,   # 0 = 9am, 1 = 6pm

    # Change spread (structural impact)
    file_spread      = log1p(files_changed),
    commit_density   = log1p(commits),

    # Risk signals
    risk_intensity   = (total_churn / (files_changed + 1)) *
                        (hour_std / 5),
    workload_intensity = commits * log1p(total_churn),

    # Refactoring proxy: high deletion relative to addition
    refactor_signal  = ifelse(total_churn > 0,
                               lines_deleted / total_churn, 0)
  ) %>%
  # Remove any rows where core features are all zero
  filter(!(commits == 0 & total_churn == 0))

cat(sprintf("\nFeatures engineered: %d records, %d columns\n",
            nrow(df), ncol(df)))

# ------------------------------------------------------------------
# Step 5: Encode categorical variables
# ------------------------------------------------------------------
df$repo_id <- as.integer(factor(df$repo))
df$dev_id  <- as.integer(factor(paste(df$repo, df$developer, sep = "/")))

# ------------------------------------------------------------------
# Step 6: Summary statistics
# ------------------------------------------------------------------
feature_cols <- c("commits", "lines_added", "lines_deleted", "files_changed",
                  "total_churn", "churn_ratio", "avg_lines_per_commit",
                  "avg_files_per_commit", "risk_intensity",
                  "workload_intensity", "refactor_signal")

cat("\n--- Summary Statistics (key features) ---\n")
summary_df <- data.frame(
  feature = feature_cols,
  mean    = round(sapply(df[feature_cols], mean,   na.rm = TRUE), 2),
  sd      = round(sapply(df[feature_cols], sd,     na.rm = TRUE), 2),
  min     = round(sapply(df[feature_cols], min,    na.rm = TRUE), 2),
  p25     = round(sapply(df[feature_cols], quantile, 0.25, na.rm = TRUE), 2),
  median  = round(sapply(df[feature_cols], median, na.rm = TRUE), 2),
  p75     = round(sapply(df[feature_cols], quantile, 0.75, na.rm = TRUE), 2),
  max     = round(sapply(df[feature_cols], max,    na.rm = TRUE), 2)
)
print(summary_df, row.names = FALSE)

# ------------------------------------------------------------------
# Save
# ------------------------------------------------------------------
write.csv(df, "data/cleaned_features.csv",
          row.names = FALSE)
cat(sprintf("\n✓ Cleaned & engineered dataset saved: %d × %d\n",
            nrow(df), ncol(df)))
