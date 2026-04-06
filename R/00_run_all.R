# =============================================================================
# 00_run_all.R
# Master pipeline script — runs all stages in order
#
# Usage (from terminal):
#   Rscript R/00_run_all.R
#
# Or from RStudio:
#   source("R/00_run_all.R")
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════╗\n")
cat("║   DEVELOPER BEHAVIOR MINING PIPELINE             ║\n")
cat("║   Data Mining & Data Warehouse Project           ║\n")
cat("╚══════════════════════════════════════════════════╝\n\n")

# ------------------------------------------------------------------
# Auto-install required packages
# ------------------------------------------------------------------
required_pkgs <- c(
  "dplyr", "tidyr", "ggplot2", "scales", "lubridate",
  "cluster", "factoextra", "dendextend", "ggrepel",
  "httr", "jsonlite"
)

missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat(sprintf("Installing %d missing package(s): %s\n\n",
              length(missing), paste(missing, collapse = ", ")))
  install.packages(missing, repos = "https://cloud.r-project.org",
                   quiet = TRUE)
}
invisible(lapply(required_pkgs, library, character.only = TRUE,
                 warn.conflicts = FALSE, quietly = TRUE))

# ------------------------------------------------------------------
# Pipeline
# ------------------------------------------------------------------
pipeline <- list(
  list(script = "R/01_data_extraction.R",     label = "Stage 1: Data Extraction"),
  list(script = "R/02_data_cleaning_features.R",label = "Stage 2: Cleaning & Feature Engineering"),
  list(script = "R/03_eda.R",                 label = "Stage 3: Exploratory Data Analysis"),
  list(script = "R/04_pca.R",                 label = "Stage 4: Principal Component Analysis"),
  list(script = "R/05_clustering.R",           label = "Stage 5: Clustering"),
  list(script = "R/06_olap_operations.R",      label = "Stage 6: OLAP Operations")
)

timings <- list()
for (stage in pipeline) {
  cat(sprintf("\n▶ %s\n%s\n", stage$label, strrep("─", 50)))
  t0 <- proc.time()
  source(stage$script)
  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  timings[[stage$label]] <- elapsed
  cat(sprintf("  ✓ Completed in %.1fs\n", elapsed))
}

# ------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------
cat("\n╔══════════════════════════════════════════════════╗\n")
cat("║   PIPELINE COMPLETE                               ║\n")
cat("╚══════════════════════════════════════════════════╝\n\n")

cat("Stage timings:\n")
for (nm in names(timings))
  cat(sprintf("  %-45s %.1fs\n", nm, timings[[nm]]))

cat("\nOutput files:\n")
cat("  data/raw_commits.csv          — raw extraction\n")
cat("  data/cleaned_features.csv     — engineered features\n")
cat("  data/pca_scores.csv           — PCA component scores\n")
cat("  data/clustered_data.csv       — records with cluster labels\n")
cat("  output/cluster_summary.csv    — cluster profile table\n")
cat("  output/olap_rollup_monthly.csv\n")
cat("  output/olap_rollup_quarterly.csv\n")
cat("  output/olap_drilldown_weekly.csv\n")
cat("  output/olap_slice.csv\n")
cat("  output/olap_dice.csv\n")
cat("  output/olap_pivot.csv\n")

cat("\nPlots (plots/*.png):\n")
plots <- c(
  "01_commit_activity        — timeline by repo",
  "02_feature_distributions  — histograms of key features",
  "03_correlation_heatmap    — feature correlation matrix",
  "04_add_vs_delete          — lines added vs deleted",
  "05_workload_boxplot        — workload distribution",
  "06_after_hours            — after-hours proportion",
  "07_pca_scree              — variance explained",
  "08_pca_biplot             — PC1 vs PC2 by repo",
  "09_pca_loadings           — feature loading vectors",
  "10_elbow                  — optimal k selection",
  "11_kmeans_clusters        — cluster projections",
  "12_dendrogram             — hierarchical tree",
  "13_cluster_profiles       — normalised cluster radar",
  "14_olap_rollup            — monthly commit stack",
  "15_olap_drilldown         — weekly detail view",
  "16_olap_pivot_heatmap     — repo × behavior matrix"
)
for (p in plots) cat(sprintf("  %s\n", p))
cat("\n")

