# =============================================================================
# 04_pca.R
# Principal Component Analysis on developer behavioral features
# =============================================================================

cat("========================================\n")
cat("  PRINCIPAL COMPONENT ANALYSIS (PCA)\n")
cat("========================================\n\n")

library(ggplot2)
library(dplyr)
library(factoextra)   # fviz_* helpers (install if missing)

# auto-install if needed
for (pkg in c("factoextra", "ggrepel")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org",
                     quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}

df <- read.csv("data/cleaned_features.csv",
               stringsAsFactors = FALSE)

# ------------------------------------------------------------------
# Select features for PCA (remove ID/label columns)
# ------------------------------------------------------------------
pca_features <- c(
  "commits", "lines_added", "lines_deleted", "files_changed",
  "total_churn", "churn_ratio", "avg_lines_per_commit",
  "avg_files_per_commit", "risk_intensity", "workload_intensity",
  "refactor_signal", "hour_std", "is_after_hours", "file_spread",
  "commit_density"
)

X <- df[pca_features]
X <- data.frame(lapply(X, function(x) as.numeric(as.character(x))))
X <- X[complete.cases(X), ]

cat(sprintf("PCA input matrix: %d rows × %d features\n\n",
            nrow(X), ncol(X)))

# ------------------------------------------------------------------
# Standardise and run PCA
# ------------------------------------------------------------------
pca_result <- prcomp(X, center = TRUE, scale. = TRUE)

# Variance explained
var_exp   <- pca_result$sdev^2
prop_var  <- var_exp / sum(var_exp)
cum_var   <- cumsum(prop_var)

cat("Variance explained per component:\n")
vdf <- data.frame(
  PC            = paste0("PC", seq_along(prop_var)),
  Eigenvalue    = round(var_exp, 4),
  Prop_Variance = round(prop_var * 100, 2),
  Cum_Variance  = round(cum_var * 100, 2)
)
print(head(vdf, 10), row.names = FALSE)

n_pcs_90 <- which(cum_var >= 0.90)[1]
cat(sprintf("\n→ %d PCs needed to explain ≥ 90%% of variance\n\n", n_pcs_90))

# ------------------------------------------------------------------
# Loadings for PC1–PC3
# ------------------------------------------------------------------
cat("Feature loadings (PC1 – PC3):\n")
loadings <- as.data.frame(pca_result$rotation[, 1:3])
loadings <- loadings[order(abs(loadings$PC1), decreasing = TRUE), ]
print(round(loadings, 4))

# ------------------------------------------------------------------
# Plot 1: Scree plot
# ------------------------------------------------------------------
theme_mining <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

p_scree <- ggplot(vdf[1:min(12, nrow(vdf)), ],
                  aes(x = PC, y = Prop_Variance, group = 1)) +
  geom_col(fill = "#4472C4", alpha = 0.8) +
  geom_line(aes(y = Cum_Variance / 5), colour = "#ED7D31",
            linewidth = 1) +
  geom_point(aes(y = Cum_Variance / 5), colour = "#ED7D31", size = 2.5) +
  geom_hline(yintercept = 90 / 5, linetype = "dashed",
             colour = "grey40") +
  scale_y_continuous(
    name = "Proportion of Variance (%)",
    sec.axis = sec_axis(~. * 5, name = "Cumulative Variance (%)")) +
  labs(title    = "PCA Scree Plot",
       subtitle = "Bars = individual variance; orange line = cumulative",
       x = "Principal Component") +
  theme_mining
ggsave("plots/07_pca_scree.png",
       p_scree, width = 9, height = 5, dpi = 150)

# ------------------------------------------------------------------
# Plot 2: Biplot (PC1 vs PC2)
# ------------------------------------------------------------------
scores <- as.data.frame(pca_result$x[, 1:2])
scores$repo <- as.factor(df$repo[complete.cases(df[pca_features])])
scores$repo <- df$repo[complete.cases(df[pca_features])]

p_biplot <- ggplot(scores, aes(x = PC1, y = PC2, colour = repo)) +
  geom_point(alpha = 0.45, size = 1.8) +
  stat_ellipse(aes(group = repo), level = 0.68,
               linewidth = 0.8, linetype = "dashed") +
  labs(title    = "PCA Biplot – PC1 vs PC2",
       subtitle = "Coloured by repository; ellipses = 68% CI",
       x = sprintf("PC1 (%.1f%% variance)", prop_var[1] * 100),
       y = sprintf("PC2 (%.1f%% variance)", prop_var[2] * 100),
       colour = "Repository") +
  theme_mining
ggsave("plots/08_pca_biplot.png",
       p_biplot, width = 9, height = 6, dpi = 150)

# ------------------------------------------------------------------
# Plot 3: Loading vectors (custom biplot arrows)
# ------------------------------------------------------------------
rot_df <- as.data.frame(pca_result$rotation[, 1:2])
rot_df$feature <- rownames(rot_df)
scale_factor <- max(abs(scores[, c("PC1", "PC2")])) * 0.55

p_loading <- ggplot() +
  geom_point(data = scores, aes(PC1, PC2, colour = repo),
             alpha = 0.3, size = 1.2) +
  geom_segment(data = rot_df,
               aes(x = 0, y = 0,
                   xend = PC1 * scale_factor,
                   yend = PC2 * scale_factor),
               arrow = arrow(length = unit(0.18, "cm")),
               colour = "#C00000", linewidth = 0.7) +
  geom_text(data = rot_df,
            aes(x = PC1 * scale_factor * 1.12,
                y = PC2 * scale_factor * 1.12,
                label = feature),
            size = 2.8, colour = "#C00000") +
  labs(title    = "PCA Loading Plot",
       subtitle = "Arrows show feature direction in PC space",
       x = sprintf("PC1 (%.1f%%)", prop_var[1] * 100),
       y = sprintf("PC2 (%.1f%%)", prop_var[2] * 100),
       colour = "Repository") +
  theme_mining
ggsave("plots/09_pca_loadings.png",
       p_loading, width = 9, height = 7, dpi = 150)

# ------------------------------------------------------------------
# Save PCA scores for clustering
# ------------------------------------------------------------------
pca_scores <- as.data.frame(pca_result$x)
pca_scores$repo      <- df$repo[complete.cases(df[pca_features])]
pca_scores$developer <- df$developer[complete.cases(df[pca_features])]
pca_scores$week      <- df$week[complete.cases(df[pca_features])]
# attach original features for later interpretation
for (f in pca_features) pca_scores[[f]] <- X[[f]]

write.csv(pca_scores,
          "data/pca_scores.csv",
          row.names = FALSE)

cat("\n✓ PCA complete. Scores saved to data/pca_scores.csv\n")
cat("  Plots: 07_pca_scree.png, 08_pca_biplot.png, 09_pca_loadings.png\n\n")
