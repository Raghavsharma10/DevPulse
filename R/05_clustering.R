# =============================================================================
# 05_clustering.R
# K-Means + Hierarchical Clustering in PCA latent space
# =============================================================================

cat("========================================\n")
cat("  CLUSTERING ANALYSIS\n")
cat("========================================\n\n")

library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

for (pkg in c("factoextra", "dendextend", "ggrepel")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  library(pkg, character.only = TRUE)
}

set.seed(42)

pca_df <- read.csv("data/pca_scores.csv",
                   stringsAsFactors = FALSE)
pca_df$week <- as.Date(pca_df$week)

# Use first 5 PCs (explains bulk of variance)
pc_cols <- paste0("PC", 1:5)
X_clust <- as.matrix(pca_df[, pc_cols])

theme_mining <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

# ==================================================================
# A.  Determine optimal K
# ==================================================================

# Elbow (WSS)
wss <- sapply(1:10, function(k) {
  kmeans(X_clust, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

# Silhouette
sil <- sapply(2:10, function(k) {
  km <- kmeans(X_clust, centers = k, nstart = 25, iter.max = 100)
  mean(silhouette(km$cluster, dist(X_clust))[, 3])
})

cat("Silhouette scores (k = 2..10):\n")
print(data.frame(k = 2:10, silhouette = round(sil, 4)))
best_k <- which.max(sil) + 1
cat(sprintf("\n→ Best k by silhouette: %d\n\n", best_k))
# Clamp to sensible range for interpretability
best_k <- max(3, min(best_k, 6))
cat(sprintf("  Using k = %d (interpretability clamped)\n\n", best_k))

# Plot: elbow
p_elbow <- ggplot(data.frame(k = 1:10, wss), aes(k, wss)) +
  geom_line(colour = "#4472C4", linewidth = 1) +
  geom_point(colour = "#4472C4", size = 3) +
  geom_vline(xintercept = best_k, linetype = "dashed", colour = "#ED7D31") +
  labs(title = "Elbow Plot – Within-Cluster Sum of Squares",
       x = "Number of clusters (k)", y = "WSS") +
  theme_mining
ggsave("plots/10_elbow.png",
       p_elbow, width = 7, height = 4, dpi = 150)

# ==================================================================
# B.  K-Means clustering
# ==================================================================
km <- kmeans(X_clust, centers = best_k, nstart = 50, iter.max = 200)
pca_df$kmeans_cluster <- km$cluster

cat(sprintf("K-Means cluster sizes (k=%d):\n", best_k))
print(table(pca_df$kmeans_cluster))

# ------------------------------------------------------------------
# Interpret clusters from original feature means
# ------------------------------------------------------------------
feat_cols <- c("commits", "lines_added", "lines_deleted", "total_churn",
               "churn_ratio", "avg_files_per_commit", "risk_intensity",
               "workload_intensity", "refactor_signal")

cluster_summary <- pca_df %>%
  group_by(kmeans_cluster) %>%
  summarise(across(all_of(feat_cols), mean, .names = "mean_{.col}"),
            n = n(), .groups = "drop")

cat("\nCluster feature means:\n")
print(as.data.frame(cluster_summary), digits = 2)

# Assign behavior labels heuristically
label_cluster <- function(row) {
  if (row["mean_refactor_signal"] > 0.45)
    return("Refactoring activity")
  if (row["mean_workload_intensity"] > quantile(cluster_summary$mean_workload_intensity, 0.75))
    return("High-intensity burst")
  if (row["mean_commits"] < quantile(cluster_summary$mean_commits, 0.33) &&
      row["mean_total_churn"] < quantile(cluster_summary$mean_total_churn, 0.33))
    return("Low-activity maintenance")
  return("Normal feature development")
}

cluster_summary$behavior_label <- apply(cluster_summary, 1, label_cluster)
cat("\nBehavior labels:\n")
print(cluster_summary[, c("kmeans_cluster", "n", "behavior_label")],
      row.names = FALSE)

label_map <- setNames(cluster_summary$behavior_label,
                      cluster_summary$kmeans_cluster)
pca_df$behavior_label <- label_map[as.character(pca_df$kmeans_cluster)]

# ------------------------------------------------------------------
# Plot: K-Means in PC1-PC2 space
# ------------------------------------------------------------------
cluster_colors <- c("#4472C4", "#ED7D31", "#A9D18E", "#FF0000",
                    "#7030A0", "#00B0F0")[seq_len(best_k)]

p_km <- ggplot(pca_df, aes(PC1, PC2, colour = behavior_label)) +
  geom_point(alpha = 0.5, size = 1.8) +
  stat_ellipse(aes(group = behavior_label),
               level = 0.68, linewidth = 0.9) +
  labs(title    = sprintf("K-Means Clusters (k=%d) in PCA Space", best_k),
       subtitle = "Ellipses = 68% CI per cluster",
       colour   = "Behavior State",
       x = "PC1", y = "PC2") +
  scale_colour_brewer(palette = "Set1") +
  theme_mining
ggsave("plots/11_kmeans_clusters.png",
       p_km, width = 10, height = 6, dpi = 150)

# ==================================================================
# C.  Hierarchical clustering (on a sample for speed)
# ==================================================================
n_sample  <- min(400, nrow(X_clust))
idx       <- sample(nrow(X_clust), n_sample)
d_mat     <- dist(X_clust[idx, ], method = "euclidean")
hc        <- hclust(d_mat, method = "ward.D2")
pca_df$hclust_cluster <- NA
pca_df$hclust_cluster[idx] <- cutree(hc, k = best_k)

p_dend <- fviz_dend(hc, k = best_k,
                    palette = "jco",
                    show_labels = FALSE,
                    main = sprintf("Hierarchical Dendrogram (Ward, k=%d)", best_k),
                    ggtheme = theme_mining)
ggsave("plots/12_dendrogram.png",
       p_dend, width = 10, height = 5, dpi = 150)

# ==================================================================
# D.  Cluster profile bar chart
# ==================================================================
profile_long <- cluster_summary %>%
  select(behavior_label, mean_commits, mean_total_churn,
         mean_risk_intensity, mean_workload_intensity,
         mean_refactor_signal) %>%
  tidyr::pivot_longer(-behavior_label,
                      names_to = "metric", values_to = "value") %>%
  mutate(metric = gsub("mean_", "", metric))

# Normalise within each metric to 0–1 for comparability
profile_long <- profile_long %>%
  group_by(metric) %>%
  mutate(value_norm = (value - min(value)) /
           (max(value) - min(value) + 1e-9)) %>%
  ungroup()

p_profile <- ggplot(profile_long,
                    aes(x = metric, y = value_norm, fill = behavior_label)) +
  geom_col(position = "dodge", width = 0.65) +
  labs(title    = "Normalised Cluster Profiles",
       subtitle = "Each metric scaled 0–1 for cross-metric comparison",
       x = NULL, y = "Normalised value",
       fill = "Behavior State") +
  scale_fill_brewer(palette = "Set1") +
  theme_mining +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("plots/13_cluster_profiles.png",
       p_profile, width = 10, height = 5, dpi = 150)

# ==================================================================
# Save clustered data
# ==================================================================
write.csv(pca_df,
          "data/clustered_data.csv",
          row.names = FALSE)

write.csv(cluster_summary,
          "output/cluster_summary.csv",
          row.names = FALSE)

cat("\n✓ Clustering complete.\n")
cat("  Plots: 10_elbow, 11_kmeans_clusters, 12_dendrogram, 13_cluster_profiles\n\n")
