# =============================================================================
# 03_eda.R
# Exploratory Data Analysis with saved plots
# =============================================================================

cat("========================================\n")
cat("  EXPLORATORY DATA ANALYSIS\n")
cat("========================================\n\n")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

df <- read.csv("data/cleaned_features.csv",
               stringsAsFactors = FALSE)
df$week <- as.Date(df$week)

theme_mining <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey40", size = 10),
        panel.grid.minor = element_blank(),
        strip.text    = element_text(face = "bold"),
        legend.position = "bottom")

# ------------------------------------------------------------------
# Plot 1: Commit activity over time by repo
# ------------------------------------------------------------------
p1 <- df %>%
  group_by(repo, week) %>%
  summarise(total_commits = sum(commits), .groups = "drop") %>%
  ggplot(aes(x = week, y = total_commits, colour = repo, group = repo)) +
  geom_line(alpha = 0.8, linewidth = 0.9) +
  geom_smooth(se = FALSE, method = "loess", span = 0.4,
              linewidth = 0.5, linetype = "dashed", alpha = 0.5) +
  labs(title    = "Weekly Commit Activity by Repository",
       subtitle = "Solid = actual, dashed = LOESS trend",
       x = "Week", y = "Total Commits", colour = "Repository") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme_mining
ggsave("plots/01_commit_activity.png",
       p1, width = 10, height = 5, dpi = 150)

# ------------------------------------------------------------------
# Plot 2: Distribution of key features
# ------------------------------------------------------------------
feat_long <- df %>%
  select(repo, commits, total_churn, churn_ratio,
         avg_files_per_commit, workload_intensity) %>%
  pivot_longer(-repo, names_to = "feature", values_to = "value")

p2 <- feat_long %>%
  ggplot(aes(x = value, fill = repo)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~feature, scales = "free", ncol = 3) +
  labs(title    = "Distribution of Key Behavioral Features",
       subtitle = "Faceted by feature, coloured by repository",
       x = NULL, y = "Count", fill = "Repository") +
  theme_mining
ggsave("plots/02_feature_distributions.png",
       p2, width = 12, height = 7, dpi = 150)

# ------------------------------------------------------------------
# Plot 3: Correlation heatmap
# ------------------------------------------------------------------
num_cols <- c("commits", "lines_added", "lines_deleted", "files_changed",
              "total_churn", "churn_ratio", "avg_lines_per_commit",
              "avg_files_per_commit", "risk_intensity",
              "workload_intensity", "refactor_signal",
              "hour_std", "is_after_hours")
cor_mat <- cor(df[num_cols], use = "complete.obs")
cor_long <- as.data.frame(as.table(cor_mat))
names(cor_long) <- c("Var1", "Var2", "Corr")

p3 <- ggplot(cor_long, aes(Var1, Var2, fill = Corr)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(Corr, 2)), size = 2.5) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#D6604D",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Feature Correlation Matrix",
       x = NULL, y = NULL, fill = "r") +
  theme_mining +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
ggsave("plots/03_correlation_heatmap.png",
       p3, width = 10, height = 9, dpi = 150)

# ------------------------------------------------------------------
# Plot 4: Lines added vs deleted scatter
# ------------------------------------------------------------------
p4 <- df %>%
  ggplot(aes(x = lines_added, y = lines_deleted, colour = repo)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey30") +
  scale_x_continuous(trans = "log1p", labels = scales::comma) +
  scale_y_continuous(trans = "log1p", labels = scales::comma) +
  labs(title    = "Lines Added vs. Lines Deleted (log scale)",
       subtitle = "Dashed line = equal add/delete; points above = net deletion",
       x = "Lines Added", y = "Lines Deleted", colour = "Repository") +
  theme_mining
ggsave("plots/04_add_vs_delete.png",
       p4, width = 8, height = 6, dpi = 150)

# ------------------------------------------------------------------
# Plot 5: Boxplot – workload by repo
# ------------------------------------------------------------------
p5 <- df %>%
  ggplot(aes(x = reorder(repo, workload_intensity, median),
             y = workload_intensity, fill = repo)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.5) +
  coord_flip() +
  labs(title = "Workload Intensity Distribution by Repository",
       x = NULL, y = "Workload Intensity") +
  theme_mining + theme(legend.position = "none")
ggsave("plots/05_workload_boxplot.png",
       p5, width = 9, height = 5, dpi = 150)

# ------------------------------------------------------------------
# Plot 6: After-hours coding proportion per repo
# ------------------------------------------------------------------
p6 <- df %>%
  group_by(repo) %>%
  summarise(pct_after_hours = mean(is_after_hours) * 100) %>%
  ggplot(aes(x = reorder(repo, pct_after_hours), y = pct_after_hours,
             fill = repo)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = sprintf("%.1f%%", pct_after_hours)),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 55)) +
  labs(title = "Proportion of After-Hours Commits by Repository",
       x = NULL, y = "% developer-weeks with after-hours activity") +
  theme_mining + theme(legend.position = "none")
ggsave("plots/06_after_hours.png",
       p6, width = 9, height = 4, dpi = 150)

cat("✓ EDA plots saved to plots/\n")
cat("  01_commit_activity.png\n")
cat("  02_feature_distributions.png\n")
cat("  03_correlation_heatmap.png\n")
cat("  04_add_vs_delete.png\n")
cat("  05_workload_boxplot.png\n")
cat("  06_after_hours.png\n\n")
