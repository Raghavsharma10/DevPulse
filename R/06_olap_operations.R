# =============================================================================
# 06_olap_operations.R
# OLAP-style multi-dimensional analysis (Roll-Up, Drill-Down, Slice, Dice, Pivot)
# =============================================================================

cat("========================================\n")
cat("  OLAP OPERATIONS\n")
cat("========================================\n\n")

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

df <- read.csv("data/clustered_data.csv",
               stringsAsFactors = FALSE)
df$week  <- as.Date(df$week)
df$month <- floor_date(df$week, "month")
df$qtr   <- paste0(year(df$week), " Q", quarter(df$week))

theme_mining <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  1. ROLL-UP: Week → Month → Quarter\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

rollup_month <- df %>%
  group_by(repo, month) %>%
  summarise(
    total_commits    = sum(commits),
    total_churn      = sum(total_churn),
    active_devs      = n_distinct(developer),
    avg_workload     = mean(workload_intensity),
    avg_risk         = mean(risk_intensity),
    .groups = "drop"
  )

rollup_qtr <- df %>%
  group_by(repo, qtr) %>%
  summarise(
    total_commits    = sum(commits),
    total_churn      = sum(total_churn),
    active_devs      = n_distinct(developer),
    avg_workload     = mean(workload_intensity),
    .groups = "drop"
  )

cat("Monthly roll-up (first 12 rows):\n")
print(head(rollup_month, 12), row.names = FALSE)

cat("\nQuarterly roll-up:\n")
print(head(rollup_qtr, 12), row.names = FALSE)

write.csv(rollup_month, "output/olap_rollup_monthly.csv", row.names=FALSE)
write.csv(rollup_qtr,   "output/olap_rollup_quarterly.csv", row.names=FALSE)

# Plot roll-up
p_rollup <- rollup_month %>%
  ggplot(aes(x = month, y = total_commits, fill = repo)) +
  geom_col(position = "stack") +
  labs(title = "OLAP Roll-Up: Monthly Commit Volume by Repository",
       x = "Month", y = "Total Commits", fill = "Repository") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme_mining +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/14_olap_rollup.png",
       p_rollup, width = 11, height = 5, dpi = 150)

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  2. DRILL-DOWN: Quarter → Month → Week\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

# Pick the top repo by commits for drill-down demo
top_repo <- rollup_month %>%
  group_by(repo) %>%
  summarise(tot = sum(total_commits)) %>%
  slice_max(tot, n=1) %>%
  pull(repo)

cat(sprintf("Drill-down demo on: %s\n\n", top_repo))

drilldown_week <- df %>%
  filter(repo == top_repo) %>%
  group_by(week) %>%
  summarise(total_commits = sum(commits),
            active_devs   = n_distinct(developer),
            avg_churn     = mean(total_churn),
            .groups = "drop")

cat("Weekly drill-down (first 10 rows):\n")
print(head(drilldown_week, 10), row.names = FALSE)
write.csv(drilldown_week, "output/olap_drilldown_weekly.csv", row.names=FALSE)

p_drill <- drilldown_week %>%
  ggplot(aes(x = week, y = total_commits)) +
  geom_col(fill = "#4472C4", alpha = 0.8) +
  geom_line(aes(y = avg_churn / max(avg_churn, na.rm=TRUE) *
                    max(total_commits, na.rm=TRUE)),
            colour = "#ED7D31", linewidth = 1) +
  labs(title    = sprintf("OLAP Drill-Down: Weekly Activity — %s", top_repo),
       subtitle = "Bars = commits; orange line = normalised avg churn",
       x = "Week", y = "Commits") +
  theme_mining
ggsave("plots/15_olap_drilldown.png",
       p_drill, width = 10, height = 5, dpi = 150)

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  3. SLICE: Single repo, single quarter\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

slice_qtr  <- df$qtr[which.max(tabulate(match(df$qtr, unique(df$qtr))))]
slice_repo <- top_repo

sliced <- df %>%
  filter(repo == slice_repo, qtr == slice_qtr) %>%
  select(developer, week, commits, total_churn, workload_intensity,
         behavior_label)

cat(sprintf("Slice: repo=%s | quarter=%s | %d records\n\n",
            slice_repo, slice_qtr, nrow(sliced)))
print(head(sliced, 15), row.names = FALSE)
write.csv(sliced, "output/olap_slice.csv", row.names=FALSE)

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  4. DICE: Specific repos × behaviors × quarters\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

dice_repos  <- unique(df$repo)[1:2]
dice_labels <- c("High-intensity burst", "Refactoring activity",
                 "Normal feature development")
dice_qtrs   <- unique(df$qtr)[1:2]

diced <- df %>%
  filter(repo %in% dice_repos,
         behavior_label %in% dice_labels,
         qtr %in% dice_qtrs)

cat(sprintf("Dice result: %d records\n", nrow(diced)))
cat(sprintf("  repos: %s\n", paste(dice_repos, collapse=", ")))
cat(sprintf("  behaviors: %s\n", paste(dice_labels, collapse=", ")))
cat(sprintf("  quarters: %s\n\n", paste(dice_qtrs, collapse=", ")))
print(head(diced[, c("repo","developer","week","commits",
                      "total_churn","behavior_label","qtr")], 10),
      row.names = FALSE)
write.csv(diced, "output/olap_dice.csv", row.names=FALSE)

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  5. PIVOT: Repo × Behavior State matrix\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

pivot_tbl <- df %>%
  group_by(repo, behavior_label) %>%
  summarise(avg_commits = round(mean(commits), 2),
            avg_workload = round(mean(workload_intensity), 2),
            n = n(), .groups = "drop") %>%
  pivot_wider(names_from = behavior_label,
              values_from = c(avg_commits, n),
              values_fill = 0)

cat("Pivot table — avg commits per (repo × behavior):\n")
print(as.data.frame(pivot_tbl), row.names = FALSE)
write.csv(pivot_tbl, "output/olap_pivot.csv", row.names=FALSE)

# Heatmap pivot
pivot_heat <- df %>%
  group_by(repo, behavior_label) %>%
  summarise(avg_workload = mean(workload_intensity), .groups = "drop")

p_pivot <- ggplot(pivot_heat,
                  aes(x = behavior_label, y = repo, fill = avg_workload)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = round(avg_workload, 1)), size = 3.5) +
  scale_fill_gradient(low = "#DEEBF7", high = "#084594") +
  labs(title    = "OLAP Pivot – Avg Workload Intensity (Repo × Behavior)",
       x = "Behavior State", y = "Repository", fill = "Workload") +
  theme_mining +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave("plots/16_olap_pivot_heatmap.png",
       p_pivot, width = 10, height = 5, dpi = 150)

cat("\n✓ All OLAP operations complete.\n")
cat("  Outputs saved to output/\n")
cat("  Plots: 14_olap_rollup, 15_olap_drilldown, 16_olap_pivot_heatmap\n\n")
