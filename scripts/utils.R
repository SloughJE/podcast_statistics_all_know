library(dplyr)
library(ggplot2)
library(patchwork)



##########################
set.seed(123)

#########################

ns <- 10^(2:8)
bill <- 150e9
wealth_master <- rlnorm(max(ns), meanlog = 11, sdlog = 0.4)

df <- tibble(n = ns) %>%
  rowwise() %>%
  mutate(
    sample     = list(wealth_master[1:n]),
    all_vals   = list(c(sample, bill)),
    mean_val   = mean(all_vals),
    median_val = median(all_vals)
  ) %>%
  ungroup()

summary_df <- df %>%
  select(n, mean_val, median_val)
summary_df$n = summary_df$n+1
saveRDS(summary_df, "/Users/johnslough/Desktop/code/podcast_presentations/everyone_should_know_statistics/assets/wealth_with_bill.rds")


#################
# Total people
n <- 1000000

# % of ultra-rich
prop_rich <- 0.0001  

n_rich <- floor(n * prop_rich)
n_base <- n - n_rich

# Simulate "normal" wealth: log-normal around middle class
wealth_base <- rlnorm(n_base, meanlog = 11, sdlog = 0.3)

# Simulate "rich": much higher and more spread out
wealth_rich <- rlnorm(n_rich, meanlog = 20, sdlog = 1)

# Combine
wealth_all <- c(wealth_base, wealth_rich)

# Compute mean & median
base_mean = mean(wealth_base)
base_median = median(wealth_base)
rich_mean = mean(wealth_rich)
rich_median = median(wealth_rich)
combined_mean   <- mean(wealth_all)
combined_median <- median(wealth_all)

# Colors
mean_col   <- "#C04E3D"
median_col <- "#BFA06D"


# Add group labels and combine into one df
df_plot <- bind_rows(
  data.frame(wealth = wealth_base, group = "Base Population"),
  data.frame(wealth = wealth_rich, group = "Ultra-Rich (0.1%)"),
  data.frame(wealth = wealth_all,  group = "Combined")
)

# Count values
n_base <- length(wealth_base)
n_rich <- length(wealth_rich)
n_comb <- length(wealth_all)
base_pct <- round(n_base / (n_base + n_rich) * 100, 4)
rich_pct <- round(n_rich / (n_base + n_rich) * 100, 4)

# Text positions (adjust y later if needed)
y_pos <- 0.9

# Plot 1: Base
plot_base <- ggplot(data.frame(wealth = wealth_base), aes(x = wealth)) +
  geom_histogram(bins = 50, fill = "#5A6B80", color = "#3C4756", alpha = 0.8) +
  geom_vline(xintercept = base_mean, color = mean_col, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = base_median, color = median_col, linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Mean: $", format(round(base_mean), big.mark = ",")),
           hjust = 1.05, vjust = 4, size = 5, color = mean_col) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Median: $", format(round(base_median), big.mark = ",")),
           hjust = 1.05, vjust = 2, size = 5, color = median_col) +
  scale_x_log10(labels = scales::label_dollar()) +
  labs(title = paste0("Base Population: n = ", format(n_base, big.mark = ","),
                      " (", base_pct, "%)"
  ),
  x = NULL, y = "Count") +
  theme_minimal(base_size = 13) 


# Plot 2: Rich
plot_rich <- ggplot(data.frame(wealth = wealth_rich), aes(x = wealth)) +
  geom_histogram(bins = 20, fill = "#5A6B80", color = "#3C4756", alpha = 0.8) +
  geom_vline(xintercept = rich_mean, color = mean_col, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = rich_median, color = median_col, linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Mean: $", format(round(rich_mean), big.mark = ",")),
           hjust = 1.05, vjust = 4, size = 5, color = mean_col) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Median: $", format(round(rich_median), big.mark = ",")),
           hjust = 1.05, vjust = 2.0, size = 5, color = median_col) +
  scale_x_log10(labels = scales::label_dollar()) +
  labs(
    title = paste0(
      "Ultra Rich: n = ", format(n_rich, big.mark = ","),
      " (", rich_pct, "%)"
    ),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.title.y = element_blank()) 

# Plot 3: Combined
plot_combined <- ggplot(data.frame(wealth = wealth_all), aes(x = wealth)) +
  geom_histogram(bins = 100, fill = "#5A6B80", color = "#3C4756", alpha = 0.8) +
  geom_vline(xintercept = combined_mean, color = mean_col, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = combined_median, color = median_col, linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Mean: $", format(round(combined_mean), big.mark = ",")),
           hjust = 1.05, vjust = 4, size = 5, color = mean_col) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Median: $", format(round(combined_median), big.mark = ",")),
           hjust = 1.05, vjust = 2.0, size = 5, color = median_col) +
  scale_x_log10(labels = scales::label_dollar()) +
  labs(title = paste0("Combined: ","n = ", format(n_comb, big.mark = ",")), x = "Wealth (log scale)", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Arrange all plots``
(plot_base | plot_rich) / plot_combined +
  plot_layout(heights = c(1, 1)) 


ggsave(
  "/Users/johnslough/Desktop/code/podcast_presentations/everyone_should_know_statistics/assets/wealth_patchwork.png",(plot_base | plot_rich) / plot_combined +
    plot_layout(heights = c(1, 1)),
  width = 12, height = 8, dpi = 1200
) 
