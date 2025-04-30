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



make_dist_plot <- function(dist_fun, ...,
                           discrete = FALSE,
                           title    = "",
                           adjust   = 1.5,
                           n_sample = 1e5,
                           xlim     = NULL) {
  
  x <- dist_fun(n_sample, ...)
  if (!is.null(xlim)) x <- x[x >= xlim[1] & x <= xlim[2]]
  df <- data.frame(x)
  
  p <- ggplot(df, aes(x)) +
    {                       # <- wrap the if/else inside braces
      if (discrete) {
        geom_bar(
          aes(y = after_stat(count / max(count))),
          fill = "skyblue", colour = "black", width = 0.8
        )
      } else {
        geom_density(
          aes(y = after_stat(density / max(density))),
          fill = "skyblue", alpha = 0.6, adjust = adjust
        )
      }
    } +
    labs(title = title) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5, vjust = 0.3),
      panel.grid = element_blank()
    ) +
    ylim(0, 1.05)
  
  if (!is.null(xlim))
    p <- p + coord_cartesian(xlim = xlim, expand = FALSE)
  
  p
}



set.seed(1)
n_sample = 100000
# Continuous
p_normal     <- make_dist_plot(rnorm, mean = 0, sd = 1,               title = "Normal")
p_expon      <- make_dist_plot(rexp, rate = 1,                        title = "Exponential",     adjust = 2.0)
p_chisq      <- make_dist_plot(rchisq, df = 3,                        title = "Chi-Squared",     adjust = 2.0)
p_gamma      <- make_dist_plot(rgamma, shape = 2, scale = 2,          title = "Gamma",           adjust = 2.0)
p_beta       <- make_dist_plot(rbeta, 2, 5,                           title = "Beta",            adjust = 2.5)
p_t          <- make_dist_plot(rt, df = 5,                            title = "t",               adjust = 1.8)
p_log_normal <- make_dist_plot(rlnorm, meanlog = 0, sdlog = 0.5,      title = "Log-Normal",      adjust = 2.0)
p_weibull    <- make_dist_plot(rweibull, shape = 1.5, scale = 1,      title = "Weibull",         adjust = 2.0)
p_cont_uniform <- make_dist_plot(runif, min = 0, max = 1,             title = "Cont. Uniform",   adjust = 2.0)
p_laplace    <- make_dist_plot(function(n) rlaplace(n, 0, 1),         title = "Laplace",         adjust = 1.8)
p_logistic <- make_dist_plot(rlogis, location = 0, scale = 1, title = "Logistic", adjust = 1.8)
p_cauchy <- make_dist_plot(rcauchy, location = 0, scale = 1, title    = "Cauchy",adjust   = 0.9, xlim = c(-20, 20))
p_pareto <- make_dist_plot(rpareto, scale = 10000, shape    = 1, n_sample = n_sample, title    = "Pareto (log scale)",adjust   = 2)+  scale_x_log10(expand = c(0,0))


# Discrete
p_binomial   <- make_dist_plot(rbinom, size = 10, prob = 0.5,         title = "Binomial",       discrete = TRUE)
p_poisson    <- make_dist_plot(rpois, lambda = 3,                     title = "Poisson",        discrete = TRUE)
p_geometric  <- make_dist_plot(rgeom, prob = 0.3,                     title = "Geometric",      discrete = TRUE)
p_nbinom     <- make_dist_plot(rnbinom, size = 5, prob = 0.5,         title = "Neg. Binomial",  discrete = TRUE)
p_hypergeom  <- make_dist_plot(rhyper, m = 30, n = 70, k = 10,        title = "Hypergeometric", discrete = TRUE)
p_discrete_uniform <- make_dist_plot(function(n) sample(0:10, n, replace = TRUE), 
                                     title = "Discrete Uniform",      discrete = TRUE)
p_bernoulli <- make_dist_plot(rbinom, size = 1, prob = 0.3, title     = "Bernoulli", discrete  = TRUE)
zipf_sampler <- function(n, s = 1.1, N = 100) {
  ranks <- 1:N
  probs <- ranks^(-s)
  probs <- probs / sum(probs)
  sample(ranks, n, replace = TRUE, prob = probs)
}
p_zipf <- make_dist_plot(zipf_sampler,title = "Zipf–Mandelbrot", discrete = TRUE, xlim = c(0, 50))

library(patchwork)

#######################
# a tiny helper to zero out all margins on a plot
trim <- function(p) p + theme(plot.margin = margin(0,0,0,0,"pt"))

# add row label as y-axis title on the first plot of each row
p1 <- p_normal + 
  labs(y = "Symmetric") +
  theme(
    axis.title.y     = element_text(angle = 0, vjust = 0.5, hjust = 0.5,face   = "bold",
                                    margin = margin(r = 8)), 
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank()
  ) %>% trim()

p2 <- p_expon + 
  labs(y = "Skewed") +
  theme(
    axis.title.y     = element_text(angle = 0, vjust = 0.5, hjust = 0.5, face   = "bold",
                                    margin = margin(r = 8)),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank()
  ) %>% trim()



p1 <- p1 + theme(plot.margin = margin(0, 0, 15, 0))        # 15pt below row 1
p2 <- p2 + theme(plot.margin = margin(15, 0, 15, 0))       # 15pt above and below row 2

#####
p_t           <- trim(p_t)
p_laplace     <- trim(p_laplace)
p_cont_uniform<- trim(p_cont_uniform)
p_gamma       <- trim(p_gamma)
p_log_normal  <- trim(p_log_normal)
p_weibull     <- trim(p_weibull)
p_chisq       <- trim(p_chisq)
p_expon       <- trim(p_expon)
p_cauchy       <- trim(p_cauchy)
p_zipf = trim(p_zipf)
p_bernoulli = trim(p_bernoulli)
################
# assemble
slide <- 
  (p1 + p_t + p_laplace + p_logistic + p_cauchy + p_cont_uniform) /
  (p2 + p_gamma + p_log_normal + p_weibull + p_chisq +p_pareto) /
  plot_layout(guides = "collect")

slide

ggsave(
  "/Users/johnslough/Desktop/code/podcast_presentations/everyone_should_know_statistics/assets/dist_continuous.png",slide ,
  width = 12, height = 10, dpi = 1200
) 
#####################



################
## DISCRETE

p_fix1 <- p_binomial +
  labs(y = "Fixed trials") +
  theme(
    axis.title.y = element_text(face = "bold", angle = 0,
                                vjust = 0.5, hjust = 0.5,
                                margin = margin(r = 8)),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) %>% trim()

p_fix2 <- trim(p_hypergeom)
p_fix3 <- trim(p_discrete_uniform)

p_cnt1 <- p_poisson +
  labs(y = "Counting events") +
  theme(
    axis.title.y = element_text(face = "bold", angle = 0,
                                vjust = 0.5, hjust = 0.5,
                                margin = margin(r = 8)),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) %>% trim()

p_cnt2 <- trim(p_geometric)
p_cnt3 <- trim(p_nbinom)

p_fix1 <- p_fix1 + theme(plot.margin = margin(0, 0, 20, 0))        # 15pt below row 1
p_fix2 <- p_fix2 + theme(plot.margin = margin(20, 0, 0, 0))       # 15pt above and below row 2


slide_discrete <-
  (p_fix1 + p_fix2 + p_fix3) /          # 3 (or 4) plots in row 1
  (p_cnt1 + p_cnt2 + p_cnt3) +          # 3 (or 4) plots in row 2
  plot_layout(guides = "collect")

slide_discrete
####################################
# Row-1 label plot already in p_fix1
row_fixed <- p_fix1 + p_fix2 + p_fix3 + p_bernoulli      # 4 plots

# Row-2 label plot already in p_cnt1
row_count <- p_cnt1 + p_cnt2 + p_cnt3 + p_zipf           # 4 plots

slide_discrete <- (
  row_fixed /
    row_count
) + plot_layout(guides = "collect")

slide_discrete

ggsave(
  "/Users/johnslough/Desktop/code/podcast_presentations/everyone_should_know_statistics/assets/dist_discrete.png",slide_discrete ,
  width = 12, height = 10, dpi = 1200
) 
