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

############################
# DISTRIBUTIONS!!!###########
############################

make_dist_plot <- function(dist_fun, ...,
                           discrete = FALSE,
                           title    = "",
                           adjust   = 1.5,
                           n_sample = 1e5,
                           xlim     = NULL) {
  
  # Default xlim for continuous if not set
  if (!discrete && is.null(xlim)) {
    xlim <- c(-10, 10)
  }
  
  x <- dist_fun(n_sample, ...)
  if (!discrete && !is.null(xlim)) {
    x <- x[x >= xlim[1] & x <= xlim[2]]
  }
  
  df <- data.frame(x)
  
  p <- ggplot(df, aes(x)) +
    {
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
      plot.title = element_text(size = 16, hjust = 0.5, vjust = -1),
      panel.grid = element_blank()
    )
  
  if (!discrete)
    p <- p + ylim(0, 1.05)
  
  if (!discrete && !is.null(xlim))
    p <- p + coord_cartesian(xlim = xlim, expand = FALSE)
  
  p
}


# uniform treat specially
make_uniform_plot <- function(min_val = -1, max_val = 1, title = "Cont. Uniform") {
  df <- data.frame(x = c(min_val, max_val), y = 1)
  
  ggplot() +
    geom_ribbon(aes(x = x, ymin = 0, ymax = 1), fill = "skyblue", alpha = 0.6,
                data = data.frame(x = seq(min_val, max_val, length.out = 100))) +
    geom_segment(aes(x = min_val, xend = max_val, y = 1, yend = 1), color = "black") +
    labs(title = title) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 16, hjust = 0.5, vjust = -1),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "pt")
    ) +
    ylim(0, 1.05)
}



set.seed(1)
n_sample = 100000
# Continuous
p_normal     <- make_dist_plot(rnorm, mean = 0, sd = 1,               title = "Normal")
p_expon      <- make_dist_plot(rexp, rate = 1,                        title = "Exponential",     adjust = 2.0)
p_chisq      <- make_dist_plot(rchisq, df = 3,                        title = "Chi-Squared",     adjust = 2.0)
p_gamma      <- make_dist_plot(rgamma, shape = 2, scale = 2,          title = "Gamma",           adjust = 2.0)
#p_beta       <- make_dist_plot(rbeta, 2, 5,                           title = "Beta",            adjust = 2.5)
p_t          <- make_dist_plot(rt, df = 5,                            title = "t",               adjust = 1.8)
p_log_normal <- make_dist_plot(rlnorm, meanlog = 0, sdlog = 0.5,      title = "Log-Normal",      adjust = 2.0)
p_weibull    <- make_dist_plot(rweibull, shape = 1.5, scale = 1,      title = "Weibull",         adjust = 2.0)
p_cont_uniform <- make_uniform_plot(min_val = -10, max_val = 10, title = "Cont. Uniform")
p_laplace    <- make_dist_plot(function(n) rlaplace(n, 0, 1),         title = "Laplace",         adjust = 1.8)
p_logistic <- make_dist_plot(rlogis, location = 0, scale = 1, title = "Logistic", adjust = 1.8)
p_cauchy <- make_dist_plot(rcauchy, location = 0, scale = 1, title    = "Cauchy",adjust   = 0.9, xlim = c(-20, 20))
p_pareto <- make_dist_plot(rpareto, scale = 10000, shape    = 1, n_sample = n_sample, title    = "Pareto (log scale)",xlim     = c(1e2, 1e6), adjust   = 2)+  scale_x_log10(expand = c(0,0))

##

# Discrete
p_binomial   <- make_dist_plot(rbinom, size = 10, prob = 0.5,         title = "Binomial",       discrete = TRUE)
p_poisson    <- make_dist_plot(rpois, lambda = 3,                     title = "Poisson",        discrete = TRUE)
p_geometric  <- make_dist_plot(rgeom, prob = 0.3,                     title = "Geometric",      discrete = TRUE)
p_nbinom     <- make_dist_plot(rnbinom, size = 5, prob = 0.5,         title = "Neg. Binomial",  discrete = TRUE)
p_hypergeom  <- make_dist_plot(rhyper, m = 30, n = 70, k = 10,        title = "Hypergeometric", discrete = TRUE)
p_discrete_uniform <- make_dist_plot(function(n) sample(0:9, n, replace = TRUE), 
                                     title = "Discrete Uniform",      discrete = TRUE) +
  scale_x_continuous(breaks = 0:10, expand = c(0.1, 0))
p_bernoulli <- make_dist_plot(rbinom, size = 1, prob = 0.3, title     = "Bernoulli", discrete  = TRUE)

zipf_sampler <- function(n, s = 1.1, N = 100) {
  ranks <- 1:N
  probs <- ranks^(-s)
  probs <- probs / sum(probs)
  sample(ranks, n, replace = TRUE, prob = probs)
}

p_zipf <- make_dist_plot(
  zipf_sampler,
  title = "Zipf–Mandelbrot",
  discrete = TRUE
) +
  coord_cartesian(xlim = c(1, 50)) +
  theme(axis.text.x = element_blank()) + xlim(0,50)

library(patchwork)

#######################
# a tiny helper to zero out all margins on a plot
trim <- function(p) p + theme(plot.margin = margin(0,0,0,0,"pt"))

# add row label as y-axis title on the first plot of each row
p1 <- p_normal + 
  labs(y = "Symmetric") +
  theme(
    axis.title.y     = element_text(angle = 0, vjust = 0.5, hjust = 0.5,face   = "bold", size = 18,
                                    margin = margin(r = 8)), 
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank()
  ) %>% trim()

p2 <- p_expon + 
  labs(y = "Skewed") +
  theme(
    axis.title.y     = element_text(angle = 0, vjust = 0.5, hjust = 0.5, face   = "bold", size = 18,
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
# annotate normal dist:

mu     <- 0
sigma  <- 2.5
x_rng  <- c(mu - 4*sigma, mu + 4*sigma)

# base curve, peak scaled to 1
p_base <- make_dist_plot(rnorm,
                         mean   = mu,
                         sd     = sigma,
                         adjust = 10,
                         title  = NULL) +
  coord_cartesian(xlim = x_rng, ylim = c(0, 1.05), expand = FALSE)

p <- p_base +
  
  ## centre line  (μ)
  geom_vline(xintercept = mu,
             colour = "#C04E3D", linetype = "dashed") +
  annotate("text", x = mu, y = 0.8, label = "mean~(mu)",
           parse = TRUE, hjust = .5, vjust = 0, size = 4) +
  
  ## peak = 1/(σ√2π)
  annotate("text",
           x     = mu,
           y     = 1.025,
           label = "peak==1/(sigma*sqrt(2*pi))",
           parse = TRUE, hjust = .5, size = 4) +
  
  ## one-sigma “ruler”  (horizontal arrow)
  annotate("segment",
           x     = mu - sigma, xend = mu + sigma,
           y     = 0.12,       yend = 0.12,
           colour = "#BFA06D", linewidth = 1,
           arrow  = arrow(ends = "both", type = "closed",
                          length = unit(0.18, "in"))) +
  
  # centred σ label just above the arrow
  annotate("text",
           x = mu, y = 0.15,
           label = "\u03C3",      # or just "'\\u03C3'" for the symbol
           size = 6) +
  
  ## decay label near the right shoulder
  annotate("text",
           x     = mu + 1.62*sigma,
           y     = 0.52,
           label = "e^{-frac(1,2)*((x - mu)/sigma)^2}",
           parse = TRUE, hjust = 0, size = 4) +
  annotate("text",
           x     = mu + 1.47*sigma,
           y     = 0.58,
           label = "exponential~decay",
           parse = TRUE, hjust = 0, size = 4)

p  
ggsave(
  "/Users/johnslough/Desktop/code/podcast_presentations/everyone_should_know_statistics/assets/normal_annotated.png",p  ,
  width = 6, height = 4, dpi = 1200, bg="white"
) 


################
## DISCRETE

p_fix1 <- p_binomial +
  labs(y = "Fixed trials") +
  theme(
    axis.title.y = element_text(face = "bold", angle = 0, size = 18,
                                vjust = 0.5, hjust = 0.5,
                                margin = margin(r = 8)),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) %>% trim()

p_fix2 <- trim(p_hypergeom)
p_fix3 <- trim(p_discrete_uniform)
p_fix4 = trim(p_bernoulli)

p_cnt1 <- p_poisson +
  labs(y = "Counting events") +
  theme(
    axis.title.y = element_text(face = "bold", angle = 0, size = 18,
                                vjust = 0.5, hjust = 0.5,
                                margin = margin(r = 8)),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) %>% trim()

p_cnt2 <- trim(p_geometric)
p_cnt3 <- trim(p_nbinom)
p_cnt4 = trim(p_zipf)

p_fix1 <- p_fix1 + theme(plot.margin = margin(0, 0, 20, 0))        
p_fix2 <- p_fix2 + theme(plot.margin = margin(20, 0, 0, 0))       


slide_discrete <-
  (p_fix1 + p_fix2 + p_fix3) /          # 3 (or 4) plots in row 1
  (p_cnt1 + p_cnt2 + p_cnt3) +          # 3 (or 4) plots in row 2
  plot_layout(guides = "collect")

slide_discrete
####################################
# Row-1 label plot already in p_fix1
row_fixed <- p_fix1 + p_fix2 + p_fix3 + p_fix4      # 4 plots

# Row-2 label plot already in p_cnt1
row_count <- p_cnt1 + p_cnt2 + p_cnt3 + p_cnt4           # 4 plots

slide_discrete <- (
  row_fixed /
    row_count
) + plot_layout(guides = "collect")

slide_discrete

ggsave(
  "/Users/johnslough/Desktop/code/podcast_presentations/everyone_should_know_statistics/assets/dist_discrete.png",slide_discrete ,
  width = 12, height = 10, dpi = 1200
) 


################
# anscombe
library(tidyverse)

# Define the data
anscombe <- tibble::tibble(
  x1 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
  y1 = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68),
  x2 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
  y2 = c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74),
  x3 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
  y3 = c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73),
  x4 = c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8),
  y4 = c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.89)
)



make_anscombe_plot <- function(data, set_label, xlim = c(4, 19), ylim = c(3, 13), show_line = TRUE) {
  
  p <- ggplot(data, aes(x, y)) +
    geom_point(size = 4, color = "#3C4756", fill = "#5A6B80", shape = 21, stroke = 1.2) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = TRUE) +
    theme_classic(base_size = 16) +
    labs(x = "x", y = "y", title = paste0("Anscombe ", set_label)) +
    theme(
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  if (show_line) {
    model <- lm(y ~ x, data = data)
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    corr <- cor(data$x, data$y)
    eq <- paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "x")
    
    p <- p +
      geom_abline(intercept = intercept, slope = slope, color = "#C04E3D", linewidth = 1.5) +
      annotate("text", x = 4, y = 12, hjust = 0, family = "mono",
               label = paste0("Corr: ", round(corr, 2), "\n", eq),
               size = 6)
  }
  
  return(p)
}

p1 <- make_anscombe_plot(tibble(x = anscombe$x1, y = anscombe$y1), "Set 1", show_line = FALSE)
p2 <- make_anscombe_plot(tibble(x = anscombe$x2, y = anscombe$y2), "Set 2", show_line = FALSE)
p3 <- make_anscombe_plot(tibble(x = anscombe$x3, y = anscombe$y3), "Set 3", show_line = FALSE)
p4 <- make_anscombe_plot(tibble(x = anscombe$x4, y = anscombe$y4), "Set 4", show_line = FALSE)

final_plot <- (p1 + p2) / (p3 + p4)
final_plot

ggsave(
  "/Users/johnslough/Desktop/code/podcast_presentations/everyone_should_know_statistics/assets/anscombe_data.png",final_plot ,
  width = 12, height = 10, dpi = 1200
) 


p1 <- make_anscombe_plot(tibble(x = anscombe$x1, y = anscombe$y1), "Set 1")
p2 <- make_anscombe_plot(tibble(x = anscombe$x2, y = anscombe$y2), "Set 2")
p3 <- make_anscombe_plot(tibble(x = anscombe$x3, y = anscombe$y3), "Set 3")
p4 <- make_anscombe_plot(tibble(x = anscombe$x4, y = anscombe$y4), "Set 4")

final_plot <- (p1 + p2) / (p3 + p4)
final_plot

ggsave(
  "/Users/johnslough/Desktop/code/podcast_presentations/everyone_should_know_statistics/assets/anscombe_data_corr.png",final_plot ,
  width = 12, height = 10, dpi = 1200
) 