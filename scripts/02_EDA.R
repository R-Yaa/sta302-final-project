# =============================================================================
# 02_eda.R
# Purpose: Exploratory data analysis — distributions and relationships
# Input:   data/cleaned/listings_cleaned.csv
# =============================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)

df <- read_csv("data/cleaned/listings_cleaned.csv")

# Set consistent theme
theme_set(theme_bw(base_size = 11))

# -----------------------------------------------------------------------------
# Figure 1: Response variable — price (raw) vs log_price
# -----------------------------------------------------------------------------
p1a <- ggplot(df, aes(x = price)) +
  geom_histogram(bins = 60, fill = "steelblue", colour = "white") +
  labs(title = "A. Price Distribution (Raw)",
       x = "Nightly Price ($)", y = "Count") +
  xlim(0, 1500)   # trim extreme outliers for readability

p1b <- ggplot(df, aes(x = log_price)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  labs(title = "B. Log Price Distribution",
       x = "log(Nightly Price)", y = "Count")

fig1 <- p1a + p1b
fig1
# ggsave("output/fig1_price_distribution.png", fig1, width = 10, height = 4)

# -----------------------------------------------------------------------------
# Figure 2: Continuous predictors vs log_price (scatter)
# -----------------------------------------------------------------------------
p2a <- ggplot(df, aes(x = bedrooms, y = log_price)) +
  geom_jitter(alpha = 0.2, width = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "A. Bedrooms vs log(Price)",
       x = "Bedrooms", y = "log(Price)")

p2b <- ggplot(df, aes(x = bathrooms, y = log_price)) +
  geom_jitter(alpha = 0.2, width = 0.15, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "B. Bathrooms vs log(Price)",
       x = "Bathrooms", y = "log(Price)")

p2c <- ggplot(df, aes(x = guests_included, y = log_price)) +
  geom_jitter(alpha = 0.2, width = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "C. Guests Included vs log(Price)",
       x = "Guests Included", y = "log(Price)")

p2d <- ggplot(df, aes(x = security_deposit, y = log_price)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "D. Security Deposit vs log(Price)",
       x = "Security Deposit ($)", y = "log(Price)")

fig2 <- (p2a + p2b) / (p2c + p2d)
fig2
# ggsave("output/fig2_predictors_vs_logprice.png", fig2, width = 10, height = 8)

# -----------------------------------------------------------------------------
# Figure 3: Predictor distributions (for data description section)
# -----------------------------------------------------------------------------
p3a <- ggplot(df, aes(x = factor(bedrooms))) +
  geom_bar(fill = "steelblue") +
  labs(title = "A. Bedrooms", x = "Count", y = "Frequency")

p3b <- ggplot(df, aes(x = factor(bathrooms))) +
  geom_bar(fill = "steelblue") +
  labs(title = "B. Bathrooms", x = "Count", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3c <- ggplot(df, aes(x = host_is_superhost)) +
  geom_bar(fill = "steelblue") +
  labs(title = "C. Superhost Status", x = "Status", y = "Frequency")

p3d <- ggplot(df, aes(x = factor(guests_included))) +
  geom_bar(fill = "steelblue") +
  labs(title = "D. Guests Included", x = "Count", y = "Frequency")

p3e <- ggplot(df, aes(y = property_type)) +
  geom_bar(fill = "steelblue") +
  labs(title = "E. Property Type", x = "Frequency", y = NULL)

fig3 <- (p3a + p3b + p3c) / (p3d + p3e)
fig3
# ggsave("output/fig3_predictor_distributions.png", fig3, width = 12, height = 7)

# -----------------------------------------------------------------------------
# Figure 4: Log(security_deposit) — check if log transform helps
# -----------------------------------------------------------------------------
df_nonzero_deposit <- df %>% filter(security_deposit > 0)

p4a <- ggplot(df_nonzero_deposit, aes(x = security_deposit, y = log_price)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "A. Security Deposit vs log(Price) (raw)",
       x = "Security Deposit ($)", y = "log(Price)")

p4b <- ggplot(df_nonzero_deposit,
              aes(x = log(security_deposit), y = log_price)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "B. log(Security Deposit) vs log(Price)",
       x = "log(Security Deposit)", y = "log(Price)")

fig4 <- p4a + p4b
fig4
# ggsave("output/fig4_deposit_transform_check.png", fig4, width = 10, height = 4)

# -----------------------------------------------------------------------------
# Numeric summary table (for report)
# -----------------------------------------------------------------------------
summary_stats <- df %>%
  select(log_price, bedrooms, bathrooms, guests_included, security_deposit) %>%
  summarise(across(everything(),
                   list(
                     Mean   = ~round(mean(.x, na.rm = TRUE), 2),
                     SD     = ~round(sd(.x, na.rm = TRUE), 2),
                     Min    = ~round(min(.x, na.rm = TRUE), 2),
                     Median = ~round(median(.x, na.rm = TRUE), 2),
                     Max    = ~round(max(.x, na.rm = TRUE), 2)
                   )
  ))

cat("Summary statistics computed. Use kable() in .Rmd to display.\n")
print(summary_stats)