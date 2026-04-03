# =============================================================================
# 04_diagnostics.R
# Purpose: Full residual diagnostics and influential point analysis
#          for the primary model
# Input:   data/cleaned/listings_cleaned.csv
# Depends: Run 03_primary_model.R first to have model_primary in environment,
#          OR source it here:
# =============================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(car)       # for influencePlot(), vif()
library(patchwork)

df <- read_csv("data/cleaned/listings_cleaned.csv") %>%
  mutate(
    property_type     = factor(property_type,
                               levels = c("Apartment", "Condominium",
                                          "House", "Other")),
    host_is_superhost = factor(ifelse(host_is_superhost == TRUE, "t", "f"), levels = c("f", "t"))
  )

# Re-fit primary model
model_primary <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df
)

# -----------------------------------------------------------------------------
# 1. Standard residual plots (matches proposal figures)
# -----------------------------------------------------------------------------
par(mfrow = c(1, 3))
plot(model_primary, which = 1, main = "Residuals vs Fitted")   # linearity
plot(model_primary, which = 2, main = "Q-Q Residuals")          # normality
plot(model_primary, which = 3, main = "Scale-Location")         # homoscedasticity
par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# 2. Residuals vs each predictor (check linearity per predictor)
# -----------------------------------------------------------------------------
df_diag <- df %>%
  mutate(
    residuals = residuals(model_primary),
    fitted    = fitted(model_primary)
  )

p_bed <- ggplot(df_diag, aes(x = bedrooms, y = residuals)) +
  geom_jitter(alpha = 0.2, width = 0.15, colour = "steelblue") +
  geom_hline(yintercept = 0, colour = "firebrick", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "black", linewidth = 0.7) +
  labs(title = "Residuals vs Bedrooms", x = "Bedrooms", y = "Residuals")

p_bath <- ggplot(df_diag, aes(x = bathrooms, y = residuals)) +
  geom_jitter(alpha = 0.2, width = 0.1, colour = "steelblue") +
  geom_hline(yintercept = 0, colour = "firebrick", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "black", linewidth = 0.7) +
  labs(title = "Residuals vs Bathrooms", x = "Bathrooms", y = "Residuals")

p_guests <- ggplot(df_diag, aes(x = guests_included, y = residuals)) +
  geom_jitter(alpha = 0.2, width = 0.15, colour = "steelblue") +
  geom_hline(yintercept = 0, colour = "firebrick", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "black", linewidth = 0.7) +
  labs(title = "Residuals vs Guests Included",
       x = "Guests Included", y = "Residuals")

p_dep <- ggplot(df_diag, aes(x = security_deposit, y = residuals)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_hline(yintercept = 0, colour = "firebrick", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "black", linewidth = 0.7) +
  labs(title = "Residuals vs Security Deposit",
       x = "Security Deposit ($)", y = "Residuals")

(p_bed + p_bath) / (p_guests + p_dep)

# -----------------------------------------------------------------------------
# 3. Cook's Distance — identify influential points
# -----------------------------------------------------------------------------
cooks_d    <- cooks.distance(model_primary)
threshold  <- 4 / nrow(df)   # common rule of thumb: 4/n

cat("=== Cook's Distance Analysis ===\n")
cat("Threshold (4/n):", round(threshold, 5), "\n")
cat("Observations above threshold:", sum(cooks_d > threshold), "\n\n")

# Top 10 most influential
top_influential <- sort(cooks_d, decreasing = TRUE)[1:10]
cat("Top 10 influential observations:\n")
print(round(top_influential, 5))

# Cook's Distance plot
par(mfrow = c(1, 1))
plot(model_primary, which = 4, main = "Cook's Distance")
abline(h = threshold, col = "firebrick", lty = 2)

# -----------------------------------------------------------------------------
# 4. Leverage and studentized residuals
# -----------------------------------------------------------------------------
leverage  <- hatvalues(model_primary)
stud_res  <- rstudent(model_primary)
# Lecture 8: threshold is 2(p+1)/n where p+1 = number of parameters
# including intercept. ncol(model.matrix()) gives p+1 directly.
p_plus_1  <- ncol(model.matrix(model_primary))  # p+1 (includes intercept)
n          <- nrow(df)
lev_thresh <- 2 * p_plus_1 / n   # Hoaglin & Welsch (1978): 2(p+1)/n

cat("\n=== Leverage Analysis ===\n")
cat("High leverage threshold 2(p+1)/n:", round(lev_thresh, 4), "\n")
cat("  where p+1 =", p_plus_1, "and n =", n, "\n")
cat("High leverage observations:", sum(leverage > lev_thresh), "\n")

# Flag the three observations from proposal residual plots
flagged_obs <- c(2286, 3056, 2034)
cat("\n--- Flagged observations from proposal ---\n")
for (obs in flagged_obs) {
  # Find row by original index in df
  cat(sprintf("Obs %d: Cook's D = %.5f | Leverage = %.5f | Stud. Res = %.3f\n",
              obs,
              cooks_d[obs],
              leverage[obs],
              stud_res[obs]))
}

# -----------------------------------------------------------------------------
# 5. Influence plot (leverage vs studentized residuals, bubble = Cook's D)
# -----------------------------------------------------------------------------
influencePlot(model_primary,
              main = "Influence Plot",
              sub  = "Circle size proportional to Cook's Distance")

# -----------------------------------------------------------------------------
# 6. Multicollinearity — Variance Inflation Factors
# -----------------------------------------------------------------------------
cat("\n=== Variance Inflation Factors ===\n")
vif_vals <- vif(model_primary)
print(round(vif_vals, 3))

# Rule of thumb: VIF > 5 is concerning, > 10 is severe
cat("\nVIF > 5 (concerning):", names(vif_vals[vif_vals > 5]), "\n")