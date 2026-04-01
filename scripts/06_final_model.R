# =============================================================================
# 06_final_model.R
# Purpose: Fit the final selected model, produce all Section 5 outputs:
#          coefficient table with CIs, model performance metrics,
#          and final residual diagnostics
#
# Final model decisions from 05_model_selection.R:
#   - Drop host_is_superhost (AIC/BIC stepwise, both agree)
#   - Remove 161 influential observations (Cook's D > 4/n)
#   - No predictor transformations (log1p tests worsened AIC)
#   - Final formula: log_price ~ bedrooms + bathrooms + guests_included +
#                                property_type + security_deposit
# =============================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(knitr)

# -----------------------------------------------------------------------------
# Load and prepare data
# -----------------------------------------------------------------------------
df <- as.data.frame(
  read_csv("data/cleaned/listings_cleaned.csv") %>%
    mutate(
      property_type     = factor(property_type,
                                 levels = c("Apartment", "Condominium",
                                            "House", "Other")),
      host_is_superhost = factor(ifelse(host_is_superhost == TRUE, "t", "f"),
                                 levels = c("f", "t"))
    )
)

cat("Full dataset:", nrow(df), "rows\n")

# -----------------------------------------------------------------------------
# Remove influential observations (Cook's D > 4/n from primary model)
# Matches decision made in 05_model_selection.R Part B
# -----------------------------------------------------------------------------
model_primary <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df
)

cooks_d         <- cooks.distance(model_primary)
threshold       <- 4 / nrow(df)
influential_idx <- which(cooks_d > threshold)

df_final <- df[-influential_idx, ]

cat("Removed", length(influential_idx), "influential observations\n")
cat("Final dataset:", nrow(df_final), "rows\n\n")

# =============================================================================
# Fit final model
# Formula: from AIC/BIC stepwise — host_is_superhost dropped
# Data:    df_final — influential observations removed
# =============================================================================
model_final <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + security_deposit,
  data = df_final
)

cat("=== Final Model Summary ===\n")
print(summary(model_final))

# =============================================================================
# Section 5, Criterion 1: Coefficient table with CIs
# =============================================================================
coef_df     <- as.data.frame(summary(model_final)$coefficients)
ci          <- confint(model_final, level = 0.95)

final_table <- data.frame(
  Predictor = c("Intercept", "Bedrooms", "Bathrooms",
                "Guests Included", "Condominium", "House",
                "Other", "Security Deposit"),
  Estimate  = round(coef_df[, "Estimate"], 4),
  Std_Error = round(coef_df[, "Std. Error"], 4),
  CI_Lower  = round(ci[, 1], 4),
  CI_Upper  = round(ci[, 2], 4),
  t_value   = round(coef_df[, "t value"], 3),
  p_value   = round(coef_df[, "Pr(>|t|)"], 4)
)
rownames(final_table) <- NULL

cat("\n--- Final Model Coefficient Table ---\n")
print(final_table)
# In .Rmd use: kable(final_table, digits = 4,
#                    caption = "Final model: coefficients, 95% CIs, and p-values")

# =============================================================================
# Section 5, Criterion 2: Coefficient interpretations
# For log-response model: % change in price = (exp(β) - 1) * 100
# =============================================================================
cat("\n=== Coefficient Interpretations ===\n")
cat("(For report: one-unit increase in X → % change in nightly price)\n\n")

coefs    <- coef(model_final)
coef_nms <- c("Intercept", "Bedrooms", "Bathrooms",
              "Guests Included", "Condominium vs Apt",
              "House vs Apt", "Other vs Apt", "Security Deposit")

for (i in seq_along(coefs)) {
  pct <- (exp(coefs[i]) - 1) * 100
  cat(sprintf("%-25s β = %7.4f → %+.2f%% change in price\n",
              coef_nms[i], coefs[i], pct))
}

# =============================================================================
# Section 5, Criterion 3: Literature comparison — printed as reference
# =============================================================================
cat("\n=== Literature Comparison Reference ===\n")
cat("Bedrooms:   our estimate β =", round(coefs["bedrooms"], 4),
    "→", round((exp(coefs["bedrooms"]) - 1) * 100, 1), "%",
    "| Cai et al. (2019): ~17.8% per bedroom\n")
cat("House type: our estimate β =", round(coefs["property_typeHouse"], 4),
    "→", round((exp(coefs["property_typeHouse"]) - 1) * 100, 1), "%",
    "| Gibbs et al. (2018): private rooms 28-40% below entire homes\n")

# =============================================================================
# Section 5, Criterion 4: Model performance metrics
# =============================================================================
smry <- summary(model_final)

cat("\n=== Model Performance Metrics ===\n")
cat("R-squared:          ", round(smry$r.squared, 4), "\n")
cat("Adjusted R-squared: ", round(smry$adj.r.squared, 4), "\n")
cat("AIC:                ", round(AIC(model_final), 2), "\n")
cat("BIC:                ", round(BIC(model_final), 2), "\n")
cat("Residual Std Error: ", round(smry$sigma, 4), "\n")
cat("F-statistic:        ", round(smry$fstatistic[1], 2),
    "(df:", smry$fstatistic[2], ",", smry$fstatistic[3], ")\n")

# Compare final model vs primary model (full data, all predictors)
cat("\n=== Final vs Primary Model Comparison ===\n")
compare_table <- data.frame(
  Model     = c("Primary (full data, 6 pred.)",
                "Final (trimmed data, 5 pred.)"),
  N         = c(nrow(df), nrow(df_final)),
  AIC       = round(c(AIC(model_primary), AIC(model_final)), 2),
  BIC       = round(c(BIC(model_primary), BIC(model_final)), 2),
  Adj_R2    = round(c(summary(model_primary)$adj.r.squared,
                      smry$adj.r.squared), 4)
)
print(compare_table)

# =============================================================================
# Final model residual diagnostics
# =============================================================================
cat("\n=== Final Model Residual Diagnostics ===\n")

par(mfrow = c(1, 3))
plot(model_final, which = 1, main = "Residuals vs Fitted (Final)")
plot(model_final, which = 2, main = "Q-Q Residuals (Final)")
plot(model_final, which = 3, main = "Scale-Location (Final)")
par(mfrow = c(1, 1))

# Cook's Distance — verify no new influential points after trimming
plot(model_final, which = 4, main = "Cook's Distance (Final Model)")
abline(h = 4 / nrow(df_final), col = "firebrick", lty = 2)

# VIF check on final model
cat("\n=== VIF (Final Model) ===\n")
print(round(car::vif(model_final), 3))

cat("\n=== Script complete. Use outputs above in Section 5 of report. ===\n")