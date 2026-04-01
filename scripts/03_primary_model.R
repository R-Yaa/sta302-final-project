# =============================================================================
# 03_primary_model.R
# Purpose: Fit the preliminary 6-predictor model from the proposal
# Input:   data/cleaned/listings_cleaned.csv
# Output:  model object `model_primary` for use in report
# =============================================================================

library(dplyr)
library(readr)
library(knitr)

df <- read_csv("data/cleaned/listings_cleaned.csv")

# Fix: read_csv stores host_is_superhost as logical TRUE/FALSE — convert to factor directly
df <- df %>%
  mutate(
    host_is_superhost = factor(ifelse(host_is_superhost == TRUE, "t", "f"),
                               levels = c("f", "t")),
    property_type     = factor(property_type,
                               levels = c("Apartment", "Condominium", "House", "Other"))
  )

# Ensure correct factor levels (Apartment as reference — largest group)
df <- df %>%
  mutate(
    property_type     = factor(property_type,
                               levels = c("Apartment", "Condominium",
                                          "House", "Other")),
    host_is_superhost = factor(host_is_superhost, levels = c("f", "t"))
  )

# -----------------------------------------------------------------------------
# Fit primary model (matches proposal exactly)
# -----------------------------------------------------------------------------
model_primary <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df
)

# -----------------------------------------------------------------------------
# Summary outputs
# -----------------------------------------------------------------------------
summary_primary <- summary(model_primary)
cat("=== Primary Model Summary ===\n")
print(summary_primary)

# Coefficient table (clean format for report)
coef_table <- as.data.frame(summary_primary$coefficients)
coef_table$Predictor <- rownames(coef_table)
coef_table <- coef_table[, c("Predictor", "Estimate", "Std. Error",
                             "t value", "Pr(>|t|)")]
colnames(coef_table) <- c("Predictor", "Estimate", "Std. Error",
                          "t-value", "p-value")

cat("\n--- Coefficient Table ---\n")
print(round(coef_table[, 2:5], 4))

# Model fit statistics
cat("\n--- Model Fit ---\n")
cat("R-squared:         ", round(summary_primary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary_primary$adj.r.squared, 4), "\n")
cat("F-statistic:       ", round(summary_primary$fstatistic[1], 2), "\n")
cat("AIC:               ", round(AIC(model_primary), 2), "\n")
cat("BIC:               ", round(BIC(model_primary), 2), "\n")