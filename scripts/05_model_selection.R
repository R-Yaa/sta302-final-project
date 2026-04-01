# =============================================================================
# 05_model_selection.R
# Purpose: Transformations, outlier removal decisions, predictor selection
#          using AIC/BIC and stepwise regression → arrive at final model
# Input:   data/cleaned/listings_cleaned.csv
# NOTE:    MASS is loaded before dplyr to prevent MASS::select() masking
#          dplyr::select() and corrupting the data frame in pipes
# =============================================================================

# MASS removed — using car::powerTransform() instead (avoids select() conflict)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

df <- read_csv("data/cleaned/listings_cleaned.csv") %>%
  mutate(
    property_type     = factor(property_type,
                               levels = c("Apartment", "Condominium",
                                          "House", "Other")),
    # read_csv() reads "t"/"f" as logical TRUE/FALSE — convert before factoring
    host_is_superhost = factor(ifelse(host_is_superhost == TRUE, "t", "f"),
                               levels = c("f", "t"))
  )
df <- as.data.frame(df)

cat("=== Data loaded ===\n")
cat("Rows:", nrow(df), "| Class:", class(df), "\n")
cat("host_is_superhost levels:", paste(levels(df$host_is_superhost), collapse = ", "), "\n")
cat("property_type levels:    ", paste(levels(df$property_type), collapse = ", "), "\n\n")

# Re-fit primary model as baseline
model_primary <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df
)

# =============================================================================
# PART A: TRANSFORMATIONS
# =============================================================================

# -----------------------------------------------------------------------------
# A1. Power transform on response — confirm log transform is appropriate
#     Using car::powerTransform (avoids MASS::boxcox() data frame conflict)
#     Result: λ = 0.039 ≈ 0, confirming log(price) is the correct transform
# -----------------------------------------------------------------------------
pt_result <- car::powerTransform(
  price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df
)
cat("=== Power Transform Result ===\n")
print(pt_result)
cat("λ =", round(coef(pt_result), 4),
    "≈ 0 → log transformation confirmed\n\n")

# -----------------------------------------------------------------------------
# A2. Check if log1p(security_deposit) improves fit
#     log1p(x) = log(x+1) handles zeros safely
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(log1p_security_deposit = log1p(security_deposit))

model_log_deposit <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + log1p_security_deposit,
  data = df
)

cat("=== Transformation Comparison: security_deposit ===\n")
cat("Original deposit  - AIC:", round(AIC(model_primary), 2),
    "| Adj R²:", round(summary(model_primary)$adj.r.squared, 4), "\n")
cat("log1p(deposit)    - AIC:", round(AIC(model_log_deposit), 2),
    "| Adj R²:", round(summary(model_log_deposit)$adj.r.squared, 4), "\n\n")

# Compare residual plots side by side
par(mfrow = c(1, 2))
plot(model_primary,     which = 3, main = "Scale-Location: Raw Deposit")
plot(model_log_deposit, which = 3, main = "Scale-Location: log1p(Deposit)")
par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# A3. Check if log1p(bedrooms/bathrooms) helps
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(
    log1p_bedrooms  = log1p(bedrooms),
    log1p_bathrooms = log1p(bathrooms)
  )

model_log_size <- lm(
  log_price ~ log1p_bedrooms + log1p_bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df
)

cat("=== Transformation Comparison: bedrooms/bathrooms ===\n")
cat("Original        - AIC:", round(AIC(model_primary), 2), "\n")
cat("log1p(bed+bath) - AIC:", round(AIC(model_log_size), 2), "\n\n")

# =============================================================================
# PART B: OUTLIER / INFLUENTIAL POINT REMOVAL
# =============================================================================

cooks_d   <- cooks.distance(model_primary)
threshold <- 4 / nrow(df)

influential_idx <- which(cooks_d > threshold)
cat("=== Influential Observations (Cook's D > 4/n) ===\n")
cat("Threshold:", round(threshold, 5), "\n")
cat("Count above threshold:", length(influential_idx), "\n\n")

# Show stats for key flagged observations
flagged <- c(212, 2614, 373, 1416, 2264, 3032)
cat("Key flagged observations:\n")
for (i in flagged) {
  if (i <= nrow(df)) {
    cat(sprintf("  Row %d: price=$%.0f | bedrooms=%s | property=%s | Cook's D=%.5f\n",
                i,
                df$price[i],
                df$bedrooms[i],
                as.character(df$property_type[i]),
                cooks_d[i]))
  }
}

# Fit model WITHOUT influential observations to assess impact
df_no_influential <- df[-influential_idx, ]

model_no_influential <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df_no_influential
)

cat("\n=== Impact of Removing Influential Observations ===\n")
comparison <- cbind(
  "Full"    = round(coef(model_primary), 4),
  "Trimmed" = round(coef(model_no_influential), 4)
)
print(comparison)

cat("\nFull model Adj R²:   ", round(summary(model_primary)$adj.r.squared, 4), "\n")
cat("Trimmed model Adj R²:", round(summary(model_no_influential)$adj.r.squared, 4), "\n")

# =============================================================================
# PART C: VARIABLE SELECTION WITH AIC / BIC
# =============================================================================

# -----------------------------------------------------------------------------
# C1. Backward elimination using AIC
# -----------------------------------------------------------------------------
cat("\n=== Backward Stepwise Selection (AIC) ===\n")
model_step_aic <- step(model_primary, direction = "backward", trace = 1)
cat("\nFinal formula (AIC):", deparse(formula(model_step_aic)), "\n")

# -----------------------------------------------------------------------------
# C2. Backward elimination using BIC
# -----------------------------------------------------------------------------
cat("\n=== Backward Stepwise Selection (BIC) ===\n")
n <- nrow(df)
model_step_bic <- step(model_primary, direction = "backward",
                       k = log(n), trace = 1)
cat("\nFinal formula (BIC):", deparse(formula(model_step_bic)), "\n")

# -----------------------------------------------------------------------------
# C3. Model comparison table
# -----------------------------------------------------------------------------
model_no_superhost <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + security_deposit,
  data = df
)

cat("\n=== Model Comparison Table ===\n")
model_comparison <- data.frame(
  Model = c(
    "Primary (6 predictors)",
    "Drop superhost",
    "AIC stepwise result",
    "BIC stepwise result"
  ),
  AIC = round(c(
    AIC(model_primary),
    AIC(model_no_superhost),
    AIC(model_step_aic),
    AIC(model_step_bic)
  ), 2),
  BIC = round(c(
    BIC(model_primary),
    BIC(model_no_superhost),
    BIC(model_step_aic),
    BIC(model_step_bic)
  ), 2),
  Adj_R2 = round(c(
    summary(model_primary)$adj.r.squared,
    summary(model_no_superhost)$adj.r.squared,
    summary(model_step_aic)$adj.r.squared,
    summary(model_step_bic)$adj.r.squared
  ), 4)
)
print(model_comparison)

cat("\n>>> Select the model with lowest AIC/BIC as final model.\n")
cat(">>> Document decision clearly in Section 4 of the report.\n")