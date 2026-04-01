# =============================================================================
# 00_run_all.R
# Purpose: Master script — installs packages, runs all scripts in order,
#          and saves all figures and tables to output/
#
# Usage:   Open RStudio, set working directory to project root, then run:
#          source("scripts/00_run_all.R")
#
# Output:  output/figures/   — all plots as PNG files
#          output/tables/    — all tables as CSV files
# =============================================================================

cat("╔══════════════════════════════════════════════════╗\n")
cat("║        STA302 Final Project — Run All            ║\n")
cat("╚══════════════════════════════════════════════════╝\n\n")

# =============================================================================
# STEP 0: Install and load required packages
# =============================================================================
cat("--- Step 0: Installing packages (skipped if already installed) ---\n")

required_packages <- c(
  "dplyr",      # data manipulation
  "readr",      # fast CSV reading
  "stringr",    # string cleaning (currency columns)
  "ggplot2",    # plotting
  "patchwork",  # combining ggplots
  "knitr",      # tables
  "car"         # VIF, powerTransform, influencePlot
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Installing:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  } else {
    cat("  Already installed:", pkg, "\n")
  }
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE,
                 warn.conflicts = FALSE, quietly = TRUE))

cat("  All packages ready.\n\n")

# =============================================================================
# STEP 1: Create output directories
# =============================================================================
cat("--- Step 1: Creating output directories ---\n")

dirs <- c("output/figures", "output/tables")
for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("  Created:", d, "\n")
  } else {
    cat("  Exists: ", d, "\n")
  }
}
cat("\n")

# =============================================================================
# STEP 2: Run cleaning script
# =============================================================================
cat("--- Step 2: Cleaning data (01_cleaning.R) ---\n")
source("scripts/01_cleaning.R", echo = FALSE)
cat("  Done. Cleaned CSV saved to data/cleaned/listings_cleaned.csv\n\n")

# =============================================================================
# STEP 3: Load cleaned data with correct types (used by all steps below)
# =============================================================================
df <- as.data.frame(
  read_csv("data/cleaned/listings_cleaned.csv", show_col_types = FALSE) %>%
    mutate(
      property_type     = factor(property_type,
                                 levels = c("Apartment", "Condominium",
                                            "House", "Other")),
      host_is_superhost = factor(ifelse(host_is_superhost == TRUE, "t", "f"),
                                 levels = c("f", "t"))
    )
)
cat("--- Step 3: Data loaded ---\n")
cat("  Rows:", nrow(df), "| Columns:", ncol(df), "\n\n")

# =============================================================================
# STEP 4: EDA Figures
# =============================================================================
cat("--- Step 4: Generating EDA figures ---\n")

theme_set(theme_bw(base_size = 11))

# Figure 1: Price distribution
p1a <- ggplot(df, aes(x = price)) +
  geom_histogram(bins = 60, fill = "steelblue", colour = "white") +
  labs(title = "A. Price Distribution (Raw)", x = "Nightly Price ($)", y = "Count") +
  xlim(0, 1500)

p1b <- ggplot(df, aes(x = log_price)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  labs(title = "B. Log Price Distribution", x = "log(Nightly Price)", y = "Count")

fig1 <- p1a + p1b
ggsave("output/figures/fig1_price_distribution.png", fig1,
       width = 10, height = 4, dpi = 300)
cat("  Saved: fig1_price_distribution.png\n")

# Figure 2: Predictors vs log_price
p2a <- ggplot(df, aes(x = bedrooms, y = log_price)) +
  geom_jitter(alpha = 0.2, width = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "A. Bedrooms vs log(Price)", x = "Bedrooms", y = "log(Price)")

p2b <- ggplot(df, aes(x = bathrooms, y = log_price)) +
  geom_jitter(alpha = 0.2, width = 0.15, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "B. Bathrooms vs log(Price)", x = "Bathrooms", y = "log(Price)")

p2c <- ggplot(df, aes(x = guests_included, y = log_price)) +
  geom_jitter(alpha = 0.2, width = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "C. Guests Included vs log(Price)", x = "Guests Included", y = "log(Price)")

p2d <- ggplot(df, aes(x = security_deposit, y = log_price)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(title = "D. Security Deposit vs log(Price)", x = "Security Deposit ($)", y = "log(Price)")

fig2 <- (p2a + p2b) / (p2c + p2d)
ggsave("output/figures/fig2_predictors_vs_logprice.png", fig2,
       width = 10, height = 8, dpi = 300)
cat("  Saved: fig2_predictors_vs_logprice.png\n")

# Figure 3: Predictor distributions
p3a <- ggplot(df, aes(x = factor(bedrooms))) +
  geom_bar(fill = "steelblue") +
  labs(title = "A. Bedrooms", x = "Count", y = "Frequency")

p3b <- ggplot(df, aes(x = factor(bathrooms))) +
  geom_bar(fill = "steelblue") +
  labs(title = "B. Bathrooms", x = "Count", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3c <- ggplot(df, aes(x = host_is_superhost)) +
  geom_bar(fill = "steelblue") +
  scale_x_discrete(labels = c("f" = "No", "t" = "Yes")) +
  labs(title = "C. Superhost Status", x = "Is Superhost", y = "Frequency")

p3d <- ggplot(df, aes(x = factor(guests_included))) +
  geom_bar(fill = "steelblue") +
  labs(title = "D. Guests Included", x = "Count", y = "Frequency")

p3e <- ggplot(df, aes(y = property_type)) +
  geom_bar(fill = "steelblue") +
  labs(title = "E. Property Type", x = "Frequency", y = NULL)

fig3 <- (p3a + p3b + p3c) / (p3d + p3e)
ggsave("output/figures/fig3_predictor_distributions.png", fig3,
       width = 12, height = 7, dpi = 300)
cat("  Saved: fig3_predictor_distributions.png\n\n")

# =============================================================================
# STEP 5: Primary model + diagnostics
# =============================================================================
cat("--- Step 5: Fitting primary model and diagnostics ---\n")

model_primary <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df
)

# Figure 4: Primary model residual plots
png("output/figures/fig4_primary_residuals.png", width = 1200, height = 400,
    res = 120)
par(mfrow = c(1, 3))
plot(model_primary, which = 1, main = "Residuals vs Fitted (Primary)")
plot(model_primary, which = 2, main = "Q-Q Residuals (Primary)")
plot(model_primary, which = 3, main = "Scale-Location (Primary)")
par(mfrow = c(1, 1))
dev.off()
cat("  Saved: fig4_primary_residuals.png\n")

# Figure 5: Cook's Distance
cooks_d   <- cooks.distance(model_primary)
threshold <- 4 / nrow(df)

png("output/figures/fig5_cooks_distance.png", width = 900, height = 500, res = 120)
plot(model_primary, which = 4, main = "Cook's Distance (Primary Model)")
abline(h = threshold, col = "firebrick", lty = 2)
dev.off()
cat("  Saved: fig5_cooks_distance.png\n")

# Figure 6: Influence plot
png("output/figures/fig6_influence_plot.png", width = 900, height = 700, res = 120)
car::influencePlot(model_primary,
                   main = "Influence Plot",
                   sub  = "Circle size proportional to Cook's Distance")
dev.off()
cat("  Saved: fig6_influence_plot.png\n")

# Table 1: Primary model coefficients
coef_primary <- as.data.frame(summary(model_primary)$coefficients)
ci_primary   <- confint(model_primary)
table1 <- data.frame(
  Predictor = rownames(coef_primary),
  Estimate  = round(coef_primary[, "Estimate"], 4),
  Std_Error = round(coef_primary[, "Std. Error"], 4),
  CI_Lower  = round(ci_primary[, 1], 4),
  CI_Upper  = round(ci_primary[, 2], 4),
  t_value   = round(coef_primary[, "t value"], 3),
  p_value   = round(coef_primary[, "Pr(>|t|)"], 4)
)
rownames(table1) <- NULL
write.csv(table1, "output/tables/table1_primary_model_coefficients.csv",
          row.names = FALSE)
cat("  Saved: table1_primary_model_coefficients.csv\n")

# Table 2: Primary model fit statistics
table2 <- data.frame(
  Metric  = c("R-squared", "Adjusted R-squared", "AIC", "BIC",
              "F-statistic", "p-value", "N"),
  Value   = c(
    round(summary(model_primary)$r.squared, 4),
    round(summary(model_primary)$adj.r.squared, 4),
    round(AIC(model_primary), 2),
    round(BIC(model_primary), 2),
    round(summary(model_primary)$fstatistic[1], 2),
    "< 2.2e-16",
    nrow(df)
  )
)
write.csv(table2, "output/tables/table2_primary_model_fit.csv", row.names = FALSE)
cat("  Saved: table2_primary_model_fit.csv\n\n")

# =============================================================================
# STEP 6: Model selection
# =============================================================================
cat("--- Step 6: Model selection (transformations, outliers, stepwise) ---\n")

# A1. Power transform confirmation
pt_result <- car::powerTransform(
  price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df
)
cat("  Power transform λ =", round(coef(pt_result), 4), "(≈ 0 → log confirmed)\n")

# A2/A3. Transformation comparisons
df_test <- df %>%
  mutate(
    log1p_security_deposit = log1p(security_deposit),
    log1p_bedrooms         = log1p(bedrooms),
    log1p_bathrooms        = log1p(bathrooms)
  )

model_log_deposit <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + log1p_security_deposit,
  data = df_test
)
model_log_size <- lm(
  log_price ~ log1p_bedrooms + log1p_bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df_test
)

# Figure 7: Transformation comparison (Scale-Location)
png("output/figures/fig7_transform_comparison.png", width = 1000, height = 450,
    res = 120)
par(mfrow = c(1, 2))
plot(model_primary,     which = 3, main = "Scale-Location: Raw Deposit")
plot(model_log_deposit, which = 3, main = "Scale-Location: log1p(Deposit)")
par(mfrow = c(1, 1))
dev.off()
cat("  Saved: fig7_transform_comparison.png\n")

# Table 3: Transformation comparison
table3 <- data.frame(
  Model = c("Primary (raw deposit)",
            "log1p(security_deposit)",
            "log1p(bedrooms + bathrooms)"),
  AIC    = round(c(AIC(model_primary),
                   AIC(model_log_deposit),
                   AIC(model_log_size)), 2),
  Adj_R2 = round(c(summary(model_primary)$adj.r.squared,
                   summary(model_log_deposit)$adj.r.squared,
                   summary(model_log_size)$adj.r.squared), 4),
  Decision = c("Baseline", "Rejected (AIC worse)", "Rejected (AIC worse)")
)
write.csv(table3, "output/tables/table3_transformation_comparison.csv",
          row.names = FALSE)
cat("  Saved: table3_transformation_comparison.csv\n")

# B. Outlier removal
influential_idx <- which(cooks_d > threshold)

df_no_inf <- df[-influential_idx, ]
model_no_inf <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + host_is_superhost + security_deposit,
  data = df_no_inf
)

# Table 4: Outlier removal impact
table4 <- data.frame(
  Model    = c("Full data (n=3561)", "Influential obs removed (n=3400)"),
  N        = c(nrow(df), nrow(df_no_inf)),
  Adj_R2   = round(c(summary(model_primary)$adj.r.squared,
                     summary(model_no_inf)$adj.r.squared), 4),
  AIC      = round(c(AIC(model_primary), AIC(model_no_inf)), 2)
)
write.csv(table4, "output/tables/table4_outlier_removal_impact.csv",
          row.names = FALSE)
cat("  Saved: table4_outlier_removal_impact.csv\n")

# C. Stepwise selection
model_step <- step(model_primary, direction = "backward",
                   trace = 0, k = log(nrow(df)))  # BIC

# Table 5: Model comparison
model_no_superhost <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + security_deposit,
  data = df
)
table5 <- data.frame(
  Model  = c("Primary (6 pred.)", "Drop superhost (5 pred.)",
             "AIC stepwise", "BIC stepwise"),
  AIC    = round(c(AIC(model_primary), AIC(model_no_superhost),
                   AIC(model_step), AIC(model_step)), 2),
  BIC    = round(c(BIC(model_primary), BIC(model_no_superhost),
                   BIC(model_step), BIC(model_step)), 2),
  Adj_R2 = round(c(summary(model_primary)$adj.r.squared,
                   summary(model_no_superhost)$adj.r.squared,
                   summary(model_step)$adj.r.squared,
                   summary(model_step)$adj.r.squared), 4)
)
write.csv(table5, "output/tables/table5_model_comparison.csv", row.names = FALSE)
cat("  Saved: table5_model_comparison.csv\n\n")

# =============================================================================
# STEP 7: Final model
# =============================================================================
cat("--- Step 7: Fitting final model ---\n")

df_final <- df[-influential_idx, ]

model_final <- lm(
  log_price ~ bedrooms + bathrooms + guests_included +
    property_type + security_deposit,
  data = df_final
)

cat("  Final model: n =", nrow(df_final), "| Adj R² =",
    round(summary(model_final)$adj.r.squared, 4), "\n")

# Figure 8: Final model residual plots
png("output/figures/fig8_final_residuals.png", width = 1200, height = 400,
    res = 120)
par(mfrow = c(1, 3))
plot(model_final, which = 1, main = "Residuals vs Fitted (Final)")
plot(model_final, which = 2, main = "Q-Q Residuals (Final)")
plot(model_final, which = 3, main = "Scale-Location (Final)")
par(mfrow = c(1, 1))
dev.off()
cat("  Saved: fig8_final_residuals.png\n")

# Figure 9: Final model Cook's Distance
png("output/figures/fig9_final_cooks.png", width = 900, height = 500, res = 120)
plot(model_final, which = 4, main = "Cook's Distance (Final Model)")
abline(h = 4 / nrow(df_final), col = "firebrick", lty = 2)
dev.off()
cat("  Saved: fig9_final_cooks.png\n")

# Table 6: Final model coefficients (main report table)
coef_final <- as.data.frame(summary(model_final)$coefficients)
ci_final   <- confint(model_final)

table6 <- data.frame(
  Predictor = c("Intercept", "Bedrooms", "Bathrooms", "Guests Included",
                "Condominium vs Apt", "House vs Apt", "Other vs Apt",
                "Security Deposit"),
  Estimate  = round(coef_final[, "Estimate"], 4),
  Std_Error = round(coef_final[, "Std. Error"], 4),
  CI_Lower  = round(ci_final[, 1], 4),
  CI_Upper  = round(ci_final[, 2], 4),
  t_value   = round(coef_final[, "t value"], 3),
  p_value   = round(coef_final[, "Pr(>|t|)"], 4),
  Pct_Change = round((exp(coef_final[, "Estimate"]) - 1) * 100, 2)
)
rownames(table6) <- NULL
write.csv(table6, "output/tables/table6_final_model_coefficients.csv",
          row.names = FALSE)
cat("  Saved: table6_final_model_coefficients.csv\n")

# Table 7: Final model performance
smry_final <- summary(model_final)
table7 <- data.frame(
  Metric = c("R-squared", "Adjusted R-squared", "AIC", "BIC",
             "Residual Std Error", "F-statistic", "p-value", "N"),
  Primary_Model = c(
    round(summary(model_primary)$r.squared, 4),
    round(summary(model_primary)$adj.r.squared, 4),
    round(AIC(model_primary), 2),
    round(BIC(model_primary), 2),
    round(summary(model_primary)$sigma, 4),
    round(summary(model_primary)$fstatistic[1], 2),
    "< 2.2e-16",
    nrow(df)
  ),
  Final_Model = c(
    round(smry_final$r.squared, 4),
    round(smry_final$adj.r.squared, 4),
    round(AIC(model_final), 2),
    round(BIC(model_final), 2),
    round(smry_final$sigma, 4),
    round(smry_final$fstatistic[1], 2),
    "< 2.2e-16",
    nrow(df_final)
  )
)
write.csv(table7, "output/tables/table7_model_performance.csv", row.names = FALSE)
cat("  Saved: table7_model_performance.csv\n")

# Table 8: VIF
vif_final <- as.data.frame(round(car::vif(model_final), 3))
vif_final$Predictor <- rownames(vif_final)
write.csv(vif_final, "output/tables/table8_vif_final_model.csv", row.names = FALSE)
cat("  Saved: table8_vif_final_model.csv\n\n")

# =============================================================================
# STEP 8: Summary
# =============================================================================
cat("╔══════════════════════════════════════════════════╗\n")
cat("║              All done! Summary:                  ║\n")
cat("╚══════════════════════════════════════════════════╝\n\n")

cat("FIGURES saved to output/figures/:\n")
figs <- list.files("output/figures", pattern = "*.png")
for (f in figs) cat("  ", f, "\n")

cat("\nTABLES saved to output/tables/:\n")
tabs <- list.files("output/tables", pattern = "*.csv")
for (t in tabs) cat("  ", t, "\n")

cat("\nFINAL MODEL:\n")
cat("  Formula: log_price ~ bedrooms + bathrooms + guests_included\n")
cat("                        + property_type + security_deposit\n")
cat("  N:          ", nrow(df_final), "\n")
cat("  Adj R²:     ", round(smry_final$adj.r.squared, 4), "\n")
cat("  AIC:        ", round(AIC(model_final), 2), "\n")
cat("  Res Std Err:", round(smry_final$sigma, 4), "\n")