# =============================================================================
# 01_cleaning.R
# Purpose: Load raw data, clean variables, and save cleaned dataset
# Input:   data/raw/listings.csv
# Output:  data/cleaned/listings_cleaned.csv
# =============================================================================

library(dplyr)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Load raw data
# -----------------------------------------------------------------------------
df_raw <- read_csv("data/raw/listings.csv")

cat("Raw data dimensions:", nrow(df_raw), "rows x", ncol(df_raw), "cols\n")

# -----------------------------------------------------------------------------
# 2. Select only variables relevant to our model
# -----------------------------------------------------------------------------
df <- df_raw %>%
  select(
    # Response
    price,
    # Core predictors (proposal model)
    bedrooms,
    bathrooms,
    guests_included,
    property_type,
    host_is_superhost,
    security_deposit,
    # Additional variables available for model selection
    room_type,
    neighbourhood_cleansed,
    number_of_reviews,
    review_scores_rating,
    minimum_nights,
    cleaning_fee,
    accommodates
  )

# -----------------------------------------------------------------------------
# 3. Clean currency columns: strip "$" and "," then convert to numeric
# -----------------------------------------------------------------------------
clean_currency <- function(x) {
  as.numeric(str_replace_all(x, "[$,]", ""))
}

df <- df %>%
  mutate(
    price             = clean_currency(price),
    security_deposit  = clean_currency(security_deposit),
    cleaning_fee      = clean_currency(cleaning_fee)
  )

# -----------------------------------------------------------------------------
# 4. Convert logical-style columns
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(
    # read_csv() may parse "t"/"f" as TRUE/FALSE logical — handle both
    host_is_superhost = case_when(
      host_is_superhost == TRUE  | host_is_superhost == "t" ~ "t",
      host_is_superhost == FALSE | host_is_superhost == "f" ~ "f",
      TRUE ~ "f"
    )
  )

# -----------------------------------------------------------------------------
# 5. Collapse property_type into meaningful groups
#    (rare categories create unstable dummy variables)
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(
    property_type = case_when(
      property_type == "Apartment"   ~ "Apartment",
      property_type == "House"       ~ "House",
      property_type == "Condominium" ~ "Condominium",
      TRUE                           ~ "Other"       # Townhouse, Loft, B&B, etc.
    )
  )

cat("Property type distribution after collapsing:\n")
print(table(df$property_type))

# -----------------------------------------------------------------------------
# 6. Handle missing values
#    - security_deposit: missing likely means no deposit required → set to 0
#    - cleaning_fee: missing likely means no fee → set to 0
#    - Drop rows with missing price, bedrooms, bathrooms, property_type
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(
    security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
    cleaning_fee     = ifelse(is.na(cleaning_fee), 0, cleaning_fee)
  ) %>%
  filter(
    !is.na(price),
    !is.na(bedrooms),
    !is.na(bathrooms),
    !is.na(property_type),
    price > 0          # remove listings with $0 price (data errors)
  )

cat("After removing NAs:", nrow(df), "rows\n")

# -----------------------------------------------------------------------------
# 7. Create log-transformed response variable
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(log_price = log(price))

# -----------------------------------------------------------------------------
# 8. Final column selection for cleaned file
# -----------------------------------------------------------------------------
df_cleaned <- df %>%
  select(
    price, log_price,
    bedrooms, bathrooms, guests_included,
    property_type, host_is_superhost,
    security_deposit, cleaning_fee,
    room_type, neighbourhood_cleansed,
    number_of_reviews, review_scores_rating,
    minimum_nights, accommodates
  )

cat("Cleaned data dimensions:", nrow(df_cleaned), "rows x", ncol(df_cleaned), "cols\n")
summary(df_cleaned[, c("price", "log_price", "bedrooms", "bathrooms",
                       "guests_included", "security_deposit")])

# -----------------------------------------------------------------------------
# 9. Save cleaned dataset
# -----------------------------------------------------------------------------
write_csv(df_cleaned, "data/cleaned/listings_cleaned.csv")
cat("Saved: data/cleaned/listings_cleaned.csv\n")