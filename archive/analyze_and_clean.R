library(tidyverse)

INPUT_FILE <- "Gulmi1_voters_final.csv"
CLEAN_FILE <- "Gulmi1_voters_clean_temp.csv" # Temporary clean file
RETRY_TARGETS_FILE <- "retry_targets_2.csv"

# Valid VDC names (from config)
VALID_VDCS <- c(
  "कालीगण्डकी गाउँपालिका",
  "गुल्मी दरबार गाउँपालिका",
  "चन्द्रकोट गाउँपालिका",
  "छत्रकोट गाउँपालिका",
  "रुरुक्षेत्र गाउँपालिका",
  "रेसुङ्गा नगरपालिका",
  "सत्यवती गाउँपालिका"
)

df <- read_csv(INPUT_FILE, show_col_types = FALSE)

cat("Initial Rows:", nrow(df), "\n")

# 1. Clean Garbage
# Remove rows where VDC is not in our known list or is NA
df_clean <- df %>%
  mutate(VDC_trim = trimws(VDC)) %>%
  filter(VDC_trim %in% VALID_VDCS) %>%
  select(-VDC_trim)

cat("Rows after removing garbage:", nrow(df_clean), "\n")
cat("Garbage rows removed:", nrow(df) - nrow(df_clean), "\n")

# 2. Analyze for Suspicious Counts
summary <- df_clean %>%
  group_by(VDC, Ward, RegCentre) %>%
  summarise(Count = n(), .groups = "drop")

# Criteria for Retry:
# 1. Count is exactly a multiple of 100 (100, 200, 300...). 
#    This implies we likely crashed/stopped exactly at a page boundary.
# 2. RegCentre is NA (shouldn't be any left, but good to check)
# 3. Very low counts? (Optional, maybe < 10? But some centres are small. Let's stick to multiples of 100 first as the strongest signal for 'missing pages')

retry_targets <- summary %>%
  filter(
    is.na(RegCentre) | 
    (Count > 0 & Count %% 100 == 0)
  )

cat("\n--- Retry Analysis ---\n")
cat("Found", nrow(retry_targets), "suspicious registration centres.\n")
if(nrow(retry_targets) > 0) {
  print(retry_targets, n = Inf)
}

# Export Retry List
if (nrow(retry_targets) > 0) {
  write_csv(retry_targets, RETRY_TARGETS_FILE)
  cat("\nRetry targets saved to:", RETRY_TARGETS_FILE, "\n")
} else {
  cat("\nNo obvious retry targets found based on 'multiple of 100' criteria.\n")
}

# Save the Cleaned Dataset (so we don't carry garbage forward)
write_csv(df_clean, CLEAN_FILE)
cat("Cleaned dataset saved to:", CLEAN_FILE, "\n")
