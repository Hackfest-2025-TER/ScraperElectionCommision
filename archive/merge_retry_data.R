library(tidyverse)

# Files
ORIGINAL_FILE <- "Gulmi1_voters_final.csv"
RETRY_FILE <- "Gulmi1_voters_retry_2.csv"
FINAL_FILE <- "Gulmi1_voters_complete.csv"

# Function to read CSV safely
read_voters <- function(f) {
  if (!file.exists(f)) stop(paste("File not found:", f))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(
      # Standardize key columns for joining/filtering
      VDC_clean = trimws(VDC),
      Ward_clean = as.numeric(Ward),
      RegCentre_clean = trimws(gsub("\n", "", RegCentre))
    )
}

cat("Reading original data...\n")
df_orig <- read_voters(ORIGINAL_FILE)
cat("Original rows:", nrow(df_orig), "\n")

cat("Reading retry data...\n")
df_retry <- read_voters(RETRY_FILE)
cat("Retry rows:", nrow(df_retry), "\n")

# Identify the groups (VDC, Ward, RegCentre) present in the RETRY data
# We want to remove these groups from the ORIGINAL data before appending the new ones.
retry_keys <- df_retry %>%
  distinct(VDC_clean, Ward_clean, RegCentre_clean) %>%
  mutate(in_retry = TRUE)

# Join original with retry keys to find which rows to drop
df_orig_marked <- df_orig %>%
  left_join(retry_keys, by = c("VDC_clean", "Ward_clean", "RegCentre_clean"))

# Rows to keep from original: Those NOT in the retry set
df_keep <- df_orig_marked %>%
  filter(is.na(in_retry)) %>%
  select(-in_retry, -VDC_clean, -Ward_clean, -RegCentre_clean)

cat("Rows kept from original:", nrow(df_keep), "\n")
cat("Rows dropped from original:", nrow(df_orig) - nrow(df_keep), "\n")

# Prepare retry data for binding (remove helper cols)
df_retry_clean <- df_retry %>%
  select(-VDC_clean, -Ward_clean, -RegCentre_clean)

# Bind
df_final <- bind_rows(df_keep, df_retry_clean)
cat("Final row count:", nrow(df_final), "\n")

# Sort for tidiness
df_final <- df_final %>%
  arrange(VDC, Ward, RegCentre)

# Write
write_csv(df_final, FINAL_FILE)
cat("Successfully wrote merged data to:", FINAL_FILE, "\n")

# Verification
cat("--- Final Summary ---\n")
summary <- df_final %>%
  group_by(VDC, Ward, RegCentre) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Count)

print(summary, n = 20)
