library(tidyverse)

df <- read_csv("Gulmi1_FINAL.csv", show_col_types = FALSE)
colnames(df)[1] <- "SN" # Safe rename for access

# Target IDs from your HTML snippet
target_ids <- c("16914976", "18534959", "19219106", "09324188", "18375432", "24670708")
target_sns <- c(242, 252, 262, 273, 1401, 1423)

cat("--- Inspecting Target IDs ---
")
# Check if these IDs exist in the file (even in corrupted columns)
matches <- df %>%
  filter(`मतदाता नं` %in% target_ids | SN %in% target_sns)

print(matches)

cat("\n--- Inspecting Garbage Rows (Invalid VDCs) ---
")
valid_vdcs <- c("कालीगण्डकी गाउँपालिका", "गुल्मी दरबार गाउँपालिका", "चन्द्रकोट गाउँपालिका", "छत्रकोट गाउँपालिका", "रुरुक्षेत्र गाउँपालिका", "रेसुङ्गा नगरपालिका", "सत्यवती गाउँपालिका")

garbage <- df %>%
  filter(!VDC %in% valid_vdcs | is.na(VDC))

print(garbage)

