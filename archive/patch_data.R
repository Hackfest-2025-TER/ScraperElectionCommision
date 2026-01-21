library(tidyverse)

# Load Data
df <- read_csv("Gulmi1_FINAL.csv", show_col_types = FALSE)
colnames(df)[1] <- "SN"

cat("Original Count:", nrow(df), "\n")

# 1. Define Valid VDCs
valid_vdcs <- c(
  "कालीगण्डकी गाउँपालिका",
  "गुल्मी दरबार गाउँपालिका",
  "चन्द्रकोट गाउँपालिका",
  "छत्रकोट गाउँपालिका",
  "रुरुक्षेत्र गाउँपालिका",
  "रेसुङ्गा नगरपालिका",
  "सत्यवती गाउँपालिका"
)

# 2. Filter out Garbage Rows
# We remove rows where VDC is invalid or NA
df_clean <- df %>%
  filter(VDC %in% valid_vdcs & !is.na(VDC))

cat("Count after removing garbage:", nrow(df_clean), "\n")
cat("Removed:", nrow(df) - nrow(df_clean), "\n")

# 3. Construct Correct Rows
# Common columns
common_prov <- "लुम्बिनी प्रदेश"
common_dist <- "गुल्मी"

# Set A: KaliGandaki-5-Janakalyan
vdc_a <- "कालीगण्डकी गाउँपालिका"
ward_a <- 5
rc_a <- "जनकल्याण मा.वि., फोक्सिङ्ग"

rows_a <- tribble(
  ~SN, ~ID, ~Name, ~Age, ~Gender, ~Spouse, ~Parent,
  242, 16914976, "कृष्ण राना", 35, "महिला", "-", "प्रेम बहादुर/ नन्दकली",
  252, 18534959, "कृष्‍णा सार्की", 41, "महिला", "तारा बहादुर  सार्की", "राम बहादुर  सार्की/ सुमित्रा   सार्की",
  262, 19219106, "केशव  सुनार", 30, "पुरुष", "-", "लुद्रमान / पुकला",
  273, 9324188, "कौशिला सुनार", 62, "महिला", "चक्र बहादुर", "पुर्ण बहादुर/ समुन्द्रामति"
) %>%
  mutate(VDC = vdc_a, Ward = ward_a, RegCentre = rc_a)

# Set B: Chhatrakot-2-Janajyoti
vdc_b <- "छत्रकोट गाउँपालिका"
ward_b <- 2
rc_b <- "जनज्योती मा.वि., कोलधरी"

rows_b <- tribble(
  ~SN, ~ID, ~Name, ~Age, ~Gender, ~Spouse, ~Parent,
  1401, 18375432, "मंगली लोहार", 49, "महिला", "खिम बहादुर", "जगत राम/ सिता धरी",
  1423, 24670708, "मन कुमारी बलाल", 21, "महिला", "- -", "धन बहादुर/ देवी कुमारी"
) %>%
  mutate(VDC = vdc_b, Ward = ward_b, RegCentre = rc_b)

# Combine new rows
new_rows <- bind_rows(rows_a, rows_b) %>%
  mutate(
    Province = common_prov,
    District = common_dist,
    `मतदाता विवरण` = "मतदाता विवरण"
  ) %>%
  rename(
    `सि.नं.` = SN,
    `मतदाता नं` = ID,
    `मतदाताको नाम` = Name,
    `उमेर(वर्ष)` = Age,
    `लिङ्ग` = Gender,
    `पति/पत्नीको नाम` = Spouse,
    `पिता/माताको नाम` = Parent
  )

# 4. Bind and Sort
# Align columns exactly
final_df <- bind_rows(df_clean %>%
  rename(`सि.नं.` = SN), new_rows) %>%
  arrange(VDC, Ward, RegCentre, `सि.नं.`)

cat("Final Count:", nrow(final_df), "\n")

# 5. Write
write_csv(final_df, "Gulmi1_FINAL.csv")
cat("Successfully patched and saved to Gulmi1_FINAL.csv\n")
