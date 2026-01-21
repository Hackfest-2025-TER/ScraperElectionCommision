library(tidyverse)

# Function to summarize voter data
summarize_voter_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  summary <- df %>%
    group_by(VDC, Ward, RegCentre) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(VDC, Ward, RegCentre)
  
  return(summary)
}

# Default to Gulmi1_voters_merged.csv if no argument provided
args <- commandArgs(trailingOnly = TRUE)
target_file <- if (length(args) > 0) args[1] else "Gulmi1_voters_merged.csv"

cat("Summarizing file:", target_file, "\n")
summary_data <- summarize_voter_data(target_file)

# Export summary to CSV
output_csv <- paste0("summary_", basename(target_file))
write_csv(summary_data, output_csv)
cat("Summary saved to:", output_csv, "\n")

# Identify failed/suspicious entries
# Criteria: RegCentre is NA OR Count <= 100
retry_targets <- summary_data %>%
  filter(is.na(RegCentre) | Count <= 100)

if (nrow(retry_targets) > 0) {
  retry_file <- "retry_targets.csv"
  write_csv(retry_targets, retry_file)
  cat("Found", nrow(retry_targets), "targets to retry.\n")
  cat("Retry list saved to:", retry_file, "\n")
  print(retry_targets, n = 20)
} else {
  cat("No retry targets found based on criteria (NA or <= 100).\n")
}

# print(summary_data, n = Inf)

