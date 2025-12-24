library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

# Constants
URL <- "https://voterlist.election.gov.np"
STATE_VAL <- "3"
STATE_NAME <- "बागमती प्रदेश"
DISTRICT_VAL <- "29"
DISTRICT_NAME <- "काभ्रेपलाञ्चोक"
VDC_VAL <- "5301"
VDC_NAME <- "धुलिखेल नगरपालिका"
WARDS <- 1:12
OUTPUT_FILE <- "dhulikhel_voter_list_full.csv"
DRIVER_BASE_PORT <- 4550L # Will use 4550, 4551, 4552 for 3 instances
NUM_WORKERS <- 3

# Helper function to create a WebDriver session
create_session <- function(port) {
    driver_url <- paste0("http://localhost:", port)
    session_body <- list(
        capabilities = list(
            alwaysMatch = list(
                browserName = "chrome",
                `goog:chromeOptions` = list(
                    args = list("--start-maximized", "--disable-notifications")
                )
            )
        )
    )

    session_resp <- POST(
        paste0(driver_url, "/session"),
        content_type_json(),
        body = toJSON(session_body, auto_unbox = TRUE)
    )

    if (status_code(session_resp) != 200) {
        stop("Failed to create session on port ", port)
    }

    session_data <- content(session_resp, "parsed")
    list(
        port = port,
        url = driver_url,
        session_id = session_data$value$sessionId
    )
}

# Helper functions for WebDriver
wd_request <- function(session, method, endpoint, body = NULL) {
    url <- paste0(session$url, "/session/", session$session_id, endpoint)
    if (method == "GET") {
        resp <- GET(url)
    } else if (method == "POST") {
        resp <- POST(url, content_type_json(), body = toJSON(body, auto_unbox = TRUE))
    }
    content(resp, "parsed")
}

wd_navigate <- function(session, url) {
    wd_request(session, "POST", "/url", list(url = url))
}

wd_script <- function(session, script) {
    wd_request(session, "POST", "/execute/sync", list(script = script, args = list()))
}

wd_source <- function(session) {
    wd_request(session, "GET", "/source")$value
}

wd_quit <- function(session) {
    DELETE(paste0(session$url, "/session/", session$session_id))
}

# Function to scrape a single ward with full pagination
scrape_ward <- function(ward, port) {
    message("[Worker ", port - DRIVER_BASE_PORT + 1, "] Processing Ward: ", ward)

    session <- create_session(port)
    ward_data <- data.frame()

    tryCatch({
        # Navigate and select dropdowns
        wd_navigate(session, URL)
        Sys.sleep(3)

        # Select State
        wd_script(session, paste0("document.querySelector('#state').value = '", STATE_VAL, "';"))
        wd_script(session, "document.querySelector('#state').dispatchEvent(new Event('change'));")
        Sys.sleep(2)

        # Select District
        wd_script(session, paste0("document.querySelector('#district').value = '", DISTRICT_VAL, "';"))
        wd_script(session, "document.querySelector('#district').dispatchEvent(new Event('change'));")
        Sys.sleep(2)

        # Select VDC
        wd_script(session, paste0("document.querySelector('#vdc_mun').value = '", VDC_VAL, "';"))
        wd_script(session, "document.querySelector('#vdc_mun').dispatchEvent(new Event('change'));")
        Sys.sleep(2)

        # Select Ward
        wd_script(session, paste0("document.querySelector('#ward').value = '", ward, "';"))
        wd_script(session, "document.querySelector('#ward').dispatchEvent(new Event('change'));")
        Sys.sleep(2)

        # Get Registration Centres
        reg_html <- wd_script(session, "return document.querySelector('#reg_centre').outerHTML;")$value
        reg_opts <- read_html(reg_html) %>% html_nodes("option")
        reg_values <- reg_opts %>% html_attr("value")
        reg_names <- reg_opts %>% html_text()

        reg_centers <- data.frame(value = reg_values, name = reg_names, stringsAsFactors = FALSE) %>%
            filter(value != "")

        if (nrow(reg_centers) == 0) {
            message("[Worker ", port - DRIVER_BASE_PORT + 1, "] No registration centres for Ward ", ward)
            wd_quit(session)
            return(ward_data)
        }

        for (i in 1:nrow(reg_centers)) {
            rc_val <- reg_centers$value[i]
            rc_name <- reg_centers$name[i]
            message("[Worker ", port - DRIVER_BASE_PORT + 1, "]   -> Reg Centre: ", rc_name)

            # Select Reg Centre
            wd_script(session, paste0("document.querySelector('#reg_centre').value = '", rc_val, "';"))
            wd_script(session, "document.querySelector('#reg_centre').dispatchEvent(new Event('change'));")
            Sys.sleep(1)

            # Click Submit
            wd_script(session, "document.querySelector('#btnSubmit').click();")
            Sys.sleep(5)

            # Change page size to 100 entries
            wd_script(session, "document.querySelector('select[name=\"tbl_data_length\"]').value = '100';")
            wd_script(session, "document.querySelector('select[name=\"tbl_data_length\"]').dispatchEvent(new Event('change'));")
            Sys.sleep(3)

            # Pagination loop
            page_num <- 1
            while (TRUE) {
                message("[Worker ", port - DRIVER_BASE_PORT + 1, "]       Page ", page_num)

                # Extract table
                page_src <- wd_source(session)
                page_html <- read_html(page_src)

                target_table <- page_html %>% html_node("table.bbvrs_data")

                if (!is.na(target_table)) {
                    df <- target_table %>% html_table(fill = TRUE)

                    if (nrow(df) > 0) {
                        # Add metadata columns
                        df$Province <- STATE_NAME
                        df$District <- DISTRICT_NAME
                        df$VDC <- VDC_NAME
                        df$Ward <- ward
                        df$RegCentre <- rc_name

                        ward_data <- bind_rows(ward_data, df)
                        message("[Worker ", port - DRIVER_BASE_PORT + 1, "]       Extracted ", nrow(df), " rows from page ", page_num)
                    }
                }

                # Check if "Next" button is enabled
                next_btn_class <- wd_script(session, "return document.querySelector('#tbl_data_next').className;")$value

                if (grepl("disabled", next_btn_class)) {
                    message("[Worker ", port - DRIVER_BASE_PORT + 1, "]       No more pages.")
                    break
                }

                # Click Next
                wd_script(session, "document.querySelector('#tbl_data_next').click();")
                Sys.sleep(2)
                page_num <- page_num + 1

                # Safety limit
                if (page_num > 100) {
                    message("[Worker ", port - DRIVER_BASE_PORT + 1, "]       Safety limit reached (100 pages)")
                    break
                }
            }

            # Navigate back to form for next reg centre
            wd_navigate(session, URL)
            Sys.sleep(2)

            # Re-select dropdowns
            wd_script(session, paste0("document.querySelector('#state').value = '", STATE_VAL, "';"))
            wd_script(session, "document.querySelector('#state').dispatchEvent(new Event('change'));")
            Sys.sleep(1)

            wd_script(session, paste0("document.querySelector('#district').value = '", DISTRICT_VAL, "';"))
            wd_script(session, "document.querySelector('#district').dispatchEvent(new Event('change'));")
            Sys.sleep(1)

            wd_script(session, paste0("document.querySelector('#vdc_mun').value = '", VDC_VAL, "';"))
            wd_script(session, "document.querySelector('#vdc_mun').dispatchEvent(new Event('change'));")
            Sys.sleep(1)

            wd_script(session, paste0("document.querySelector('#ward').value = '", ward, "';"))
            wd_script(session, "document.querySelector('#ward').dispatchEvent(new Event('change'));")
            Sys.sleep(1)
        }
    }, error = function(e) {
        message("[Worker ", port - DRIVER_BASE_PORT + 1, "] Error in Ward ", ward, ": ", e$message)
    }, finally = {
        wd_quit(session)
    })

    return(ward_data)
}

# Main execution
message("=== Starting Dhulikhel Voter List Scraper ===")
message("Province: ", STATE_NAME)
message("District: ", DISTRICT_NAME)
message("VDC: ", VDC_NAME)
message("Wards: ", paste(WARDS, collapse = ", "))
message("Workers: ", NUM_WORKERS)
message("")

# Setup parallel backend
cl <- makeCluster(NUM_WORKERS)
registerDoParallel(cl)

# Assign wards to workers
# Each worker gets a subset of wards to process
ward_assignments <- split(WARDS, rep(1:NUM_WORKERS, length.out = length(WARDS)))

message("Ward assignments:")
for (i in seq_along(ward_assignments)) {
    message("  Worker ", i, ": Wards ", paste(ward_assignments[[i]], collapse = ", "))
}
message("")

# Run in parallel - each worker processes its assigned wards sequentially
all_results <- foreach(
    worker_id = 1:NUM_WORKERS,
    .combine = bind_rows,
    .packages = c("httr", "jsonlite", "rvest", "tidyverse"),
    .export = c(
        "create_session", "wd_request", "wd_navigate", "wd_script", "wd_source", "wd_quit",
        "scrape_ward", "URL", "STATE_VAL", "STATE_NAME", "DISTRICT_VAL", "DISTRICT_NAME",
        "VDC_VAL", "VDC_NAME", "DRIVER_BASE_PORT"
    )
) %dopar% {
    worker_port <- DRIVER_BASE_PORT + worker_id - 1
    worker_wards <- ward_assignments[[worker_id]]
    worker_data <- data.frame()

    for (ward in worker_wards) {
        ward_data <- scrape_ward(ward, worker_port)
        worker_data <- bind_rows(worker_data, ward_data)
    }

    worker_data
}

# Stop cluster
stopCluster(cl)

# Save to CSV
write.csv(all_results, OUTPUT_FILE, row.names = FALSE, fileEncoding = "UTF-8")
message("")
message("=== Scraping Complete ===")
message("Total records: ", nrow(all_results))
message("Saved to: ", OUTPUT_FILE)
