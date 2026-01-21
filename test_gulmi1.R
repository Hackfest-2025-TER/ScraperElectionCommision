library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)

# ============================================
# TESTED CONFIGURATION FOR GULMI 1
# All values verified via browser on 2026-01-20
# ============================================

URL <- "https://voterlist.election.gov.np"

# Selectors (verified)
SELECTORS <- list(
    state = "#state",
    district = "#district",
    vdc = "#vdc_mun",
    ward = "#ward",
    reg_centre = "#reg_centre",
    submit = "#btnSubmit",
    table = ".bbvrs_data",
    page_size = "select[name=\"tbl_data_length\"]",
    next_btn = "#tbl_data_next"
)

# Gulmi 1 Constituency Values (verified)
STATE_VAL <- "5" # लुम्बिनी प्रदेश
DISTRICT_VAL <- "42" # गुल्मी

# All VDCs in Gulmi 1 (verified from dropdown)
GULMI1_VDCS <- list(
    list(val = "5434", name = "कालीगण्डकी गाउँपालिका", wards = 1:7),
    list(val = "5435", name = "गुल्मी दरबार गाउँपालिका", wards = 1:7),
    list(val = "5436", name = "चन्द्रकोट गाउँपालिका", wards = 1:8),
    list(val = "5437", name = "छत्रकोट गाउँपालिका", wards = 1:6),
    list(val = "5442", name = "रुरुक्षेत्र गाउँपालिका", wards = 1:6),
    list(val = "5443", name = "रेसुङ्गा नगरपालिका", wards = 7:14),
    list(val = "5444", name = "सत्यवती गाउँपालिका", wards = 1:8)
)

# Delays in seconds (7s for all dropdown selections)
DELAYS <- list(
    page_load = 3,
    state_to_district = 10,
    district_to_vdc = 10,
    vdc_to_ward = 15,
    ward_to_regcentre = 15,
    after_submit = 15,
    after_page_change = 3
)

DRIVER_PORT <- 4550L

# ============================================
# WebDriver Helper Functions
# ============================================

create_session <- function(port = DRIVER_PORT) {
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

# Select dropdown and wait for next to populate
select_dropdown <- function(session, selector, value, wait_seconds) {
    script <- paste0(
        "document.querySelector('", selector, "').value = '", value, "';",
        "document.querySelector('", selector, "').dispatchEvent(new Event('change'));"
    )
    wd_script(session, script)
    message("    Selected ", selector, " = ", value, ", waiting ", wait_seconds, "s...")
    Sys.sleep(wait_seconds)
}

# Get dropdown options
get_options <- function(session, selector, debug = FALSE) {
    html <- wd_script(session, paste0("return document.querySelector('", selector, "').outerHTML;"))$value

    if (debug) {
        message("  DEBUG: Raw HTML for ", selector)
        message("  ", substr(as.character(html), 1, 500))
    }

    if (is.null(html) || is.na(html)) {
        return(data.frame(value = character(0), name = character(0), stringsAsFactors = FALSE))
    }

    opts <- read_html(html) %>% html_nodes("option")
    data.frame(
        value = opts %>% html_attr("value"),
        name = opts %>% html_text(),
        stringsAsFactors = FALSE
    ) %>% filter(value != "")
}

# ============================================
# Test: Single Ward Scrape
# ============================================

test_single_ward <- function() {
    message("\n=== TEST: Single Ward Scraping ===")
    message("VDC: कालीगण्डकी गाउँपालिका (5434)")
    message("Ward: 1\n")

    session <- create_session()

    tryCatch({
        # 1. Navigate
        message("Step 1: Navigating to ", URL)
        wd_navigate(session, URL)
        Sys.sleep(DELAYS$page_load)
        message("  ✓ Page loaded")

        # 2. Select State
        message("Step 2: Selecting State (5 - Lumbini Pradesh)")
        select_dropdown(session, SELECTORS$state, STATE_VAL, DELAYS$state_to_district)
        message("  ✓ State selected")

        # 3. Select District
        message("Step 3: Selecting District (42 - Gulmi)")
        select_dropdown(session, SELECTORS$district, DISTRICT_VAL, DELAYS$district_to_vdc)
        message("  ✓ District selected")

        # 4. Select VDC
        message("Step 4: Selecting VDC (5434 - Kaligandaki)")
        select_dropdown(session, SELECTORS$vdc, "5434", DELAYS$vdc_to_ward)
        message("  ✓ VDC selected")

        # 5. Select Ward
        message("Step 5: Selecting Ward (1)")
        select_dropdown(session, SELECTORS$ward, "1", DELAYS$ward_to_regcentre)
        message("  ✓ Ward selected")

        # 6. Get Registration Centres (with retry)
        message("Step 6: Getting registration centres...")
        reg_centres <- get_options(session, SELECTORS$reg_centre, debug = TRUE)

        # Retry if empty - may need more time
        if (nrow(reg_centres) == 0) {
            message("  No centres yet, waiting 5 more seconds...")
            Sys.sleep(5)
            reg_centres <- get_options(session, SELECTORS$reg_centre, debug = TRUE)
        }

        message("  Found ", nrow(reg_centres), " registration centre(s):")
        for (i in 1:nrow(reg_centres)) {
            message("    - ", reg_centres$name[i], " (", reg_centres$value[i], ")")
        }

        if (nrow(reg_centres) == 0) {
            stop("No registration centres found!")
        }

        # 7. Select first reg centre
        rc_val <- reg_centres$value[1]
        rc_name <- reg_centres$name[1]
        message("Step 7: Selecting registration centre: ", rc_name)
        select_dropdown(session, SELECTORS$reg_centre, rc_val, 1)
        message("  ✓ Registration centre selected")

        # 8. Submit
        message("Step 8: Clicking submit...")
        wd_script(session, "document.querySelector('#btnSubmit').click();")
        Sys.sleep(DELAYS$after_submit)
        message("  ✓ Submit clicked")

        # 9. Check for table
        message("Step 9: Checking for results table...")
        page_src <- wd_source(session)
        page_html <- read_html(page_src)
        table <- page_html %>% html_node("table.bbvrs_data")

        if (is.na(table)) {
            message("  ✗ Table NOT found!")
            message("  Page URL: ", wd_script(session, "return window.location.href;")$value)
            return(NULL)
        }

        message("  ✓ Table found!")

        # 10. Extract data
        message("Step 10: Extracting data...")
        df <- table %>% html_table(fill = TRUE)
        message("  ✓ Extracted ", nrow(df), " rows")

        # Print first few rows
        message("\nSample data (first 3 rows):")
        print(head(df, 3))

        return(df)
    }, error = function(e) {
        message("ERROR: ", e$message)
        return(NULL)
    }, finally = {
        message("\nClosing browser...")
        wd_quit(session)
    })
}

# Enhanced test: Multi reg_centre and multi ward (selector-only, no reload)
test_multi_regcentre_multi_ward <- function() {
    message("\n=== TEST: Multi Reg Centre, Multi Ward, No Reload ===")
    session <- create_session()
    results <- list()
    errors <- list()

    tryCatch({
        # Initial navigation and setup
        wd_navigate(session, URL)
        Sys.sleep(DELAYS$page_load)
        select_dropdown(session, SELECTORS$state, STATE_VAL, DELAYS$state_to_district)
        select_dropdown(session, SELECTORS$district, DISTRICT_VAL, DELAYS$district_to_vdc)
        select_dropdown(session, SELECTORS$vdc, "5434", DELAYS$vdc_to_ward)
        
        for (ward_num in 1:2) {  # Try ward 1 and 2 for speed/test
            message("\n*** Switching to Ward ", ward_num, " ***")
            select_dropdown(session, SELECTORS$ward, as.character(ward_num), DELAYS$ward_to_regcentre)
            reg_centres <- get_options(session, SELECTORS$reg_centre, debug = TRUE)

            # Retry if empty
            if (nrow(reg_centres) == 0) {
                message("  No centres yet, waiting extra 5 seconds...")
                Sys.sleep(5)
                reg_centres <- get_options(session, SELECTORS$reg_centre, debug = TRUE)
            }
            message("  Found ", nrow(reg_centres), " registration centre(s) for ward ", ward_num)
            for (i in seq_len(nrow(reg_centres))) {
                rc_val <- reg_centres$value[i]
                rc_name <- reg_centres$name[i]
                message("    > Reg Centre: ", rc_name, " (", rc_val, ") - Submitting...")
                select_dropdown(session, SELECTORS$reg_centre, rc_val, 1)
                wd_script(session, "document.querySelector('#btnSubmit').click();")
                Sys.sleep(DELAYS$after_submit)
                # Scrape table if present
                page_src <- wd_source(session)
                page_html <- read_html(page_src)
                table <- page_html %>% html_node("table.bbvrs_data")
                if (is.na(table)) {
                    err_msg <- paste("[Ward", ward_num, "]", "[RegCentre", rc_name, "] Table NOT found!")
                    message("  ✗ ", err_msg)
                    errors <- c(errors, err_msg)
                    next
                }
                df <- table %>% html_table(fill = TRUE)
                if (nrow(df) == 0) {
                    err_msg <- paste("[Ward", ward_num, "]", "[RegCentre", rc_name, "] Table EMPTY!")
                    message("  ✗ ", err_msg)
                    errors <- c(errors, err_msg)
                    next
                }
                # Add identifiers for tracking
                df$ward <- ward_num
                df$reg_centre <- rc_name
                results[[paste(ward_num, rc_val, sep = "_")]] <- df
                message("      ✓ Extracted ", nrow(df), " rows.")
            }
        }
    }, error = function(e) {
        message("FATAL ERROR: ", e$message)
    }, finally = {
        wd_quit(session)
    })

    # Combine
    all_df <- dplyr::bind_rows(results)
    list(data = all_df, errors = errors)
}

# Run the enhanced test
message("\n========================================")
message("  GULMI 1 VOTER SCRAPER - TEST SCRIPT: MULTI REG CENTRE & WARD")
message("========================================\n")
res <- test_multi_regcentre_multi_ward()

if (!is.null(res$data) && nrow(res$data) > 0) {
    message("\n✓ TEST PASSED! Scraped ", nrow(res$data), " rows from multi regcentres/wards.")
    write.csv(res$data, "test_output_multi.csv", row.names = FALSE, fileEncoding = "UTF-8")
    message("Saved to: test_output_multi.csv")
    if (length(res$errors) > 0) {
        message("Encountered ", length(res$errors), " minor errors:")
        for (e in res$errors) message("  - ", e)
    }
} else {
    message("\n✗ TEST FAILED - No data scraped from enhanced multi run")
    if (length(res$errors) > 0) {
        for (e in res$errors) message("  - ", e)
    }
}
