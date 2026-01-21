library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)

# Constants
URL <- "https://voterlist.election.gov.np"
DRIVER_PORT <- 4550 # Use one of the existing ports

create_session <- function(port = DRIVER_PORT) {
    driver_url <- paste0("http://localhost:", port)
    session_body <- list(
        capabilities = list(
            alwaysMatch = list(
                browserName = "chrome",
                `goog:chromeOptions` = list(args = list("--start-maximized", "--disable-notifications"))
            )
        )
    )
    # Simple create
    resp <- POST(paste0(driver_url, "/session"), content_type_json(), body = toJSON(session_body, auto_unbox = TRUE))
    if (status_code(resp) != 200) stop("Session create failed")
    content(resp, "parsed")
}

wd_req <- function(sess, method, endpoint, body=NULL) {
    url <- paste0("http://localhost:", DRIVER_PORT, "/session/", sess$value$sessionId, endpoint)
    if(method=="GET") content(GET(url), "parsed")
    else content(POST(url, content_type_json(), body=toJSON(body, auto_unbox=TRUE)), "parsed")
}

wd_script <- function(sess, script) wd_req(sess, "POST", "/execute/sync", list(script=script, args=list()))

# Main Test
tryCatch({
    message("=== VERIFYING PAGINATION & PAGE SIZE ===")
    
    # 1. Connect (assuming driver is running from previous step, if not we fail fast)
    # We'll try to use port 4550. If busy, might fail.
    # Actually, let's kill/restart 4550 to be safe?
    # No, user script might be running. I'll try port 4555 (new one) or just 4550 assuming previous script finished.
    # The previous script `voter_scraping_parallel.R` runs in parallel on 4550-4554.
    # If it's still running, I shouldn't interrupt.
    # But this is a "check".
    
    # I'll try to create a session on 4550.
    sess <- create_session(4550)
    message("Session created on 4550")
    
    # Navigate
    wd_req(sess, "POST", "/url", list(url=URL))
    Sys.sleep(4)
    
    # Setup Gulmi Ward 1 with ROBUST delays
    # State
    wd_script(sess, "document.querySelector('#state').value='5'; document.querySelector('#state').dispatchEvent(new Event('change'));")
    Sys.sleep(10)
    
    # District
    wd_script(sess, "document.querySelector('#district').value='42'; document.querySelector('#district').dispatchEvent(new Event('change'));")
    Sys.sleep(10)
    
    # VDC
    wd_script(sess, "document.querySelector('#vdc_mun').value='5434'; document.querySelector('#vdc_mun').dispatchEvent(new Event('change'));")
    Sys.sleep(15)

    # Ward selection
    wd_script(sess, "document.querySelector('#ward').value='1'; document.querySelector('#ward').dispatchEvent(new Event('change'));")
    message("Selected Ward 1. Waiting 15s for Reg Centres...")
    Sys.sleep(15)
    
    # Select first reg centre with retry
    rc_val <- NULL
    for(i in 1:5) {
        rc_script <- "var sel = document.querySelector('#reg_centre'); return sel.options.length > 1 ? sel.options[1].value : null;"
        rc_val <- wd_script(sess, rc_script)$value
        if(!is.null(rc_val)) break
        message("  Retry ", i, ": Reg centre not ready yet...")
        Sys.sleep(5)
    }
    
    if (is.null(rc_val)) {
        html <- wd_req(sess, "GET", "/source")$value
        rc_html <- read_html(html) %>% html_node("#reg_centre") %>% as.character()
        message("DEBUG: Reg Centre HTML:\n", rc_html)
        stop("No reg centre found")
    }
    
    if (is.null(rc_val)) stop("No reg centre found")
    message("Selected RegCentre Value: ", rc_val)
    
    wd_script(sess, paste0("document.querySelector('#reg_centre').value='", rc_val, "'; document.querySelector('#reg_centre').dispatchEvent(new Event('change'));"))
    Sys.sleep(1)
    wd_script(sess, "document.querySelector('#btnSubmit').click();")
    message("Clicked Submit. Waiting 5s...")
    Sys.sleep(5)
    
    # CHECK 1: Default Page Size (should be 10)
    html <- wd_req(sess, "GET", "/source")$value
    rows <- read_html(html) %>% html_node("table.bbvrs_data") %>% html_table(fill=TRUE)
    message("Default Row Count: ", nrow(rows))
    
    if (nrow(rows) != 10) message("WARNING: Default rows is ", nrow(rows), " (Expected 10)")
    
    # CHECK 2: Change to 100
    message("Changing page size to 100...")
    wd_script(sess, "document.querySelector('select[name=\"tbl_data_length\"]').value='100';")
    wd_script(sess, "document.querySelector('select[name=\"tbl_data_length\"]').dispatchEvent(new Event('change'));")
    Sys.sleep(5) # Wait for reload
    
    html2 <- wd_req(sess, "GET", "/source")$value
    rows2 <- read_html(html2) %>% html_node("table.bbvrs_data") %>% html_table(fill=TRUE)
    message("New Row Count: ", nrow(rows2))
    
    if (nrow(rows2) <= 10) {
        message("FAILURE: Page size change did not increase row count!")
    } else {
        message("SUCCESS: Page size change worked. Rows: ", nrow(rows2))
    }
    
    # CHECK 3: Pagination Next
    first_person <- rows2[1, 2] # Assuming name is col 2
    message("Row 1 Person: ", first_person)
    
    message("Clicking Next...")
    wd_script(sess, "document.querySelector('#tbl_data_next').click();")
    Sys.sleep(5)
    
    html3 <- wd_req(sess, "GET", "/source")$value
    rows3 <- read_html(html3) %>% html_node("table.bbvrs_data") %>% html_table(fill=TRUE)
    
    new_first_person <- rows3[1, 2]
    message("New Row 1 Person: ", new_first_person)
    
    if (first_person == new_first_person) {
        message("FAILURE: Data did not change after clicking Next!")
    } else {
        message("SUCCESS: Pagination worked. Data changed.")
    }

}, error = function(e) {
    message("ERROR: ", e$message)
}, finally = {
    if (exists("sess")) wd_req(sess, "DELETE", "")
})
