library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

# ============================================ 
# CONFIGURATION
# ============================================ 

URL <- "https://voterlist.election.gov.np"
STATE_VAL <- "5"
STATE_NAME <- "लुम्बिनी प्रदेश"
DISTRICT_VAL <- "42"
DISTRICT_NAME <- "गुल्मी"

# Verified VDC list from test_gulmi1.R
GULMI1_VDCS <- list(
    list(val = "5434", name = "कालीगण्डकी गाउँपालिका", wards = 1:7),
    list(val = "5435", name = "गुल्मी दरबार गाउँपालिका", wards = 1:7),
    list(val = "5436", name = "चन्द्रकोट गाउँपालिका", wards = 1:8),
    list(val = "5437", name = "छत्रकोट गाउँपालिका", wards = 1:6),
    list(val = "5442", name = "रुरुक्षेत्र गाउँपालिका", wards = 1:6),
    list(val = "5443", name = "रेसुङ्गा नगरपालिका", wards = 7:14),
    list(val = "5444", name = "सत्यवती गाउँपालिका", wards = 1:8)
)

OUTPUT_FILE <- "Gulmi1_voters_merged.csv"
CHECKPOINT_FILE <- "checkpoint_status.csv"
ERROR_LOG <- "error_log.csv"

# Driver Config
DRIVER_BASE_PORT <- 4550L
NUM_WORKERS <- 5
PAGE_SIZE <- 100

# Robustness Config
MAX_DROPDOWN_TRIES <- 15    # Increased from 8
MAX_TABLE_TRIES <- 10       # Increased from 8
BASE_WAIT <- 8              # Increased to 8s to match test script
SESSION_RESTART_EVERY <- 20 # Restart driver every 20 reg centres

# ============================================ 
# HELPER FUNCTIONS
# ============================================ 

create_session <- function(port) {
    driver_url <- paste0("http://localhost:", port)
    session_body <- list(
        capabilities = list(
            alwaysMatch = list(
                browserName = "chrome",
                `goog:chromeOptions` = list(args = list("--start-maximized", "--disable-notifications"))
            )
        )
    )
    # Try creating session with retry
    for(i in 1:3) {
        tryCatch({
            session_resp <- POST(paste0(driver_url, "/session"), content_type_json(), body = toJSON(session_body, auto_unbox = TRUE), timeout(10))
            if (status_code(session_resp) == 200) {
                session_data <- content(session_resp, "parsed")
                return(list(port = port, url = driver_url, session_id = session_data$value$sessionId))
            }
        }, error = function(e) { message("  Error creating session on port ", port, ": ", e$message) })
        Sys.sleep(2)
    }
    stop("Failed to create session on port ", port)
}

wd_request <- function(session, method, endpoint, body = NULL) {
    url <- paste0(session$url, "/session/", session$session_id, endpoint)
    resp <- switch(method, 
        GET = GET(url), 
        POST = POST(url, content_type_json(), body = toJSON(body, auto_unbox = TRUE))
    )
    content(resp, "parsed")
}

wd_navigate <- function(session, url) wd_request(session, "POST", "/url", list(url = url))
wd_script <- function(session, script) wd_request(session, "POST", "/execute/sync", list(script = script, args = list()))
wd_source <- function(session) wd_request(session, "GET", "/source")$value
wd_quit <- function(session) DELETE(paste0(session$url, "/session/", session$session_id))

# Wait for a dropdown to have options > 1
wait_for_populated <- function(session, selector, timeout=30) {
    start_time <- Sys.time()
    while (as.numeric(Sys.time() - start_time) < timeout) {
        script <- paste0("return document.querySelector('", selector, "').options.length;")
        val <- tryCatch(wd_script(session, script)$value, error=function(e) NULL)
        
        if (!is.null(val) && as.integer(val) > 1) {
            return(TRUE)
        }
        Sys.sleep(1)
    }
    return(FALSE)
}

retry_select_dropdown <- function(session, selector, value, base_wait=BASE_WAIT, tries=MAX_DROPDOWN_TRIES, whatdesc="option") {
    # First, try to wait for the value to be present in options if it's not empty
    if (value != "") {
         for(k in 1:10) {
             html <- wd_script(session, paste0("return document.querySelector('", selector, "').outerHTML;"))$value
             opts <- suppressWarnings(tryCatch(read_html(html) %>% html_nodes("option"), error=function(e) NULL))
             vals <- if (!is.null(opts)) html_attr(opts, "value") else character(0)
             if (any(vals == value)) break
             Sys.sleep(1)
         }
    }

    for (i in 1:tries) {
        # Set value and dispatch event
        wd_script(session, paste0("document.querySelector('", selector, "').value='", value, "';"))
        wd_script(session, paste0("document.querySelector('", selector, "').dispatchEvent(new Event('change'));"))
        
        # Wait
        Sys.sleep(base_wait + (i * 0.5)) # Progressive backoff
        
        # Verify
        html <- wd_script(session, paste0("return document.querySelector('", selector, "').outerHTML;"))$value
        opts <- suppressWarnings(tryCatch(read_html(html) %>% html_nodes("option"), error=function(e) NULL))
        
        vals <- if (!is.null(opts)) html_attr(opts, "value") else character(0)
        
        # If we selected something specific, check if it's there. 
        # If we selected "" (to populate), check if we have more than default option
        found <- FALSE
        if (value == "" && length(vals) > 1) {
             found <- TRUE
             namez <- html_text(opts)
             return(list(value=vals, name=namez))
        } else if (value != "" && any(vals == value)) {
             return(list(value=vals))
        }
        
        message("  [", whatdesc, "] Try ", i, "/", tries, " waiting for update...")
    }
    message("  [", whatdesc, "] FAILED. Value '", value, "' not found or dropdown not populated.")
    return(NULL)
}

retry_extract_table <- function(session, tries=MAX_TABLE_TRIES, base_wait=BASE_WAIT) {
    for (a in 1:tries) {
        page_src <- wd_source(session)
        page_html <- read_html(page_src)
        tbl <- page_html %>% html_node("table.bbvrs_data")
        if (!is.na(tbl)) {
            dfx <- tbl %>% html_table(fill=TRUE)
            if (nrow(dfx) > 0) return(list(df = dfx, html=page_html))
        }
        message("  [Table] RETRY ", a, "/", tries, " ...")
        Sys.sleep(base_wait + a)
    }
    return(NULL)
}

log_checkpoint <- function(vdc, ward, reg_centre, status, nrows) {
    df = data.frame(
        timestamp=Sys.time(), vdc=vdc, ward=ward, reg_centre=reg_centre,
        status=status, nrows=nrows, stringsAsFactors=FALSE)
    line <- paste(df, collapse=",")
    # Simple append to avoid file lock contention issues in simple CSV (flock is better but R is tricky on Win) 
    # utilizing catch/retry if needed, but cat is usually atomic enough for small lines
    cat(paste0(paste(df, collapse=","), "\n"), file=CHECKPOINT_FILE, append=TRUE)
}

log_error <- function(vdc, ward, reg_centre, phase, msg){
    df = data.frame(time=Sys.time(), vdc=vdc, ward=ward, reg_centre=reg_centre, phase=phase, msg=msg, stringsAsFactors=FALSE)
    cat(paste0(paste(df, collapse=","), "\n"), file=ERROR_LOG, append=TRUE)
}

append_output <- function(dat){
    # Using write.table with append. 
    # Note: Column headers only on first write.
    if (!file.exists(OUTPUT_FILE)) {
        write.csv(dat, OUTPUT_FILE, row.names=FALSE, fileEncoding="UTF-8")
    } else {
        write.table(dat, OUTPUT_FILE, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE, fileEncoding="UTF-8")
    }
}

# ============================================ 
# CORE SCRAPER LOGIC
# ============================================ 

scrape_ward <- function(ward, vdc_val, vdc_name, port) {
    session <- NULL
    on.exit(if (!is.null(session)) wd_quit(session))
    
    msg_head <- function(...) paste0("[ ", vdc_name, "] [Ward ", ward, "] [Port ", port, "] ", ...)
    
    attempt <- 1
    done <- FALSE
    total_regcentres <- 0
    
    while(!done && attempt <= 3) { # Retry whole ward scrape up to 3 times
        tryCatch({
            session <- create_session(port)
            wd_navigate(session, URL); Sys.sleep(5)
            
            # 1. State
            retry_select_dropdown(session, "#state", STATE_VAL, BASE_WAIT, MAX_DROPDOWN_TRIES, "state")
            
            # Wait for District to populate
            if(!wait_for_populated(session, "#district")) stop("District dropdown failed to populate")
            
            # 2. District
            retry_select_dropdown(session, "#district", DISTRICT_VAL, BASE_WAIT, MAX_DROPDOWN_TRIES, "district")
            
            # Wait for VDC to populate
            if(!wait_for_populated(session, "#vdc_mun")) stop("VDC dropdown failed to populate")

            # 3. VDC
            retry_select_dropdown(session, "#vdc_mun", vdc_val, BASE_WAIT, MAX_DROPDOWN_TRIES, "vdc")
            
            # Wait for Ward to populate
            if(!wait_for_populated(session, "#ward")) stop("Ward dropdown failed to populate")

            # 4. Ward
            retry_select_dropdown(session, "#ward", ward, BASE_WAIT, MAX_DROPDOWN_TRIES, "ward")
            
            # Wait for Reg Centre to populate
            if(!wait_for_populated(session, "#reg_centre")) stop("RegCentre dropdown failed to populate")

            # 5. Get Registration Centres
            rc_opts <- retry_select_dropdown(session, "#reg_centre", "", BASE_WAIT, MAX_DROPDOWN_TRIES, "regcentre-pop")
            
            if (is.null(rc_opts) || length(rc_opts$value) == 0){
                log_checkpoint(vdc_name, ward, NA, "FAIL: no-centres", 0)
                log_error(vdc_name, ward, NA, "regcentre-list", "No regcentres found")
                break
            }
            
            # Filter empty values
            valid_indices <- which(rc_opts$value != "")
            reg_values <- rc_opts$value[valid_indices]
            reg_names <- rc_opts$name[valid_indices]
            
            message(msg_head(sprintf("Found %d registration centres.", length(reg_values))))
            
            for (i in seq_along(reg_values)){
                rc_val <- reg_values[i]
                rc_name <- reg_names[i]
                
                # Check if already done (optional optimization, skipped for simplicity)
                
                ok <- FALSE
                ntries <- 0
                while(!ok && ntries < 3) {
                    tryCatch({
                        message(msg_head(sprintf("Processing RC: %s (%s)", rc_name, rc_val)))
                        
                        retry_select_dropdown(session, "#reg_centre", rc_val, 2, 5, "regcentre-select")
                        
                        # Click Submit
                        wd_script(session, "document.querySelector('#btnSubmit').click();")
                        Sys.sleep(5)
                        
                        # Set Page Size to 100
                        wd_script(session, sprintf("document.querySelector('select[name=\"tbl_data_length\"]').value='%d';", PAGE_SIZE))
                        wd_script(session, "document.querySelector('select[name=\"tbl_data_length\"]').dispatchEvent(new Event('change'));")
                        Sys.sleep(3)
                        
                        paged <- TRUE
                        collected <- data.frame()
                        page_idx <- 1
                        max_page_tries <- 100 # Safety limit
                        
                        while(paged && page_idx <= max_page_tries){
                            tb <- retry_extract_table(session, MAX_TABLE_TRIES)
                            
                            if (!is.null(tb) && nrow(tb$df) > 0) {
                                thispage <- tb$df
                                # Enrich data
                                thispage$Province <- STATE_NAME
                                thispage$District <- DISTRICT_NAME
                                thispage$VDC <- vdc_name
                                thispage$Ward <- ward
                                thispage$RegCentre <- rc_name
                                
                                append_output(thispage)
                                collected <- bind_rows(collected, thispage)
                                
                                message(msg_head(sprintf("  -> Pg %d: %d rows", page_idx, nrow(thispage))))
                            } else {
                                # If table is empty on page 1, maybe no data?
                                if (page_idx == 1) {
                                    message(msg_head("  -> No data found."))
                                }
                                paged <- FALSE
                                break
                            }
                            
                            # Check Next button
                            nxstat <- wd_script(session, "return document.querySelector('#tbl_data_next').className;")
                            if (is.null(nxstat) || grepl("disabled", nxstat$value)) {
                                paged <- FALSE
                            } else {
                                wd_script(session, "document.querySelector('#tbl_data_next').click();")
                                Sys.sleep(3)
                                page_idx <- page_idx + 1
                            }
                        }
                        
                        log_checkpoint(vdc_name, ward, rc_name, "SUCCESS", nrow(collected))
                        ok <- TRUE
                        total_regcentres <- total_regcentres + 1
                        
                    }, error=function(e){
                        ntries <<- ntries + 1
                        log_error(vdc_name, ward, rc_name, "regcentre-loop", e$message)
                        message(msg_head(sprintf("  ERROR: %s. Retry %d/3...", e$message, ntries)))
                        Sys.sleep(5)
                        
                        # If error, maybe refresh page
                        wd_navigate(session, URL); Sys.sleep(3)
                        retry_select_dropdown(session, "#state", STATE_VAL, BASE_WAIT, 3, "state-recover")
                        # Wait for District to populate
                        wait_for_populated(session, "#district")
                        retry_select_dropdown(session, "#district", DISTRICT_VAL, BASE_WAIT, 3, "district-recover")
                        # Wait for VDC to populate
                        wait_for_populated(session, "#vdc_mun")
                        retry_select_dropdown(session, "#vdc_mun", vdc_val, BASE_WAIT, 3, "vdc-recover")
                        # Wait for Ward to populate
                        wait_for_populated(session, "#ward")
                        retry_select_dropdown(session, "#ward", ward, BASE_WAIT, 3, "ward-recover")
                    })
                }
                
                # Session hygiene
                if (total_regcentres > 0 && total_regcentres %% SESSION_RESTART_EVERY == 0) {
                    message(msg_head("Restarting session for hygiene..."))
                    wd_quit(session)
                    Sys.sleep(2)
                    session <- create_session(port)
                    wd_navigate(session, URL); Sys.sleep(3)
                    # Re-navigate to current context
                    retry_select_dropdown(session, "#state", STATE_VAL, BASE_WAIT, 3)
                    wait_for_populated(session, "#district")
                    retry_select_dropdown(session, "#district", DISTRICT_VAL, BASE_WAIT, 3)
                    wait_for_populated(session, "#vdc_mun")
                    retry_select_dropdown(session, "#vdc_mun", vdc_val, BASE_WAIT, 3)
                    wait_for_populated(session, "#ward")
                    retry_select_dropdown(session, "#ward", ward, BASE_WAIT, 3)
                }
            }
            done <- TRUE
            
        }, error=function(e){
            attempt <<- attempt + 1
            log_error(vdc_name, ward, NA, "ward-attempt", e$message)
            message(msg_head(sprintf("CRITICAL WARD ERROR: %s. Retry %d/3...", e$message, attempt)))
            Sys.sleep(10)
        })
    }
    invisible(TRUE)
}

# ============================================ 
# MAIN EXECUTION
# ============================================ 

message("=== PARALLEL VOTER SCRAPER ===")
message(sprintf("Target: %d VDCs in Gulmi", length(GULMI1_VDCS)))

# Build Task List
tasks <- data.frame(
    vdc_val = character(),
    vdc_name = character(),
    ward = integer(),
    stringsAsFactors=FALSE
)

for(v in GULMI1_VDCS) {
    for(w in v$wards) {
        tasks <- rbind(tasks, data.frame(vdc_val=v$val, vdc_name=v$name, ward=w))
    }
}

message(sprintf("Total Tasks: %d ward-jobs", nrow(tasks)))
message(sprintf("Workers: %d", NUM_WORKERS))

# Pre-checks
if (!file.exists(CHECKPOINT_FILE)) {
    write.table(data.frame(timestamp=character(), vdc=character(), ward=integer(), reg_centre=character(), status=character(), nrows=integer()), 
                CHECKPOINT_FILE, sep=",", row.names=FALSE)
}

# Cluster Setup
message("Creating cluster with outfile='' for debug...")
cl <- makeCluster(NUM_WORKERS, outfile="")
registerDoParallel(cl)

# Distribute tasks
# We must use the "Worker ID" loop structure to guarantee unique ports.

# Split tasks into N chunks
task_indices <- 1:nrow(tasks)
chunks <- split(task_indices, cut(task_indices, NUM_WORKERS, labels=FALSE))

message("Starting parallel foreach...")
results <- foreach(
    worker_id = 1:NUM_WORKERS,
    .combine = list,
    .multicombine = TRUE,
    .export = c("scrape_ward", "create_session", "wd_request", "wd_navigate", "wd_script", "wd_source", "wd_quit", 
                "retry_select_dropdown", "retry_extract_table", "log_checkpoint", "log_error", "append_output", "wait_for_populated",
                "URL", "STATE_VAL", "STATE_NAME", "DISTRICT_VAL", "DISTRICT_NAME", "PAGE_SIZE", 
                "DRIVER_BASE_PORT", "MAX_DROPDOWN_TRIES", "MAX_TABLE_TRIES", "SESSION_RESTART_EVERY", 
                "OUTPUT_FILE", "CHECKPOINT_FILE", "ERROR_LOG", "BASE_WAIT", "tasks", "chunks"),
    .packages = c("httr","jsonlite","rvest","tidyverse")
) %dopar% {
    
    my_port <- DRIVER_BASE_PORT + (worker_id - 1)
    my_chunk <- chunks[[worker_id]]
    
    message(sprintf("Worker %d (Port %d) alive! Assigned %d tasks.", worker_id, my_port, length(my_chunk)))
    
    if (length(my_chunk) > 0) {
        for (idx in my_chunk) {
            t <- tasks[idx, ]
            message(sprintf("Worker %d starting: %s Ward %d", worker_id, t$vdc_name, t$ward))
            tryCatch({
                scrape_ward(t$ward, t$vdc_val, t$vdc_name, my_port)
            }, error = function(e) {
                msg <- paste("Worker", worker_id, "failed task:", e$message)
                message(msg)
                log_error(t$vdc_name, t$ward, "GLOBAL", "worker-fail", e$message)
            })
        }
    } else {
        message(sprintf("Worker %d has no tasks.", worker_id))
    }
    TRUE
}

stopCluster(cl)
message("All tasks completed.")