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

# VDC Map (Name to Value)
VDC_MAP <- list(
    "कालीगण्डकी गाउँपालिका" = "5434",
    "गुल्मी दरबार गाउँपालिका" = "5435",
    "चन्द्रकोट गाउँपालिका" = "5436",
    "छत्रकोट गाउँपालिका" = "5437",
    "रुरुक्षेत्र गाउँपालिका" = "5442",
    "रेसुङ्गा नगरपालिका" = "5443",
    "सत्यवती गाउँपालिका" = "5444"
)

OUTPUT_FILE <- "Gulmi1_voters_retry_2.csv"
RETRY_INPUT <- "retry_targets_2.csv"
ERROR_LOG <- "retry_error_log_2.csv"

# Driver Config
DRIVER_BASE_PORT <- 4550L
NUM_WORKERS <- 5
PAGE_SIZE <- 100

# Robustness Config
MAX_DROPDOWN_TRIES <- 15
MAX_TABLE_TRIES <- 10
BASE_WAIT <- 8
SESSION_RESTART_EVERY <- 20

# ============================================ 
# HELPER FUNCTIONS (Copied & Adapted)
# ============================================ 

content_type_json <- function() add_headers(`Content-Type` = "application/json")

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
        POST = POST(url, content_type_json(), body = toJSON(body, auto_unbox = TRUE)))
    content(resp, "parsed")
}

wd_navigate <- function(session, url) wd_request(session, "POST", "/url", list(url = url))
wd_script <- function(session, script) wd_request(session, "POST", "/execute/sync", list(script = script, args = list()))
wd_source <- function(session) wd_request(session, "GET", "/source")$value
wd_quit <- function(session) DELETE(paste0(session$url, "/session/", session$session_id))

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
        wd_script(session, paste0("document.querySelector('", selector, "').value='", value, "';"))
        wd_script(session, paste0("document.querySelector('", selector, "').dispatchEvent(new Event('change'));"))
        Sys.sleep(base_wait + (i * 0.5)) # Add jitter
        
        html <- wd_script(session, paste0("return document.querySelector('", selector, "').outerHTML;"))$value
        opts <- suppressWarnings(tryCatch(read_html(html) %>% html_nodes("option"), error=function(e) NULL))
        vals <- if (!is.null(opts)) html_attr(opts, "value") else character(0)
        
        if (value == "" && length(vals) > 1) { # If selecting the default empty option
             namez <- html_text(opts)
             return(list(value=vals, name=namez))
        } else if (value != "" && any(vals == value)) { # If selecting a specific value
             return(list(value=vals))
        }
        message("  [", whatdesc, "] Try ", i, "/", tries, " waiting for update...")
    }
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

log_error <- function(vdc, ward, reg_centre, phase, msg){
    df = data.frame(time=Sys.time(), vdc=vdc, ward=ward, reg_centre=reg_centre, phase=phase, msg=msg, stringsAsFactors=FALSE)
    cat(paste0(paste(df, collapse=","), "\n"), file=ERROR_LOG, append=TRUE)
}

append_output <- function(dat){
    if (!file.exists(OUTPUT_FILE)) {
        write.csv(dat, OUTPUT_FILE, row.names=FALSE, fileEncoding="UTF-8")
    } else {
        write.table(dat, OUTPUT_FILE, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE, fileEncoding="UTF-8")
    }
}

# ============================================ 
# RETRY LOGIC
# ============================================ 

scrape_ward_subset <- function(ward, vdc_val, vdc_name, target_rc_names, port) {
    session <- NULL
    on.exit(if (!is.null(session)) wd_quit(session))
    
    msg_head <- function(...) paste0("[ ", vdc_name, "] [Ward ", ward, "] [Port ", port, "] ", ...)
    
    attempt <- 1
    done <- FALSE
    
    while(!done && attempt <= 3) {
        tryCatch({
            session <- create_session(port)
            wd_navigate(session, URL); Sys.sleep(5)
            
            # Navigate Hierarchy
            retry_select_dropdown(session, "#state", STATE_VAL, BASE_WAIT, MAX_DROPDOWN_TRIES, "state")
            if(!wait_for_populated(session, "#district")) stop("District failed")
            retry_select_dropdown(session, "#district", DISTRICT_VAL, BASE_WAIT, MAX_DROPDOWN_TRIES, "district")
            if(!wait_for_populated(session, "#vdc_mun")) stop("VDC failed")
            retry_select_dropdown(session, "#vdc_mun", vdc_val, BASE_WAIT, MAX_DROPDOWN_TRIES, "vdc")
            if(!wait_for_populated(session, "#ward")) stop("Ward failed")
            retry_select_dropdown(session, "#ward", ward, BASE_WAIT, MAX_DROPDOWN_TRIES, "ward")
            if(!wait_for_populated(session, "#reg_centre")) stop("RegCentre failed")

            # Get Available Reg Centres
            rc_opts <- retry_select_dropdown(session, "#reg_centre", "", BASE_WAIT, MAX_DROPDOWN_TRIES, "regcentre-pop")
            
            if (is.null(rc_opts) || length(rc_opts$value) == 0){
                log_error(vdc_name, ward, NA, "regcentre-list", "No regcentres found")
                break
            }
            
            valid_indices <- which(rc_opts$value != "")
            reg_values <- rc_opts$value[valid_indices]
            reg_names <- rc_opts$name[valid_indices]
            
            # Clean names for matching (trim whitespace, remove newlines)
            clean_reg_names <- trimws(gsub("\n", "", reg_names))
            clean_targets <- trimws(gsub("\n", "", target_rc_names))
            
            message(msg_head(sprintf("Found %d RCs. Targeting %d specific ones.", length(reg_values), length(target_rc_names))))
            
            # Loop through found RCs and match against targets
            processed_count <- 0
            for (i in seq_along(reg_values)){
                rc_val <- reg_values[i]
                rc_name <- clean_reg_names[i]
                
                # CHECK IF THIS RC IS IN OUR TARGET LIST
                # We use partial matching or exact matching? Let's try exact first, then partial if needed.
                # Actually, input csv might have partial names if scraped poorly.
                # Let's check if rc_name is contained in target list.
                
                # Note: target_rc_names contains the names from the CSV.
                # If the CSV has "NA", we can't match by name easily unless we scrape ALL.
                # But the user logic is: Scrape "failed" ones.
                # If RegCentre was NA in CSV, it means we likely failed to get the table or RC name.
                # In that case, we might want to scrape ALL RCs in that Ward that are NOT in the "Successful" list?
                # For now, let's assume the user wants to re-scrape the specific RCs that had low counts.
                # If target_rc_names has NA, we scrape ALL for that ward?
                # Let's stick to: if rc_name matches a target, we scrape.
                
                should_scrape <- rc_name %in% clean_targets
                
                # If target list has NA (meaning we want to retry everything/unknowns in this ward), 
                # or if we have specific matches.
                # Actually, if the CSV said "NA", we probably want to try *every* RC in this ward 
                # effectively treating it as a fresh scrape for that ward.
                if (any(is.na(target_rc_names))) {
                    should_scrape <- TRUE
                }
                
                if (should_scrape) {
                    message(msg_head(sprintf("MATCH! Processing Target RC: %s", rc_name)))
                    
                    # --- PROCESSING LOGIC (Same as original) ---
                    retry_select_dropdown(session, "#reg_centre", rc_val, 2, 5, "regcentre-select")
                    wd_script(session, "document.querySelector('#btnSubmit').click();")
                    Sys.sleep(5)
                    
                    # Set Page Size to 100
                    wd_script(session, sprintf("document.querySelector('select[name=\"tbl_data_length\"]').value='%d';", PAGE_SIZE))
                    wd_script(session, "document.querySelector('select[name=\"tbl_data_length\"]').dispatchEvent(new Event('change'));")
                    Sys.sleep(3)
                    
                    paged <- TRUE
                    collected <- data.frame()
                    page_idx <- 1
                    max_page_tries <- 100
                    
                    while(paged && page_idx <= max_page_tries){
                        tb <- retry_extract_table(session, MAX_TABLE_TRIES)
                        
                        if (!is.null(tb) && nrow(tb$df) > 0) {
                            thispage <- tb$df
                            thispage$Province <- STATE_NAME
                            thispage$District <- DISTRICT_NAME
                            thispage$VDC <- vdc_name
                            thispage$Ward <- ward
                            thispage$RegCentre <- rc_name # Use the clean name from dropdown
                            
                            append_output(thispage)
                            collected <- bind_rows(collected, thispage)
                            
                            message(msg_head(sprintf("  -> Pg %d: %d rows", page_idx, nrow(thispage))))
                        } else {
                            if (page_idx == 1) message(msg_head("  -> No data found."))
                            paged <- FALSE
                            break
                        }
                        
                        nxstat <- wd_script(session, "return document.querySelector('#tbl_data_next').className;")
                        if (is.null(nxstat) || grepl("disabled", nxstat$value)) {
                            paged <- FALSE
                        } else {
                            wd_script(session, "document.querySelector('#tbl_data_next').click();")
                            Sys.sleep(3)
                            page_idx <- page_idx + 1
                        }
                    }
                    processed_count <- processed_count + 1
                    # -------------------------------------------
                }
            }
            
            message(msg_head(sprintf("Finished Ward. Processed %d RCs.", processed_count)))
            done <- TRUE
            
        }, error=function(e){
            attempt <<- attempt + 1
            log_error(vdc_name, ward, NA, "ward-attempt", e$message)
            message(msg_head(sprintf("ERROR: %s. Retry %d/3...", e$message, attempt)))
            Sys.sleep(10)
        })
    }
}

# ============================================ 
# EXECUTION
# ============================================ 

message("=== RETRY VOTER SCRAPER ===")

if (!file.exists(RETRY_INPUT)) {
    stop("Retry targets file not found: ", RETRY_INPUT)
}

targets_df <- read_csv(RETRY_INPUT, show_col_types = FALSE)

# Clean targets: Filter out garbage rows (parsing errors often result in VDC="2" etc)
targets_clean <- targets_df %>%
    filter(!is.na(VDC)) %>%
    filter(VDC %in% names(VDC_MAP)) %>%
    group_by(VDC, Ward) %>%
    summarise(TargetRCs = list(RegCentre), .groups="drop")

message(sprintf("Loaded %d Ward-tasks for retry.", nrow(targets_clean)))

cl <- makeCluster(NUM_WORKERS, outfile="")
registerDoParallel(cl)

task_indices <- 1:nrow(targets_clean)
chunks <- split(task_indices, cut(task_indices, NUM_WORKERS, labels=FALSE))

results <- foreach(
    worker_id = 1:NUM_WORKERS,
    .combine = list,
    .multicombine = TRUE,
    .export = c("scrape_ward_subset", "create_session", "wd_request", "wd_navigate", "wd_script", "wd_source", "wd_quit", 
                "retry_select_dropdown", "retry_extract_table", "log_error", "append_output", "wait_for_populated", "content_type_json",
                "URL", "STATE_VAL", "STATE_NAME", "DISTRICT_VAL", "DISTRICT_NAME", "PAGE_SIZE", 
                "DRIVER_BASE_PORT", "MAX_DROPDOWN_TRIES", "MAX_TABLE_TRIES", "SESSION_RESTART_EVERY", 
                "OUTPUT_FILE", "ERROR_LOG", "BASE_WAIT", "targets_clean", "chunks", "VDC_MAP"),
    .packages = c("httr","jsonlite","rvest","tidyverse")
) %dopar% {
    
    my_port <- DRIVER_BASE_PORT + (worker_id - 1)
    my_chunk <- chunks[[worker_id]]
    
    if (length(my_chunk) > 0) {
        for (idx in my_chunk) {
            t <- targets_clean[idx, ]
            vdc_val <- VDC_MAP[[t$VDC]]
            
            # Handle list column unnesting if needed, but it's a list of character vectors
            target_rcs <- t$TargetRCs[[1]]
            
            if (!is.null(vdc_val)) {
                message(sprintf("Worker %d starting Retry: %s Ward %d", worker_id, t$VDC, t$Ward))
                scrape_ward_subset(t$Ward, vdc_val, t$VDC, target_rcs, my_port)
            } else {
                message(sprintf("Skipping unknown VDC: %s", t$VDC))
            }
        }
    }
    TRUE
}

stopCluster(cl)
message("Retry completed.")
