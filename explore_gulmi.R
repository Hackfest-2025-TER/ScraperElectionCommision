library(httr)
library(jsonlite)
library(rvest)

# Constants
URL <- "https://voterlist.election.gov.np"

# Helper function to create a WebDriver session
create_session <- function(port = 4550) {
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

wd_quit <- function(session) {
    DELETE(paste0(session$url, "/session/", session$session_id))
}

# Main exploration
message("=== Exploring Gulmi District Values ===")
session <- create_session(4550)

tryCatch({
    wd_navigate(session, URL)
    Sys.sleep(3)

    # Get all states
    message("\n--- Available States ---")
    state_html <- wd_script(session, "return document.querySelector('#state').outerHTML;")$value
    state_opts <- read_html(state_html) %>% html_nodes("option")
    for (i in seq_along(state_opts)) {
        val <- html_attr(state_opts[i], "value")
        txt <- html_text(state_opts[i])
        if (!is.na(val) && val != "") {
            message(sprintf("Value: %s -> %s", val, txt))
        }
    }

    # Select Lumbini Pradesh (Province 5)
    wd_script(session, "document.querySelector('#state').value = '5';")
    wd_script(session, "document.querySelector('#state').dispatchEvent(new Event('change'));")
    Sys.sleep(3)

    # Get districts
    message("\n--- Districts in Lumbini Pradesh ---")
    dist_html <- wd_script(session, "return document.querySelector('#district').outerHTML;")$value
    dist_opts <- read_html(dist_html) %>% html_nodes("option")
    for (i in seq_along(dist_opts)) {
        val <- html_attr(dist_opts[i], "value")
        txt <- html_text(dist_opts[i])
        if (!is.na(val) && val != "") {
            message(sprintf("Value: %s -> %s", val, txt))
        }
    }

    # Looking for Gulmi - need to find the value
    message("\n--- Searching for Gulmi ---")
    gulmi_val <- NULL
    for (i in seq_along(dist_opts)) {
        txt <- html_text(dist_opts[i])
        if (grepl("गुल्मी", txt)) {
            gulmi_val <- html_attr(dist_opts[i], "value")
            message(sprintf("Found Gulmi! Value: %s -> %s", gulmi_val, txt))
            break
        }
    }

    if (!is.null(gulmi_val)) {
        # Select Gulmi
        wd_script(session, paste0("document.querySelector('#district').value = '", gulmi_val, "';"))
        wd_script(session, "document.querySelector('#district').dispatchEvent(new Event('change'));")
        Sys.sleep(3)

        # Get VDC/municipalities
        message("\n--- VDC/Municipalities in Gulmi ---")
        vdc_html <- wd_script(session, "return document.querySelector('#vdc_mun').outerHTML;")$value
        vdc_opts <- read_html(vdc_html) %>% html_nodes("option")
        for (i in seq_along(vdc_opts)) {
            val <- html_attr(vdc_opts[i], "value")
            txt <- html_text(vdc_opts[i])
            if (!is.na(val) && val != "") {
                message(sprintf("Value: %s -> %s", val, txt))
            }
        }
    }
}, finally = {
    wd_quit(session)
})

message("\n=== Exploration Complete ===")
