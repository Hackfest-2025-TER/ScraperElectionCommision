# Election Commission Data Scraper

This project is a tool capable of scraping voter list data from the Election Commission of Nepal. It is designed to handle large datasets by utilizing parallel processing and automated browser control to navigate through provinces, districts, VDCs, and wards.

## Tools Used

This project utilizes the following technologies and packages:

*   **R Language**: Core scripting and data manipulation.
    *   `tidyverse`: For data cleaning and organization.
    *   `rvest`: For parsing HTML content.
    *   `httr`: For handling HTTP requests.
    *   `parallel` / `doParallel`: For running multiple scraper instances simultaneously.
*   **Selenium (ChromeDriver)**: For automating web browser interaction to handle dynamic dropdowns and navigation.
*   **PowerShell**: For managing driver instances and setup scripts.

## Usage

1.  **Setup Drivers**:
    Run the setup script to download the appropriate ChromeDriver.
    ```powershell
    .\setup_driver.ps1
    ```

2.  **Start Driver Instances**:
    Initialize the browser instances required for parallel scraping.
    ```powershell
    .\start_drivers.ps1
    ```

3.  **Run Scraper**:
    Execute the main R script to begin the scraping process.
    ```powershell
    Rscript voter_scraping_parallel.R
    ```