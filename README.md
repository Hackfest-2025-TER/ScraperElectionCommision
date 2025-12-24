# Election Commission Voter List Scraper

R + Selenium scraper for Nepal Election Commission voter list data.

## Output
- `dhulikhel_voter_list_full.csv` - 25,924 voter records from Dhulikhel Municipality

## Requirements
- R with packages: `httr`, `jsonlite`, `rvest`, `tidyverse`, `parallel`, `foreach`, `doParallel`
- Chrome browser
- ChromeDriver (matching your Chrome version)

## Usage

1. Download ChromeDriver for your Chrome version:
```powershell
.\setup_driver.ps1
```

2. Start 3 ChromeDriver instances:
```powershell
.\start_drivers.ps1
```

3. Run the scraper:
```powershell
Rscript voter_scraping_parallel.R
```

## Features
- 100 entries per page
- Full pagination
- 3 parallel workers
- Includes Province, District, VDC metadata
