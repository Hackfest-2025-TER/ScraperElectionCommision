# Start 5 ChromeDriver instances for parallel scraping
$ports = @(4550, 4551, 4552, 4553, 4554)

Write-Host "Killing any existing ChromeDriver processes..."
taskkill /F /IM chromedriver.exe 2>$null

Start-Sleep -Seconds 2

foreach ($port in $ports) {
    Write-Host "Starting ChromeDriver on port $port..."
    Start-Process -FilePath ".\chromedriver.exe" -ArgumentList "--port=$port" -NoNewWindow
    Start-Sleep -Seconds 1
}

Write-Host ""
Write-Host "All 5 ChromeDriver instances started:"
Write-Host "  - Port 4550 (Worker 1)"
Write-Host "  - Port 4551 (Worker 2)"
Write-Host "  - Port 4552 (Worker 3)"
Write-Host "  - Port 4553 (Worker 4)"
Write-Host "  - Port 4554 (Worker 5)"
Write-Host ""
Write-Host "Now run: Rscript voter_scraping_parallel.R"
