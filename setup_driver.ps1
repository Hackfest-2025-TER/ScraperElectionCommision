# Script to download ChromeDriver for Chrome 143 (Stable)
$errorActionPreference = "Stop"

$chromeDriverZip = "chromedriver-win64.zip"
$extractPath = "chromedriver-win64"

# Chrome 143.0.7499.169 is Stable - using exact version URL
$url = "https://storage.googleapis.com/chrome-for-testing-public/143.0.7499.169/win64/chromedriver-win64.zip"

Write-Host "Downloading ChromeDriver 143.0.7499.169 from: $url"
Invoke-WebRequest -Uri $url -OutFile $chromeDriverZip

Write-Host "Extracting..."
Expand-Archive -Path $chromeDriverZip -DestinationPath . -Force

# Move the exe to current dir
if (Test-Path "$extractPath\chromedriver.exe") {
    Move-Item "$extractPath\chromedriver.exe" -Destination . -Force
}

Write-Host "ChromeDriver downloaded."
Write-Host "Starting ChromeDriver on port 4548..."
Start-Process -FilePath ".\chromedriver.exe" -ArgumentList "--port=4548" -NoNewWindow

Write-Host "Driver started. Now run your R script."
