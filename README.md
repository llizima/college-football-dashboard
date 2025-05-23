# College Football Dashboard

An interactive R Shiny dashboard for exploring NCAA college football data, including team locations, stats, and visualizations.

## Features
- Interactive map of team locations with conference and stat filters
- Compare teams and conferences with side-by-side stats and radar charts
- Visualizations for stadium capacity, elevation, time zone, and more
- Game insights: win rates, completion rates, and trends
- Data tables for teams, games, recruiting, rankings, and more
- **Supports loading data from either local files or Azure Blob Storage**

## Data Sources
- Data files: `teams_geocoded.csv`, `games.csv`, `recruiting.csv`, `rankings.csv`, `sp_ratings.csv`, `talent_data.csv`
- Data can be loaded from:
  - Local `data/` folder (default)
  - **Azure Blob Storage** (set `use_azure <- TRUE` in `server.R`)

## Azure Blob Storage Setup
If you want to load data from Azure Blob Storage:
1. Set `use_azure <- TRUE` near the top of `server.R`.
2. The app will connect to the Azure storage account and container:
   - **Storage account:** `defaultresourcegrou89d2`
   - **Container:** `app-package-florida-consumer-func-2025-116ee8e`
3. **Set your Azure Storage key as an environment variable** named `AZURE_STORAGE_KEY`.
   - In R, run: `usethis::edit_r_environ()` and add:
     ```
     AZURE_STORAGE_KEY=your_actual_key_here
     ```
   - Save and restart R.

## Local Data Setup
If you want to use local files:
1. Set `use_azure <- FALSE` in `server.R`.
2. Place all required CSV files in the `data/` folder.

## Running the App
1. Open the project in RStudio.
2. Install required packages (see below).
3. Run the app:
   ```r
   shiny::runApp()
   ```

## Required R Packages
- shiny
- shinydashboard
- DT
- plotly
- leaflet
- dplyr
- tidyr
- scales
- viridis
- ggridges
- cowplot
- randomcoloR
- AzureStor (for Azure support)

Install any missing packages with:
```r
install.packages(c("shiny", "shinydashboard", "DT", "plotly", "leaflet", "dplyr", "tidyr", "scales", "viridis", "ggridges", "cowplot", "randomcoloR", "AzureStor"))
```

## Notes
- The app will automatically use Azure or local files based on the `use_azure` setting.
- **Never commit your Azure Storage key to git or GitHub.**
- For more details, see comments in `server.R`.

---

[GitHub Repository](https://github.com/llizima/college-football-dashboard)
