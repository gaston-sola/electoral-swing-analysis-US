# County-Level Democratic Swing (2016–2020): Data Integration, OLS Modelling, and Monte Carlo Simulation

**Gastón Sola** · MSc Data Science and Public Policy, University College London  
`gaston.sola.25@ucl.ac.uk` · January 2026

---

## Overview

This project investigates the drivers of the **Democratic Swing** at the county level between the 2016 and 2020 U.S. presidential elections. By integrating four heterogeneous data sources — electoral results, unemployment rates, cost-of-living proxies, and demographic denominators — the analysis tests the **retrospective voting hypothesis**: whether voters in counties with labour market deterioration punished the incumbent party.

The analysis is structured in three parts:

1. **Data harmonisation** — multi-source pipeline (TAB file parsing, FRED API, web scraping, SEER FWF archives)
2. **Econometric modelling** — OLS regression with demographic and economic controls
3. **Monte Carlo simulation** — 1,000-iteration bootstrap to validate OLS bias (result: negligible bias of 0.0004 on the primary regressor)

**Key result:** County-level unemployment change between 2016 and 2020 is a statistically significant negative predictor of Democratic vote share, consistent with retrospective economic voting theory, even after controlling for demographic composition and cost-of-living differences.

---

## Repository Structure

```
electoral-swing-analysis-US/
├── TRXM8_ECON0128_endofterm.r     # Main analysis script (fully reproducible)
├── REPORT_TRXM8_ECON0128_endofterm.pdf  # Written report with results
├── README_TRXM8_ECON0128_endofterm.txt  # Original technical README
├── Outcome_R.txt                   # R session output log
├── figures/
│   ├── Plot1_Histogram_Electoral_Swing.png
│   ├── Plot2_Longitudinal_Spatial_Comparison.png
│   ├── Plot3_Econometric_Results.png
│   └── Plot4_Monte_Carlo_Sampling_Distribution.png
```

> **Large data files** (`.tab`, `.gz`, `.rds`) are excluded from the repository due to size. See **Data Sources** below for download instructions.

---

## Data Sources

| Dataset | Source | How to obtain |
|---|---|---|
| Electoral outcomes (2000–2024) | MIT Election Lab | [countypres](https://dataverse.harvard.edu/dataverse/medsl) — download `countypres_2000-2024.tab` |
| County unemployment rates | FRED API (Federal Reserve Bank of St. Louis) | Free API key at [fred.stlouisfed.org](https://fred.stlouisfed.org/); script fetches automatically |
| Cost of living (living wage) | MIT Living Wage Project | Script scrapes [livingwage.mit.edu](https://livingwage.mit.edu/) — pre-downloaded backup provided |
| Population / demographics | SEER Program (NCI) | [seer.cancer.gov](https://seer.cancer.gov/data/) — download `us.1969_2023.20ages.adjusted.txt.gz` |

---

## Prerequisites

```r
# Required packages (auto-installed by script if missing)
install.packages(c(
  "tidyverse", "lubridate", "stringr",
  "fredr", "rvest",
  "stargazer", "broom",
  "ggplot2", "maps",
  "foreach", "doParallel"
))
```

A **FRED API key** is required for the unemployment download. Register for free at [fred.stlouisfed.org/docs/api/api_key.html](https://fred.stlouisfed.org/docs/api/api_key.html) and set it in the script:

```r
fredr_set_key("YOUR_API_KEY_HERE")
```

Alternatively, use the pre-downloaded backup file `unemployment_final_backup.rds` — the script detects it automatically.

---

## How to Run

1. Clone this repository
2. Place the large raw data files in the same directory (see Data Sources)
3. Open `TRXM8_ECON0128_endofterm.r` in RStudio
4. Set the working directory: `setwd(dirname(rstudioapi::getActiveDocumentContext()$path))`
5. Run the script — it creates a `figures/` subfolder automatically

**Expected runtime:** 30–90 minutes (dominated by the FRED API download for ~3,100 counties; skipped automatically if backup file is present).

---

## Technical Notes

- **FIPS code integrity:** All county FIPS codes are enforced as 5-character zero-padded strings to prevent silent join failures (e.g., Alabama's `01001` must not be cast to integer `1001`)
- **Monte Carlo parallelisation:** Capped at 4 cores (`doParallel`) for thermal stability; adjust `num_cores` in the script as needed
- **OLS bias validation:** 1,000-iteration Monte Carlo yields bias = 0.0004 on the unemployment coefficient — negligible, confirming estimator reliability

---

## Citation

> Sola, G. (2026). *Determinants of County-Level Democratic Swing (2016–2020): Data Integration, OLS Modelling, and Monte Carlo Simulation*. MSc Data Science and Public Policy coursework, University College London.

---

## Contact

Gastón Sola · [gaston.sola.25@ucl.ac.uk](mailto:gaston.sola.25@ucl.ac.uk)
