###########################################################################
# Determinants of County-Level Democratic Swing (2016–2020)
# Data Integration, OLS Modelling, and Monte Carlo Simulation
# -------------------------------------------------------------------------
# Author:      Gastón Sola
# Affiliation: MSc Data Science and Public Policy, University College London
# Contact:     gaston.sola.25@ucl.ac.uk
# Date:        January 2026
# -------------------------------------------------------------------------
# Description: End-to-end policy analysis pipeline including multi-source
# data integration (API, web scraping, FWF archives), OLS econometric
# modelling, and a parallelised Monte Carlo simulation study.
# See README.md for full documentation and data sources.
###########################################################################

# 1. ENVIRONMENT SETUP ----------------------------------------------------

# Clear global environment and force garbage collection for memory stability
rm(list = ls())
gc()

# Close any active graphic devices
if(!is.null(dev.list())) dev.off()

# Global options: Prevent scientific notation to maintain FIPS code integrity
options(scipen = 999)

# Load essential libraries for data science and parallel processing
library(tidyverse)   # Data manipulation and visualization
library(fredr)       # Federal Reserve Economic Data (FRED) API access
library(rvest)       # Web scraping for structural economic metrics
library(lubridate)   # Temporal data parsing
library(stringr)     # String operations and Regex patterns
library(stargazer)   # Professional econometric tables
library(broom)       # Tidy model outputs
library(foreach)     # Framework for iterative loops
library(doParallel)  # Parallel backend for foreach
library(maps)        # Geographic polygon data

# 2. SOURCE 1: MIT ELECTION DATA ------------------------------------------

# Methodology: Loading historical county-level presidential results.
# Variable types are strictly enforced to preserve leading zeros in FIPS codes.
file_path <- "countypres_2000-2024.tab" 

election_data <- read_tsv(file_path, col_types = cols(
  county_fips = col_character(),
  state_po = col_character(),
  year = col_integer()
))

# Validation Check: Structural integrity and distribution of votes
print("--- Check 1: MIT Election Data Loaded ---")
glimpse(election_data)
print(summary(election_data$candidatevotes))

# Memory management
gc()
Sys.sleep(1)

# 3. SOURCE 2: FRED API (UNEMPLOYMENT DATA) -------------------------------

if (file.exists("unemployment_final_backup.rds")) {
  unemployment_final <- readRDS("unemployment_final_backup.rds")
} else {
  # API Authentication
  fredr_set_key("d914dbf1722b4c957fd8aaa0f81596d5")
  
  # Iterative state list to bypass the FRED 1,000-series search limitation
  states_list <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                   "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                   "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                   "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                   "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                   "New Hampshire", "New Jersey", "New Mexico", "New York", 
                   "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                   "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                   "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                   "West Virginia", "Wisconsin", "Wyoming")
  
  # Step A: Automated identification of Series IDs
  message("--- Phase A: Searching for County Series IDs ---")
  county_ids_info <- map_dfr(states_list, function(st) {
    message(paste("Searching IDs for:", st)) # Progress Indicator
    Sys.sleep(0.3) 
    fredr_series_search_text(
      search_text = paste("unemployment rate county", st),
      limit = 1000
    ) %>%
      filter(str_detect(id, "^LAUCN"), str_detect(title, "Unemployment Rate"))
  })
  
  target_ids <- county_ids_info$id
  total_series <- length(target_ids)
  message(paste("Total Series found:", total_series))
  
  # Step B: Longitudinal data acquisition
  message("--- Phase B: Downloading Observations (This will take ~35 mins) ---")
  
  # We use a counter to show progress every 50 downloads
  counter <- 0
  unemployment_raw <- map_dfr(target_ids, function(id_code) {
    counter <<- counter + 1
    if (counter %% 50 == 0) {
      message(paste("Progress:", counter, "/", total_series, "series downloaded..."))
    }
    
    Sys.sleep(0.6) # Adherence to FRED rate limits
    tryCatch({
      fredr(series_id = id_code, observation_start = as.Date("2016-01-01"),
            observation_end = as.Date("2020-12-31"), frequency = "a")
    }, error = function(e) return(NULL))
  })
  
  # Transformation
  unemployment_final <- unemployment_raw %>%
    mutate(date = as.Date(date),
           year = year(date),
           county_fips = str_sub(series_id, 6, 10)) %>%
    filter(year %in% c(2016, 2020)) %>%
    select(year, county_fips, value) %>%
    rename(unemp_rate = value)
  
  message("Saving backup to 'unemployment_final_backup.rds'...")
  saveRDS(unemployment_final, "unemployment_final_backup.rds")
}

print("--- Check 3: FRED Tidy Data Processed ---")
glimpse(unemployment_final)

# 4. SOURCE 3: WEB SCRAPING (MIT LIVING WAGE) -----------------------------

# Reproducibility Logic: Utilize pre-scraped structural indicators if available
if (file.exists("living_wage_final.rds")) {
  living_wage_final <- readRDS("living_wage_final.rds")
} else {
  # URL mapping dictionary for all 51 administrative districts
  state_dictionary <- c(
    "01"="ALABAMA", "02"="ALASKA", "04"="ARIZONA", "05"="ARKANSAS", "06"="CALIFORNIA",
    "08"="COLORADO", "09"="CONNECTICUT", "10"="DELAWARE", "11"="DISTRICT OF COLUMBIA",
    "12"="FLORIDA", "13"="GEORGIA", "15"="HAWAII", "16"="IDAHO", "17"="ILLINOIS",
    "18"="INDIANA", "19"="IOWA", "20"="KANSAS", "21"="KENTUCKY", "22"="LOUISIANA",
    "23"="MAINE", "24"="MARYLAND", "25"="MASSACHUSETTS", "26"="MICHIGAN", "27"="MINNESOTA",
    "28"="MISSISSIPPI", "29"="MISSOURI", "30"="MONTANA", "31"="NEBRASKA", "32"="NEVADA",
    "33"="NEW HAMPSHIRE", "34"="NEW JERSEY", "35"="NEW MEXICO", "36"="NEW YORK",
    "37"="NORTH CAROLINA", "38"="NORTH DAKOTA", "39"="OHIO", "40"="OKLAHOMA",
    "41"="OREGON", "42"="PENNSYLVANIA", "44"="RHODE ISLAND", "45"="SOUTH CAROLINA",
    "46"="SOUTH DAKOTA", "47"="TENNESSEE", "48"="TEXAS", "49"="UTAH", "50"="VERMONT",
    "51"="VIRGINIA", "53"="WASHINGTON", "54"="WEST VIRGINIA", "55"="WISCONSIN", "56"="WYOMING"
  )
  
  # Scraper implementation to extract MIT Living Wage values per state
  scrape_living_wage <- function(state_id) {
    st_name <- state_dictionary[state_id]
    message(paste("Scraping Living Wage for:", st_name)) 
    
    url <- paste0("https://livingwage.mit.edu/states/", state_id)
    page <- tryCatch(read_html(url), error = function(e) return(NULL))
    if(is.null(page)) return(NULL)
    
    Sys.sleep(1.0) # Throttling to prevent server-side refusal
    
    table_node <- page %>% html_node("table")
    if(!is.null(table_node)) {
      df_raw <- table_node %>% html_table(header = FALSE)
      target_row <- which(df_raw[, 1] == "Living Wage")
      wage_val <- if(length(target_row) > 0) as.character(df_raw[target_row, 2]) else NA
      return(data.frame(state = st_name, living_wage_text = wage_val))
    }
    return(NULL)
  }
  
  living_wage_scraped <- map_dfr(names(state_dictionary), scrape_living_wage)
  
  # Data cleaning: Coerce text-based currency to numeric format
  living_wage_final <- living_wage_scraped %>%
    mutate(living_wage_num = as.numeric(str_remove_all(living_wage_text, "\\$")))
  
  saveRDS(living_wage_final, "living_wage_final.rds")
}

print("--- Check 4: Scraped Living Wage Data Complete ---")
glimpse(living_wage_final)

# Memory optimization before processing large FWF files
gc()
Sys.sleep(1)

# 5. SOURCE 4: SEER POPULATION DATA (FWF) ---------------------------------

if (file.exists("population_clean_2016_2020.rds")) {
  population_clean <- readRDS("population_clean_2016_2020.rds")
} else {
  # Parsing large-scale Fixed-Width Format (FWF) data for demographic composition
  population_clean <- read_fwf(
    file = "us.1969_2023.20ages.adjusted.txt.gz",
    col_positions = fwf_cols(
      year          = c(1, 4),
      state_fips    = c(7, 8),
      county_fips_3 = c(9, 11),
      sex           = c(16, 16), 
      age_group     = c(17, 18), 
      population    = c(19, 30)
    ),
    col_types = cols(year = col_integer(), population = col_double(), .default = col_character())
  ) %>% 
    filter(year %in% c(2016, 2020)) %>%
    mutate(
      county_fips = str_pad(paste0(state_fips, county_fips_3), 5, side = "left", pad = "0"),
      is_female   = ifelse(sex == "2", 1, 0),
      is_young    = ifelse(age_group < "08", 1, 0) # Demographic proxy: age < 35
    ) %>%
    group_by(year, county_fips) %>%
    summarise(
      total_pop     = sum(population, na.rm = TRUE),
      female_pop    = sum(population * is_female, na.rm = TRUE),
      young_pop     = sum(population * is_young, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_female   = (female_pop / total_pop) * 100,
      pct_young    = (young_pop / total_pop) * 100
    )
  
  saveRDS(population_clean, "population_clean_2016_2020.rds")
}

print("--- Check 5: SEER Data Processed ---")
glimpse(population_clean)

# 6. DATA HARMONIZATION & JOINING -----------------------------------------

# Technical Step: Create a unique county lookup to prevent duplicate-row inflation
county_lookup <- election_data %>%
  select(county_fips, county_name, state) %>%
  mutate(county_fips = str_pad(county_fips, 5, side = "left", pad = "0")) %>%
  distinct(county_fips, .keep_all = TRUE) 

# Outcome Transformation: Computing the 2016-2020 Democratic Swing
votos_swing <- election_data %>%
  filter(year %in% c(2016, 2020), party == "DEMOCRAT") %>%
  mutate(county_fips = str_pad(county_fips, 5, side = "left", pad = "0")) %>%
  group_by(year, county_fips, state) %>%
  summarise(candidatevotes = sum(candidatevotes), totalvotes = sum(totalvotes), .groups = "drop") %>%
  mutate(dem_share = (candidatevotes / totalvotes) * 100) %>%
  select(year, county_fips, state, dem_share) %>%
  pivot_wider(names_from = year, values_from = dem_share, names_prefix = "dem_") %>%
  mutate(swing = dem_2020 - dem_2016)

# Economic Transformation: Pivoting unemployment data to wide format
unemployment_wide_clean <- unemployment_final %>%
  mutate(county_fips = str_pad(county_fips, 5, side = "left", pad = "0")) %>%
  group_by(year, county_fips) %>%
  summarise(unemp_rate = mean(unemp_rate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = unemp_rate, names_prefix = "unemp_")

# Final Master Join: Integrating electoral, economic, and demographic data
df_master <- votos_swing %>%
  left_join(unemployment_wide_clean, by = "county_fips") %>%
  left_join(population_clean %>% filter(year == 2020) %>% 
              select(county_fips, pop_2020 = total_pop, pct_female, pct_young), by = "county_fips") %>%
  left_join(living_wage_final %>% select(state, living_wage_num), by = "state") %>%
  left_join(county_lookup %>% select(county_fips, county_name), by = "county_fips") %>%
  mutate(
    unemp_diff = unemp_2020 - unemp_2016,
    pop_log = log(pop_2020 + 1)
  )

print("--- Check 6.4: FINAL CLEAN MASTER DATASET ---")
glimpse(df_master)

# 7. DESCRIPTIVE VISUALIZATIONS -------------------------------------------

# Preparation of the visualization directory
dir.create("figures", showWarnings = FALSE)

# Aesthetic theme for geospatial mapping
remove_axis <- theme(
  axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
  axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank()
)

# Figure 1: Distribution of Electoral Swing
# 
my_plot_1 <- ggplot(df_master, aes(x = swing)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "#3399FF", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "#FF6666", linewidth = 1) +
  annotate("text", x = 15, y = 0.05, label = paste("Mean Swing:", round(mean(df_master$swing, na.rm=T), 2)), color = "darkblue") +
  theme_bw() +
  labs(title = "Figure 1: Distribution of Electoral Swing (2016-2020)",
       subtitle = "Normality check and central tendency of county-level shifts",
       x = "Democratic Swing (%)", y = "Density") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("figures/Plot1_Histogram_Electoral_Swing.png", plot = my_plot_1, width = 10, height = 6, dpi = 300)

# Figure 2: Longitudinal Spatial Realignment
# 
map_data_long <- df_master %>%
  select(county_fips, state, county_name, dem_2016, dem_2020) %>%
  pivot_longer(cols = c(dem_2016, dem_2020), names_to = "Year", values_to = "Share") %>%
  mutate(Year = str_remove(Year, "dem_"),
         region = tolower(state),
         subregion = tolower(county_name)) %>%
  mutate(subregion = str_replace_all(subregion, "st\\.", "st")) %>%
  filter(!is.na(Share))

final_map_df <- map_data("county") %>%
  inner_join(map_data_long, by = c("region", "subregion"), relationship = "many-to-many") %>%
  arrange(Year, group, order)

my_plot_2 <- ggplot(final_map_df, aes(x = long, y = lat, group = group, fill = Share)) +
  geom_polygon(color = NA) +
  facet_wrap(~Year) + 
  scale_fill_gradient2(low = "#FF6666", mid = "white", high = "#3399FF", midpoint = 50, name = "% Democrat") +
  coord_fixed(1.3) +
  theme_bw() +
  remove_axis +
  labs(title = "Figure 2: Spatial Evolution of Democratic Vote Share",
       subtitle = "County-level comparison between 2016 and 2020")

ggsave("figures/Plot2_Longitudinal_Spatial_Comparison.png", plot = my_plot_2, width = 10, height = 6, dpi = 300)

# 8. ECONOMETRIC MODELLING ------------------------------------------------

# Model Estimation: Multivariate OLS to evaluate determinants of the shift
final_model <- lm(swing ~ unemp_diff + living_wage_num + pct_young + pct_female + pop_log, 
                  data = df_master)

# Academic Reporting: Generation of a professional regression table
print("--- FINAL REGRESSION RESULTS ---")
stargazer(final_model, type = "text", 
          title = "Determinants of Democratic Swing (2016-2020)",
          covariate.labels = c("Unemployment Change", "Living Wage (State)", 
                               "% Pop < 35 Years", "% Female", "Population (Log)"),
          dep.var.labels = "Democratic Swing (%)")

# 9. SIMULATION STUDY (PARALLEL COMPUTING) --------------------------------

# Object cleanup to free memory prior to parallel backend initialization
rm(unemployment_raw, election_data, population_clean)
gc()

# Simulation Setup: Fixing the covariate matrix for repeated sampling
df_sim <- df_master %>%
  select(swing, unemp_diff, living_wage_num, pct_young, pct_female, pop_log) %>%
  drop_na()

X_mat <- model.matrix(swing ~ unemp_diff + living_wage_num + pct_young + pct_female + pop_log, data = df_sim)
true_betas <- coef(final_model) 
sigma_noise <- sigma(final_model)
n_sim <- 1000

# Benchmark: Sequential Simulation Study
time_seq <- system.time({
  results_seq <- foreach(i = 1:n_sim, .combine = rbind) %do% {
    epsilon <- rnorm(nrow(X_mat), mean = 0, sd = sigma_noise)
    y_star <- X_mat %*% true_betas + epsilon
    model_tmp <- lm(y_star ~ X_mat - 1)
    coef(model_tmp)
  }
})

# Optimization: Parallel Simulation Study
# 
# Capped at 4 cores for thermal stability and OS responsiveness
num_cores <- max(1, min(4, detectCores() - 2))  
cl <- makeCluster(num_cores)
registerDoParallel(cl)

time_par <- system.time({
  results_par <- foreach(i = 1:n_sim, .combine = rbind, .packages = "stats") %dopar% {
    epsilon <- rnorm(nrow(X_mat), mean = 0, sd = sigma_noise)
    y_star <- X_mat %*% true_betas + epsilon
    model_tmp <- lm(y_star ~ X_mat - 1)
    coef(model_tmp)
  }
})
stopCluster(cl)

# Print Performance Comparison
print("--- COMPUTATIONAL PERFORMANCE ---")
print(paste("Sequential Time:", round(time_seq["elapsed"], 3), "seconds"))
print(paste("Parallel Time (4 cores):", round(time_par["elapsed"], 3), "seconds"))
print(paste("Speedup Factor:", round(time_seq["elapsed"] / time_par["elapsed"], 2), "x"))

# Validation: Assessing Estimator Bias and Consistency
sim_results <- as.data.frame(results_par)
colnames(sim_results) <- names(true_betas)
sim_means <- colMeans(sim_results)

print("--- MONTE CARLO VALIDATION TABLE ---")
print(data.frame(
  Variable = names(true_betas),
  True_Beta = as.numeric(true_betas),
  Simulated_Mean = as.numeric(sim_means),
  Bias = as.numeric(sim_means - true_betas)
))

# 10. ANALYTICAL VISUALIZATIONS -------------------------------------------

# Figure 3: Forest Plot of Econometric Estimates
# 
model_output <- tidy(final_model, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(term = c("Unemployment Change", "Living Wage (State)", "% Young Pop", "% Female Pop", "Population (Log)"))

my_plot_3 <- ggplot(model_output, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 4, color = "#3399FF") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, color = "gray30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#FF6666") +
  theme_bw() +
  labs(title = "Figure 3: Econometric Results - Standardized Coefficients",
       subtitle = "95% Confidence Intervals for the Determinants of Democratic Swing",
       x = "Estimate", y = "Independent Variables")

ggsave("figures/Plot3_Econometric_Results.png", plot = my_plot_3, width = 10, height = 6, dpi = 300)

# Figure 4: Sampling Distribution (Bias Check)
# 
my_plot_4 <- ggplot(sim_results, aes(x = unemp_diff)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#69b3a2", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) +
  geom_vline(aes(xintercept = true_betas["unemp_diff"]), color = "red", linetype = "dashed", linewidth = 1.2) +
  xlim(0.75, 1.75) + 
  theme_minimal() +
  labs(title = "Figure 4: Monte Carlo Estimator Consistency",
       subtitle = "Distribution of 1,000 simulated coefficients for Unemployment Change",
       x = "Simulated Beta Value", y = "Density")

ggsave("figures/Plot4_Monte_Carlo_Sampling_Distribution.png", plot = my_plot_4, width = 10, height = 6, dpi = 300)

###########################################################################
# END OF SCRIPT
###########################################################################