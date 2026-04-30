# =============================================================================
# REPLICATION CODE: Lethality of Crime and COVID-19 — CDC Mortality Data ----
# =============================================================================
# Description : County-month panel ITS analysis using CDC cause-of-death
#               microdata; homicide decomposed into gun vs. non-gun
# Data        : CDC Mortality Microdata (2016–2022), UCR Offenses Known (county)
# Unit        : County-month (county_fips)
# Main Model  : fepois(homicide ~ time + post + time_post + offset(log_denom8)
#               | month + county_fips, cluster = ~county_fips)
# Author      : [Author]
# Last Update : [Date]
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(fixest)
library(lubridate)
library(tidycensus)


# -----------------------------------------------------------------------------
# 1. RECODE AND SAVE CDC MICRODATA BY YEAR (2016–2022) ----
# -----------------------------------------------------------------------------
# ICD-10 cause-of-death categories:
#   homicide_gun    : X93–X95
#   homicide_nongun : X85–X92, X96–X99, Y00–Y09, Y871
#   suicide_gun     : X72–X74
#   suicide_nongun  : X60–X71, X75–X84, Y870
#   transport       : V00–V99
#   fall            : W00–W19
#   drowning        : W65–W69
#   poisoning_unintentional : X40–X49

process_year_data <- function(year) {
  cat("Processing year:", year, "\n")
  dat <- read_rds(paste0("C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/Data/CDC mortality_2016_2022/cleaned_data(", year, ")_260218.rds"))
  dat <- dat %>%
    mutate(icd_chapter_d = case_when(
      str_detect(icd10, "^X9[3-5]")                                  ~ "homicide_gun",
      str_detect(icd10, "^(X8[5-9]|X9[0-2]|X9[6-9]|Y0[0-9]|Y871)") ~ "homicide_nongun",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(icd_chapter_d)) %>%
    select(year, abbr, state_fips, county_fips, icd_chapter_d, month)
  saveRDS(dat, sprintf("C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/Data/CDC mortality_2016_2022/recoded_data(%d)_260312.rds", year))
  cat("  Saved year:", year, "\n")
}

for (year in 2016:2022) process_year_data(year)


# -----------------------------------------------------------------------------
# 2. AGGREGATE TO COUNTY-MONTH LEVEL AND COMPLETE PANEL ----
# -----------------------------------------------------------------------------

load_year_data <- function(year) {
  readRDS(sprintf("C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/Data/UCR known offense_2016_2024/recoded_data(%d)_260312.rds", year)) %>%
    mutate(year        = as.integer(year),
           county_fips = as.numeric(county_fips),
           month       = as.integer(month)) %>%
    group_by(year, county_fips, icd_chapter_d, month) %>%
    summarise(total = n(), .groups = "drop")
}

all_years_combined <- lapply(2016:2022, load_year_data) %>% bind_rows()

# Complete balanced county-month grid (zeros for missing cause-month cells)
all_counties <- unique(all_years_combined$county_fips)

county_all_years_combined <- all_years_combined %>%
  filter(!is.na(icd_chapter_d)) %>%
  complete(
    year          = 2016:2022,
    county_fips   = all_counties,
    icd_chapter_d = c("homicide_gun", "homicide_nongun", "suicide_gun", "suicide_nongun",
                      "transport", "fall", "drowning", "poisoning_unintentional"),
    month         = 1:12
  ) %>%
  mutate(total = replace_na(total, 0))

saveRDS(county_all_years_combined,
        "C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/Data/CDC mortality_2016_2022/cdc_monthly_clean_data.RDS")

# -----------------------------------------------------------------------------
# 3. LOAD AND PREPARE ANALYSIS DATA ----
# -----------------------------------------------------------------------------

# UCR assault denominator (county-level balanced panel, known to police)
ucr_assault <- readRDS("C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/Data/UCR known offense_2016_2024/countylevel_consistent report_2016_2024 (known to police).RDS") %>%
  filter(year <= 2022) %>%
  mutate(county_fips = as.numeric(county_fips)) %>%
  mutate(population = as.numeric(total_pop)) %>%
  select(-contains("lethality"), -any_of(c("date", "time", "post", "time_post")))

# CDC homicide counts: gun, non-gun, total
cdc <- readRDS("C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/Data/CDC mortality_2016_2022/cdc_monthly_clean_data.RDS") %>%
  filter(icd_chapter_d %in% c("homicide_gun", "homicide_nongun"), year >= 2016) %>%
  mutate(county_fips = as.numeric(county_fips)) %>%
  group_by(year, county_fips, month, icd_chapter_d) %>%
  summarise(homicide = sum(total, na.rm = TRUE), .groups = "drop")

cdc_all <- bind_rows(
  cdc,
  cdc %>%
    group_by(year, county_fips, month) %>%
    summarise(homicide = sum(homicide, na.rm = TRUE), .groups = "drop") %>%
    mutate(icd_chapter_d = "homicide_total")
) %>% arrange(year, county_fips, month)

# -----------------------------------------------------------------------------
# 4. CONSTRUCT ANALYSIS DATASETS (TOTAL, GUN, NON-GUN) ----
# -----------------------------------------------------------------------------
# Criteria: (1) population > 25,000; (2) complete 84 county-months (2016–2022)
# lethality8: homicide / (homicide + agg assault + robbery total) [MAIN DENOMINATOR]

make_analysis_df <- function(homicide_type) {
  cdc_all %>%
    filter(icd_chapter_d == homicide_type) %>%
    left_join(ucr_assault, by = c("year", "county_fips", "month")) %>%
    group_by(county_fips) %>%
    filter(mean(population, na.rm = TRUE) > 25000) %>%
    filter(n_distinct(paste(year, month)) == 84) %>%
    ungroup() %>%
    mutate(
      lethality8 = case_when(
        (homicide + actual_assault_aggravated + actual_robbery_total) == 0 ~ NA_real_,
        TRUE ~ homicide / (homicide + actual_assault_aggravated + actual_robbery_total)
      ),
      date = as.Date(sprintf("%d-%02d-01", year, month)),
      time      = as.numeric(date - min(date)) / 30.44,
      post      = if_else(date >= as.Date("2020-03-01"), 1, 0),
      time_post = if_else(post == 1, time - min(time[post == 1]), 0)
    )
}

df_total  <- make_analysis_df("homicide_total")
df_gun    <- make_analysis_df("homicide_gun")
df_nongun <- make_analysis_df("homicide_nongun")

# -----------------------------------------------------------------------------
# 5. MAIN ANALYSIS: FIXED-EFFECTS POISSON ITS ----
# -----------------------------------------------------------------------------
# DV:     homicide count
# Offset: log(agg assault + robbery total), UCR offenses known
# FE:     county (county_fips) + calendar month
# SE:     clustered by county_fips

for (nm in c("df_total", "df_gun", "df_nongun")) {
  df <- get(nm)
  df$log_denom8 <- log(df$actual_assault_aggravated + df$actual_robbery_total)
  df$log_denom8[is.infinite(df$log_denom8) | is.nan(df$log_denom8)] <- NA
  assign(nm, df)
}

model_total  <- fepois(homicide ~ time + post + time_post + offset(log_denom8) | month + county_fips,
                    cluster = ~county_fips, data = filter(df_total,  !is.na(log_denom8)))
model_gun    <- fepois(homicide ~ time + post + time_post + offset(log_denom8) | month + county_fips,
                    cluster = ~county_fips, data = filter(df_gun,    !is.na(log_denom8)))
model_nongun <- fepois(homicide ~ time + post + time_post + offset(log_denom8) | month + county_fips,
                    cluster = ~county_fips, data = filter(df_nongun, !is.na(log_denom8)))

etable(model_total, model_gun, model_nongun)
exp(coef(model_total)); exp(coef(model_gun)); exp(coef(model_nongun))   # IRR


## 5-1. Figure helpers: observed / fitted / counterfactual trend --------------

make_fig <- function(model, data) {
  cf <- feols(log_denom8 ~ time | county_fips + month, cluster = ~county_fips,
              data = filter(data, post == 0, !is.infinite(log_denom8)))
  data %>%
    filter(!is.na(log_denom8)) %>%
    mutate(
      fitted         = predict(model, newdata = ., type = "response"),
      counterfactual = predict(model, type = "response",
                               newdata = mutate(., post = 0, time_post = 0,
                                                log_denom8 = predict(cf, newdata = .)))
    ) %>%
    group_by(date) %>%
    summarise(observed       = mean(homicide, na.rm = TRUE),
              fitted         = mean(fitted,   na.rm = TRUE),
              counterfactual = mean(counterfactual, na.rm = TRUE), .groups = "drop")
}

plot_fig <- function(fig_data, title_label) {
  ggplot(fig_data, aes(x = date)) +
    geom_line(aes(y = observed,       color = "Observed"),  linewidth = 0.8) +
    geom_line(aes(y = fitted,         color = "Fitted"),    linewidth = 0.7) +
    geom_line(aes(y = counterfactual, color = "Pretrend"),  linewidth = 0.7, linetype = "dashed") +
    geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red", alpha = 0.6) +
    scale_color_manual(values = c("Observed" = "orange", "Fitted" = "steelblue", "Pretrend" = "darkgreen")) +
    labs(title = title_label, x = NULL, y = "Homicide Count", color = NULL) +
    theme_minimal() +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = c(0, 0)) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 9))
}

plot_fig(make_fig(model_total,  df_total),  "Total Homicide")
plot_fig(make_fig(model_gun,    df_gun),    "Gun Homicide")
plot_fig(make_fig(model_nongun, df_nongun), "Non-Gun Homicide")


# =============================================================================
# END OF REPLICATION CODE ----
# =============================================================================