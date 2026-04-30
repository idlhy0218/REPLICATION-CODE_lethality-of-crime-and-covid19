# =============================================================================
# REPLICATION CODE: Lethality of Crime and COVID-19 — County-Level Analysis
# =============================================================================
# Description : County-month panel analysis of homicide lethality (2016–2024)
#               Parallel to main analysis (agency-level) but aggregated to county
# Data        : UCR Offenses Known Monthly (2016–2024), ACS 5-Year Estimates
# Unit        : County-month (county_fips)
# Author      : Heeyoung Lee
# Last Update : 2026-04-29 (YYYY-MM-DD)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACKAGES ----
# -----------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyverse)
library(fixest)
library(janitor)
library(tidycensus)
library(purrr)


# -----------------------------------------------------------------------------
# 1. LOAD AND FILTER UCR OFFENSES KNOWN DATA (2016–2024) ----
# -----------------------------------------------------------------------------
basefolder <- "C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/"

cat("Loading UCR Offenses Known data (2016-2024)...\n")

years <- 2016:2024

ucr_list <- lapply(years, function(yr) {
  cat("  Processing", yr, "...\n")
  df_kp <- readRDS(paste0(basefolder, "Data/UCR known offense_2016_2024/offenses_known_monthly_", yr, ".rds"))
  df_kp <- df_kp %>%
    mutate(across(where(is.factor), as.character),
           followup_indication = as.character(followup_indication))
  cat("    Loaded", nrow(df_kp), "rows\n")
  return(df_kp)
})

df_kp <- bind_rows(ucr_list)


# -----------------------------------------------------------------------------
# 2. CONSTRUCT OFFENSE INDICATORS ----
# -----------------------------------------------------------------------------

df2 <- df_kp %>%
  filter(number_of_months_reported == 12) %>%
  filter(!is.na(fips_state_county_code), fips_state_county_code != "") %>%
  mutate(
    county_fips = fips_state_county_code,
    month_num       = match(tolower(month), tolower(month.name)),
    actual_homicide = actual_murder + actual_manslaughter,
    actual_rob_n_as_total  = actual_assault_total + actual_robbery_total,
    actual_rob_n_as_gun    = actual_assault_with_a_gun + actual_robbery_with_a_gun,
    actual_rob_n_as_kni    = actual_assault_with_a_knife + actual_robbery_with_a_knife,
    actual_rob_n_as_weapon =
      actual_assault_with_a_gun    + actual_robbery_with_a_gun +
      actual_assault_with_a_knife  + actual_robbery_with_a_knife +
      actual_assault_other_weapon  + actual_robbery_other_weapon
  ) %>%
  mutate(across(where(is.numeric), ~ifelse(is.finite(.), ., NA))) %>%
  select(
    ori, county_fips, agency_name, year, month, month_num, date, state_abb,
    number_of_months_reported, fips_state_county_code, population,
    contains("actual_homicide"),
    contains("actual_robbery"),
    contains("actual_assault"),
    contains("actual_rob_n_as")
  )


# -----------------------------------------------------------------------------
# 3. DIAGNOSE AND EXCLUDE MULTI-COUNTY AGENCIES ----
# -----------------------------------------------------------------------------
# Flag agencies whose reported population exceeds Census county population
# (indicates jurisdiction spans multiple counties)

county_pop <- get_decennial(
  geography = "county", variables = "P1_001N", year = 2020
) %>%
  mutate(fips_state_county_code = GEOID) %>%
  select(fips_state_county_code, census_pop = value)

df2 %>%
  group_by(fips_state_county_code, county_fips, agency_name) %>%
  summarise(max_pop = max(population, na.rm = TRUE), .groups = "drop") %>%
  left_join(county_pop, by = "fips_state_county_code") %>%
  mutate(pop_ratio = max_pop / census_pop) %>%
  filter(pop_ratio > 1.5) %>%
  arrange(desc(pop_ratio))

# Confirmed multi-county agencies excluded:
#   36061 (NY: New York City), 48301 (TX: Loving), 48229 (TX: Hudspeth), 48375 (TX: Amarillo)
problem_fips <- c("72127", "36061", "48301", "48229", "48375")

df2 <- df2 %>%
  filter(!fips_state_county_code %in% problem_fips)


# -----------------------------------------------------------------------------
# 4. COLLAPSE TO COUNTY-MONTH LEVEL ----
# -----------------------------------------------------------------------------

df3 <- df2 %>%
  filter(!is.na(fips_state_county_code), fips_state_county_code != "") %>%
  group_by(fips_state_county_code, year, month_num) %>%
  reframe(
    actual_homicide             = sum(actual_homicide,             na.rm = TRUE),
    actual_robbery_total        = sum(actual_robbery_total,        na.rm = TRUE),
    actual_robbery_with_a_gun   = sum(actual_robbery_with_a_gun,   na.rm = TRUE),
    actual_robbery_with_a_knife = sum(actual_robbery_with_a_knife, na.rm = TRUE),
    actual_robbery_other_weapon = sum(actual_robbery_other_weapon, na.rm = TRUE),
    actual_robbery_unarmed      = sum(actual_robbery_unarmed,      na.rm = TRUE),
    actual_assault_total        = sum(actual_assault_total,        na.rm = TRUE),
    actual_assault_with_a_gun   = sum(actual_assault_with_a_gun,   na.rm = TRUE),
    actual_assault_with_a_knife = sum(actual_assault_with_a_knife, na.rm = TRUE),
    actual_assault_other_weapon = sum(actual_assault_other_weapon, na.rm = TRUE),
    actual_assault_unarmed      = sum(actual_assault_unarmed,      na.rm = TRUE),
    actual_assault_simple       = sum(actual_assault_simple,       na.rm = TRUE),
    actual_assault_aggravated   = sum(actual_assault_aggravated,   na.rm = TRUE),
    actual_rob_n_as_total       = sum(actual_rob_n_as_total,       na.rm = TRUE),
    actual_rob_n_as_gun         = sum(actual_rob_n_as_gun,         na.rm = TRUE),
    actual_rob_n_as_kni         = sum(actual_rob_n_as_kni,         na.rm = TRUE),
    actual_rob_n_as_weapon      = sum(actual_rob_n_as_weapon,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ifelse(is.finite(.), ., NA)))

cat("  Final rows:", nrow(df3), "\n")

# -----------------------------------------------------------------------------
# 5. CONSTRUCT ITS TIME VARIABLES ----
# -----------------------------------------------------------------------------
# Post = March 2020 (COVID-19 onset)

finaldata <- df3 %>%
  rename(month = month_num, county_fips = fips_state_county_code) %>%
  mutate(
    date      = as.Date(paste(year, month, "01", sep = "-")),
    time      = as.numeric(date - min(date)) / 30.44,
    post      = ifelse(date >= as.Date("2020-03-01"), 1, 0),
    time_post = ifelse(post == 1, time - time[which(date == as.Date("2020-03-01"))[1]], 0)
  ) %>%
  arrange(county_fips, date)

cat("  Unique counties:", n_distinct(finaldata$county_fips), "\n")
cat("  Unique time points:", n_distinct(paste(finaldata$year, finaldata$month)), "\n")

# -----------------------------------------------------------------------------
# 7. FETCH ACS POPULATION DATA (2016–2024) ----
# -----------------------------------------------------------------------------

census_data <- map_dfr(2016:2024, function(yr) {
  cat("  Year:", yr, "\n")
  get_acs(geography = "county", variables = c(total_pop = "B01003_001"),
          year = yr, survey = "acs5", cache_table = TRUE) %>%
    mutate(year = yr)
})

census_wide <- census_data %>%
  select(GEOID, NAME, variable, estimate, year) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(county_fips = GEOID, county_name = NAME)


# -----------------------------------------------------------------------------
# 8. CONSTRUCT BALANCED PANEL AND FINAL DATASET ----
# -----------------------------------------------------------------------------
# Criteria: (1) population > 25,000; (2) complete 108 county-months (2016–2024)
# Includes: per-capita crime rates, lethality ratios (denominators 1–8)

finaldata_merged <- finaldata %>%
  left_join(census_wide, by = c("county_fips", "year")) %>%
  group_by(county_fips) %>%
  filter(mean(total_pop, na.rm = TRUE) > 25000) %>%
  filter(n_distinct(paste(year, month)) == 108) %>%
  ungroup() %>%
  mutate(across(starts_with("actual"), ~ifelse(. < 0, NA, .))) %>%
  mutate(
    actual_homicide_rate              = (actual_homicide              / total_pop) * 100000,
    actual_robbery_total_rate         = (actual_robbery_total         / total_pop) * 100000,
    actual_robbery_with_a_gun_rate    = (actual_robbery_with_a_gun    / total_pop) * 100000,
    actual_robbery_with_a_knife_rate  = (actual_robbery_with_a_knife  / total_pop) * 100000,
    actual_robbery_other_weapon_rate  = (actual_robbery_other_weapon  / total_pop) * 100000,
    actual_robbery_unarmed_rate       = (actual_robbery_unarmed       / total_pop) * 100000,
    actual_assault_total_rate         = (actual_assault_total         / total_pop) * 100000,
    actual_assault_with_a_gun_rate    = (actual_assault_with_a_gun    / total_pop) * 100000,
    actual_assault_with_a_knife_rate  = (actual_assault_with_a_knife  / total_pop) * 100000,
    actual_assault_other_weapon_rate  = (actual_assault_other_weapon  / total_pop) * 100000,
    actual_assault_unarmed_rate       = (actual_assault_unarmed       / total_pop) * 100000,
    actual_assault_simple_rate        = (actual_assault_simple        / total_pop) * 100000,
    actual_assault_aggravated_rate    = (actual_assault_aggravated    / total_pop) * 100000,
    actual_rob_n_as_total_rate        = (actual_rob_n_as_total        / total_pop) * 100000,
    actual_rob_n_as_gun_rate          = (actual_rob_n_as_gun          / total_pop) * 100000,
    actual_rob_n_as_kni_rate          = (actual_rob_n_as_kni          / total_pop) * 100000,
    actual_rob_n_as_weapon_rate       = (actual_rob_n_as_weapon       / total_pop) * 100000
  ) %>%
  # Lethality ratios: homicide / (homicide + denominator)
  # lethality8: agg assault + robbery total [MAIN DENOMINATOR]
  mutate(
    lethality1 = actual_homicide / (actual_homicide + actual_assault_aggravated),
    lethality2 = actual_homicide / (actual_homicide + actual_assault_with_a_gun),
    lethality3 = actual_homicide / (actual_homicide + actual_assault_with_a_gun + actual_assault_with_a_knife),
    lethality4 = actual_homicide / (actual_homicide + actual_assault_with_a_gun + actual_assault_with_a_knife + actual_assault_other_weapon),
    lethality5 = actual_homicide / (actual_homicide + actual_rob_n_as_gun),
    lethality6 = actual_homicide / (actual_homicide + actual_rob_n_as_gun + actual_rob_n_as_kni),
    lethality7 = actual_homicide / (actual_homicide + actual_rob_n_as_weapon),
    lethality8 = actual_homicide / (actual_homicide + actual_assault_aggravated + actual_robbery_total)
  )

cat("  Unique counties:", n_distinct(finaldata_merged$county_fips), "\n")
cat("  Unique time points:", n_distinct(paste(finaldata_merged$year, finaldata_merged$month)), "\n")


# -----------------------------------------------------------------------------
# 9. SAVE / LOAD FINAL DATASET ----
# -----------------------------------------------------------------------------

saveRDS(finaldata_merged,
        paste0(basefolder, "Data/UCR known offense_2016_2024/countylevel_consistent report_2016_2024 (known to police).RDS"))

finaldata_merged <- readRDS(paste0(basefolder, "Data/UCR known offense_2016_2024/countylevel_consistent report_2016_2024 (known to police).RDS"))


# -----------------------------------------------------------------------------
# 10. MAIN ANALYSIS: FIXED-EFFECTS POISSON ITS ----
# -----------------------------------------------------------------------------
# DV:     homicide count (integer)
# Offset: log(denominator variant 1–8)
# FE:     county (county_fips) + calendar month
# SE:     clustered by county_fips

final_reg_pois <- finaldata_merged %>%
  mutate(
    homicide_count = as.integer(round(actual_homicide)),
    # lethality8: agg assault + robbery total [MAIN SPECIFICATION]
    log_denom8 = log(actual_assault_aggravated + actual_robbery_total)
  ) %>%
  filter(!is.na(homicide_count))

# Run all eight denominator variants
pois_main <- fepois(homicide_count ~ time + post + time_post + offset(log_denom8) | month + county_fips,
                  data = filter(final_reg_pois, !is.na(log_denom8)), cluster = ~county_fips)

etable(pois_main)
exp(coef(pois_main))   # IRR for main specification

## 10-1. Figure helpers: observed / fitted / counterfactual trend -------------
# Counterfactual: post = 0, time_post = 0, offset replaced with pre-pandemic trend
# Equivalent to "no pandemic" for both homicide numerator and denominator

make_pois_fig <- function(model, denom_var, data) {
  cf_trend <- feols(as.formula(paste(denom_var, "~ time | county_fips + month")),
                    data = filter(data, post == 0, !is.infinite(.data[[denom_var]])),
                    cluster = ~county_fips)
  data %>%
    mutate(
      denom_cf       = predict(cf_trend, newdata = .),
      fitted         = predict(model, newdata = ., type = "response"),
      counterfactual = predict(model,
                               newdata = mutate(., post = 0, time_post = 0,
                                                !!denom_var := denom_cf),
                               type = "response")
    ) %>%
    group_by(date) %>%
    summarise(observed       = mean(homicide_count, na.rm = TRUE),
              fitted         = mean(fitted,         na.rm = TRUE),
              counterfactual = mean(counterfactual, na.rm = TRUE), .groups = "drop")
}

plot_pois_fig <- function(fig_data, title_label) {
  ggplot(fig_data, aes(x = date)) +
    geom_line(aes(y = observed,       color = "Observed"),       linewidth = 0.8) +
    geom_line(aes(y = fitted,         color = "Fitted"),         linewidth = 0.7) +
    geom_line(aes(y = counterfactual, color = "Counterfactual"), linewidth = 0.7, linetype = "dashed") +
    geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red", alpha = 0.6) +
    scale_color_manual(values = c("Observed" = "orange", "Fitted" = "steelblue", "Counterfactual" = "darkgreen")) +
    labs(title = title_label, x = NULL, y = "Homicide Count", color = NULL) +
    theme_minimal() +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = c(0, 0)) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10))
}

# Main figure: lethality8 (agg assault + robbery total)
plot_pois_fig(make_pois_fig(pois_main, "log_denom8", final_reg_pois), "")

ggsave(paste0(basefolder, "Figures/Extended Data Fig 9a.png"), 
       width = 10, height = 5, dpi = 300)
# =============================================================================
# END OF REPLICATION CODE
# =============================================================================