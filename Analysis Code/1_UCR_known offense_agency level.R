# =============================================================================
# REPLICATION CODE: Lethality of Crime and COVID-19 ----
# =============================================================================
# Description : Agency-month panel analysis of homicide lethality (2016–2024)
#               using Fixed-Effects Poisson interrupted time series (ITS)
# Data        : UCR Offenses Known Monthly (2016–2024), SHR
# Unit        : Agency-month (ori9)
# Author      : Heeyoung Lee
# Last Update : 2026-04-29 (YYYY-MM-DD)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACKAGES  ----
# -----------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyverse)
library(fixest)
library(gridExtra)
library(scales)
library(knitr)
library(kableExtra)


# -----------------------------------------------------------------------------
# 1. LOAD AND FILTER UCR OFFENSES KNOWN DATA (2016–2024) ----
# -----------------------------------------------------------------------------

basefolder <- "C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/"

cat("Loading UCR Offenses Known data (2016-2024)...\n")

years <- 2016:2024

ucr_list <- lapply(years, function(yr) {
  cat("  Processing", yr, "...\n")
  
  # Load RDS file for each year
  file_path <- paste0(basefolder, "Data/UCR known offense_2016_2024/offenses_known_monthly_", yr, ".rds")
  
  df_kp <- readRDS(file_path)
  
  # Convert problematic variables to character to avoid type conflicts
  df_kp <- df_kp %>%
    mutate(across(where(is.factor), as.character),
           followup_indication = as.character(followup_indication))
  
  cat("    Loaded", nrow(df_kp), "rows\n")
  return(df_kp)
})

# Combine all years
df_kp <- bind_rows(ucr_list)
names(df_kp)


# -----------------------------------------------------------------------------
# 2. CONSTRUCT OFFENSE INDICATORS ----
# -----------------------------------------------------------------------------

df_kp2 <- df_kp %>%
  # Agencies that report every month
  filter(number_of_months_reported == 12) %>%
  filter(!is.na(ori9), ori9 != "") %>%
  mutate(
    # Convert month name to numeric
    month_num = match(tolower(month), tolower(month.name)),
    # Homicide (murder + manslaughter)
    actual_homicide = actual_murder + actual_manslaughter,
    # Robbery and assault combined measures
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
    ori, ori9, agency_name, year, month, month_num, date, state_abb,
    number_of_months_reported, fips_state_county_code, population,
    contains("actual_homicide"),
    contains("actual_robbery"),
    contains("actual_assault"),
    contains("actual_rob_n_as")
  )

cat("  Final rows (agency-months):", nrow(df_kp2), "\n")
cat("  Unique ori9:", n_distinct(df_kp2$ori9), "\n")


# -----------------------------------------------------------------------------
# 3. CONSTRUCT BALANCED PANEL ----
# -----------------------------------------------------------------------------
# Criteria: (1) population > 25,000; (2) complete 108 agency-months (2016–2024)

finaldata_kp <- df_kp2 %>%
  filter(year >= 2016 & year <= 2024) %>%
  group_by(ori9) %>%
  filter(mean(population, na.rm = TRUE) > 25000) %>%
  filter(n_distinct(paste(year, month_num)) == 108) %>%
  ungroup() %>%
  mutate(
    date = as.Date(sprintf("%d-%02d-01", as.integer(year), as.integer(month_num)))
  ) %>%
  mutate(
    # ITS time variables: time in months from panel start
    # Post = March 2020 (COVID-19 onset)
    time      = as.numeric(date - min(date)) / 30.44,
    post      = ifelse(date >= as.Date("2020-03-01"), 1, 0),
    time_post = ifelse(post == 1, time - time[date == as.Date("2020-03-01")][1], 0)
  ) %>%
  arrange(ori9, date)

# Verify balance
cat("  Final rows (agency-months):", nrow(finaldata_kp), "\n")
cat("  Unique ori9:", n_distinct(finaldata_kp$ori9), "\n")
cat("  Unique time points:", n_distinct(paste(finaldata_kp$year, finaldata_kp$month)), "\n")

# Visual balance check
finaldata_kp %>%
  group_by(date) %>%
  summarise(n_agencies = n_distinct(ori9)) %>%
  ggplot(aes(x = date, y = n_agencies)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
  labs(title = "Number of Agencies per Month", x = NULL, y = "N Agencies") +
  theme_minimal()


# -----------------------------------------------------------------------------
# 4. CONSTRUCT FINAL ANALYSIS DATASET ----
# -----------------------------------------------------------------------------
# Includes: per-capita crime rates, lethality ratios (denominators 1–8)

finaldata_kp_merged <- finaldata_kp %>%
  filter(!is.na(actual_homicide)) %>%
  filter(population > 0) %>%
  mutate(across(starts_with("actual"), ~ifelse(. < 0, NA, .))) %>%   # Recode negatives to NA
  mutate(
    actual_homicide_rate              = (actual_homicide              / population) * 100000,
    actual_robbery_total_rate         = (actual_robbery_total         / population) * 100000,
    actual_robbery_with_a_gun_rate    = (actual_robbery_with_a_gun    / population) * 100000,
    actual_robbery_with_a_knife_rate  = (actual_robbery_with_a_knife  / population) * 100000,
    actual_robbery_other_weapon_rate  = (actual_robbery_other_weapon  / population) * 100000,
    actual_robbery_unarmed_rate       = (actual_robbery_unarmed       / population) * 100000,
    actual_assault_total_rate         = (actual_assault_total         / population) * 100000,
    actual_assault_with_a_gun_rate    = (actual_assault_with_a_gun    / population) * 100000,
    actual_assault_with_a_knife_rate  = (actual_assault_with_a_knife  / population) * 100000,
    actual_assault_other_weapon_rate  = (actual_assault_other_weapon  / population) * 100000,
    actual_assault_unarmed_rate       = (actual_assault_unarmed       / population) * 100000,
    actual_assault_simple_rate        = (actual_assault_simple        / population) * 100000,
    actual_assault_aggravated_rate    = (actual_assault_aggravated    / population) * 100000,
    actual_rob_n_as_total_rate        = (actual_rob_n_as_total        / population) * 100000,
    actual_rob_n_as_gun_rate          = (actual_rob_n_as_gun          / population) * 100000,
    actual_rob_n_as_kni_rate          = (actual_rob_n_as_kni          / population) * 100000,
    actual_rob_n_as_weapon_rate       = (actual_rob_n_as_weapon       / population) * 100000
  ) %>%
  # Lethality ratios: homicide / (homicide + denominator)
  # lethality1: agg assault
  # lethality2: gun assault
  # lethality3: gun + knife assault
  # lethality4: gun + knife + other weapon assault
  # lethality5: gun (rob + assault)
  # lethality6: gun + knife (rob + assault)
  # lethality7: any weapon (rob + assault)
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

cat("  Final rows (agency-months):", nrow(finaldata_kp_merged), "\n")
cat("  Unique ori9:", n_distinct(finaldata_kp_merged$ori9), "\n")


# -----------------------------------------------------------------------------
# 5. SAVE / LOAD FINAL DATASET ----
# -----------------------------------------------------------------------------

saveRDS(finaldata_kp_merged, paste0(basefolder, "Data/UCR known offense_2016_2024/agencylevel_balanced panel_2016_2024 (known to pol).RDS"))
finaldata_kp_merged <- readRDS(paste0(basefolder, "Data/UCR known offense_2016_2024/agencylevel_balanced panel_2016_2024 (known to pol).RDS"))

# -----------------------------------------------------------------------------
# 6. NATIONAL-LEVEL DESCRIPTIVE STATISTICS ----
# -----------------------------------------------------------------------------

covid_date <- as.Date("2020-03-01")

## 6-1. Sample population coverage (% of total US population by year) ---------

sample_pop <- finaldata_kp_merged %>%
  group_by(year) %>%
  summarise(sample_pop = sum(population, na.rm = TRUE) / 12,   # monthly -> annual
            .groups = "drop")

us_pop_manual <- tibble(
  year   = 2016:2024,
  us_pop = c(323127513, 325719178, 327167434, 328239523,
             331449281, 332403650, 334914895, 336997624,
             340110988)
)

sample_pop %>%
  left_join(us_pop_manual, by = "year") %>%
  mutate(pct_covered = (sample_pop / us_pop) * 100)


## 6-2. Monthly trends: violent crime rate, homicide rate, lethality, COVID deaths ----

# COVID-19 monthly deaths (US, CDC)
covid_deaths <- tribble(
  ~date_str, ~deaths,
  "2020-01",6,"2020-02",25,"2020-03",3912,"2020-04",59266,"2020-05",47130,"2020-06",16991,
  "2020-07",25791,"2020-08",35592,"2020-09",18234,"2020-10",27304,"2020-11",48413,"2020-12",85259,
  "2021-01",120763,"2021-02",50130,"2021-03",21853,"2021-04",17614,"2021-05",17919,"2021-06",7827,
  "2021-07",12091,"2021-08",42347,"2021-09",60612,"2021-10",50891,"2021-11",29699,"2021-12",39280,
  "2022-01",88375,"2022-02",53982,"2022-03",16099,"2022-04",7653,"2022-05",6799,"2022-06",8680,
  "2022-07",14596,"2022-08",12888,"2022-09",10798,"2022-10",11157,"2022-11",9200,"2022-12",15784,
  "2023-01",13668,"2023-02",9195,"2023-03",7200,"2023-04",6152,"2023-05",3160,"2023-06",1741
) %>% mutate(date = as.Date(paste0(date_str, "-01")))

# Aggregate to national monthly level
plot_lethality <- finaldata_kp_merged %>%
  group_by(year, month_num) %>%
  summarise(actual_homicide           = sum(actual_homicide,           na.rm = TRUE),
            actual_assault_aggravated = sum(actual_assault_aggravated, na.rm = TRUE),
            actual_robbery_total      = sum(actual_robbery_total,      na.rm = TRUE),
            population                = sum(population,                 na.rm = TRUE), .groups = "drop") %>%
  mutate(date          = as.Date(paste(year, month_num, "01", sep = "-")),
         assault_rate8 = (actual_assault_aggravated + actual_robbery_total) / population * 100000,
         homicide_rate = actual_homicide / population * 100000,
         lethality8    = actual_homicide / (actual_homicide + actual_assault_aggravated + actual_robbery_total)) %>%
  filter(date >= as.Date("2016-01-01"), date <= as.Date("2024-12-01"))

plot_covid <- tibble(date = seq(as.Date("2016-01-01"), as.Date("2024-12-01"), by = "month")) %>%
  left_join(covid_deaths, by = "date") %>%
  mutate(deaths = replace_na(deaths, 0))

# Shared plot elements: COVID wave shading and base theme
phase_shading <- list(
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-06-30"), ymin = -Inf, ymax = Inf, fill = "tomato", alpha = 0.15),
  annotate("rect", xmin = as.Date("2020-10-01"), xmax = as.Date("2021-03-31"), ymin = -Inf, ymax = Inf, fill = "tomato", alpha = 0.15),
  annotate("rect", xmin = as.Date("2021-07-01"), xmax = as.Date("2022-02-28"), ymin = -Inf, ymax = Inf, fill = "tomato", alpha = 0.15)
)

base_theme <- list(
  theme_bw(),
  theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = c(0, 0)),
  geom_vline(xintercept = covid_date, linetype = "dashed", color = "red", alpha = 0.7)
)

# Figure 1: four-panel trend plot
pa <- ggplot(plot_lethality, aes(x = date, y = assault_rate8)) + phase_shading + base_theme +
  geom_line() + scale_y_continuous(labels = comma) +
  labs(title = "(A) Robbery and Aggravated Assault", x = "", y = "Rate (per 100,000)")

pb <- ggplot(plot_lethality, aes(x = date, y = homicide_rate)) + phase_shading + base_theme +
  geom_line() + scale_y_continuous(labels = comma) +
  labs(title = "(B) Homicide", x = "", y = "Rate (per 100,000)")

pc <- ggplot(plot_lethality, aes(x = date, y = lethality8*1000)) + phase_shading + base_theme +
  geom_line() + labs(title = "(C) Lethality", x = "", y = "Rate (per 1,000)")

pd <- ggplot(filter(plot_covid, date <= as.Date("2023-06-01")), aes(x = date, y = deaths)) +
  phase_shading + base_theme + geom_line() + scale_y_continuous(labels = comma) +
  labs(title = "(D) COVID-19 Deaths", x = "", y = "COVID-19 Deaths")

p_final <- grid.arrange(pa, pb, pc, pd, nrow = 2)
ggsave(paste0(basefolder, "Figures/Fig 1.png"), 
       p_final, width = 10, height = 10, dpi = 300)


## 6-3. Pre/post comparison: three-month windows around COVID onset ------------
# Window 1 (Pre):  Dec 2019 – Feb 2020
# Window 2 (Post): Dec 2020 – Feb 2021

winter_compare <- plot_lethality %>%
  filter(
    (year == 2019 & month_num == 12) |
      (year == 2020 & month_num %in% c(1, 2, 12)) |
      (year == 2021 & month_num %in% c(1, 2))
  ) %>%
  mutate(window = case_when(
    (year == 2019 & month_num == 12) | (year == 2020 & month_num %in% c(1, 2)) ~ "Pre (Dec19-Feb20)",
    (year == 2020 & month_num == 12) | (year == 2021 & month_num %in% c(1, 2)) ~ "Post (Dec20-Feb21)"
  )) %>%
  group_by(window) %>%
  summarise(
    homicides = sum(actual_homicide,                                  na.rm = TRUE),
    assaults  = sum(actual_assault_aggravated + actual_robbery_total, na.rm = TRUE),
    lethality = mean(lethality8, na.rm = TRUE) * 1000,
    one_in_x  = 1000 / mean(lethality8 * 1000, na.rm = TRUE),
    .groups = "drop"
  )

print(winter_compare)


# -----------------------------------------------------------------------------
# 7. MAIN ANALYSIS: FIXED-EFFECTS POISSON ITS ----
# -----------------------------------------------------------------------------
# Model: fepois(homicide_count ~ time + post + time_post + offset(log_denom) | month + ori9)
# FE:    agency (ori9) + calendar month
# SE:    clustered by ori9
# Offset denominator variants: log_denom1 to log_denom8

final_reg_pois <- finaldata_kp_merged %>%
  mutate(
    homicide_count = as.integer(round(actual_homicide)),
    # lethality1: agg assault
    log_denom1 = log(actual_assault_aggravated),
    # lethality2: gun assault
    log_denom2 = log(actual_assault_with_a_gun),
    # lethality3: gun + knife assault
    log_denom3 = log(actual_assault_with_a_gun + actual_assault_with_a_knife),
    # lethality4: gun + knife + other weapon assault
    log_denom4 = log(actual_assault_with_a_gun + actual_assault_with_a_knife + actual_assault_other_weapon),
    # lethality5: gun (rob + assault)
    log_denom5 = log(actual_rob_n_as_gun),
    # lethality6: gun + knife (rob + assault)
    log_denom6 = log(actual_rob_n_as_gun + actual_rob_n_as_kni),
    # lethality7: any weapon (rob + assault)
    log_denom7 = log(actual_rob_n_as_weapon),
    # lethality8: agg assault + robbery total [MAIN SPECIFICATION]
    log_denom8 = log(actual_assault_aggravated + actual_robbery_total)
  )

# Main model (lethality8: agg assault + robbery total)
pois_main <- fepois(homicide_count ~ time + post + time_post + offset(log_denom8) | month + ori9,
                    data = filter(final_reg_pois, !is.na(log_denom8)), cluster = ~ori9)

etable(pois_main)
exp(coef(pois_main))   # IRR


## 7-1. Observed vs. counterfactual homicide counts at COVID onset -------------

final_reg_pois %>%
  filter(date == as.Date("2020-03-01")) %>%
  summarise(
    observed       = sum(homicide_count, na.rm = TRUE),
    counterfactual = sum(predict(pois_main,
                                 newdata = mutate(., post = 0, time_post = 0),
                                 type = "response"), na.rm = TRUE)
  ) %>%
  mutate(excess = observed - counterfactual)


## 7-2. Figure helpers: observed / fitted / counterfactual trend --------------

make_pois_fig <- function(model, denom_var, data) {
  cf <- feols(as.formula(paste(denom_var, "~ time | ori9 + month")),
              data = filter(data, post == 0, !is.infinite(.data[[denom_var]])), cluster = ~ori9)
  data %>%
    mutate(
      fitted         = predict(model, newdata = ., type = "response"),
      counterfactual = predict(model, type = "response",
                               newdata = mutate(., post = 0, time_post = 0,
                                                !!denom_var := predict(cf, newdata = .)))
    ) %>%
    group_by(date) %>%
    summarise(observed      = mean(homicide_count, na.rm = TRUE),
              fitted        = mean(fitted,         na.rm = TRUE),
              counterfactual = mean(counterfactual, na.rm = TRUE), .groups = "drop")
}

plot_pois_fig <- function(fig_data, title_label) {
  ggplot(fig_data, aes(x = date)) +
    geom_line(aes(y = observed,       color = "Observed"),  linewidth = 0.8) +
    geom_line(aes(y = fitted,         color = "Fitted"),    linewidth = 0.7) +
    geom_line(aes(y = counterfactual, color = "Pretrend"),  linewidth = 0.7, linetype = "dashed") +
    geom_vline(xintercept = covid_date, linetype = "dashed", color = "red", alpha = 0.6) +
    scale_color_manual(values = c("Observed" = "orange", "Fitted" = "steelblue", "Pretrend" = "darkgreen")) +
    labs(title = title_label, x = NULL, y = "Homicide Count", color = NULL) +
    theme_minimal() +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = c(0, 0)) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10))
}

main_plot <- plot_pois_fig(make_pois_fig(pois_main, "log_denom8", final_reg_pois), "")
main_plot
ggsave(paste0(basefolder, "Figures/Fig 2.png"), 
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 8. DECOMPOSITION: LETHALITY EFFECT vs. VIOLENCE RATE EFFECT ----
# -----------------------------------------------------------------------------
# Kitagawa-style decomposition of homicide rate change into:
#   (1) lethality effect: change in L holding V at midpoint
#   (2) violence effect:  change in V holding L at midpoint

final_reg_kp <- finaldata_kp_merged

monthly_stats <- final_reg_kp %>%
  mutate(year_mon  = format(date, "%Y-%m"),
         assault_i = actual_assault_aggravated + actual_robbery_total) %>%
  group_by(year_mon) %>%
  summarise(homicides = sum(actual_homicide, na.rm = TRUE),
            assaults  = sum(assault_i,       na.rm = TRUE),
            pop       = sum(population,       na.rm = TRUE), .groups = "drop") %>%
  mutate(
    H = (homicides / pop) * 100000,
    V = ((homicides + assaults) / pop) * 100000,
    L = if_else((homicides + assaults) == 0, 0, homicides / (homicides + assaults))
  )

# Baseline: pre-COVID monthly averages
baseline_m <- monthly_stats %>%
  filter(year_mon < "2020-03") %>%
  summarise(H0 = mean(H, na.rm = TRUE),
            V0 = mean(V, na.rm = TRUE),
            L0 = mean(L, na.rm = TRUE))

# Decomposition: midpoint method
decomp_df <- monthly_stats %>%
  mutate(
    year_mon         = as.Date(paste0(year_mon, "-01")),
    L_mid            = (L + baseline_m$L0) / 2,
    V_mid            = (V + baseline_m$V0) / 2,
    lethality_effect = (L - baseline_m$L0) * V_mid,
    violence_effect  = (V - baseline_m$V0) * L_mid,
    lethality_pct    = (lethality_effect / baseline_m$H0) * 100,
    violence_pct     = (violence_effect  / baseline_m$H0) * 100,
    total_pct        = lethality_pct + violence_pct
  )

print(decomp_df)

# Figure: stacked bar chart of decomposition
decomp_df %>%
  select(year_mon, violence_pct, lethality_pct) %>%
  pivot_longer(-year_mon, names_to = "component", values_to = "pct") %>%
  mutate(component = factor(component, levels = c("violence_pct", "lethality_pct"),
                            labels = c("Violent Crime Rate", "Lethality"))) %>%
  ggplot(aes(x = year_mon, y = pct, fill = component)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = covid_date, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = c(0, 0)) +
  scale_fill_manual(values = c("Violent Crime Rate" = "blue4", "Lethality" = "red4")) +
  labs(x = "Month", y = "Homicide Rate Change (% of Baseline)", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 10), plot.margin = margin(t = 5, r = 5, b = 30, l = 5))

ggsave(paste0(basefolder, "Figures/Fig 3.png"), 
       width = 10, height = 6, dpi = 300)
# =============================================================================
# APPENDIX ----
# =============================================================================


# -----------------------------------------------------------------------------
# A1. EVENT STUDY ----
# -----------------------------------------------------------------------------
# Relative-time Poisson model; ref = -1 (month prior to COVID onset)
# Event time bounded at [-48, +48] months

final_reg_es <- final_reg_pois %>%
  mutate(event_time = pmin(pmax((year(date) - 2020) * 12 + (month(date) - 2), -48), 48))

es_pois <- fepois(homicide_count ~ i(event_time, ref = -1) + offset(log_denom8) | month + ori9,
                  cluster = ~ori9, data = filter(final_reg_es, !is.na(log_denom8)))

es_df <- as.data.frame(summary(es_pois)$coeftable) %>%
  rownames_to_column("term") %>%
  rename(estimate = Estimate, se = `Std. Error`) %>%
  mutate(event_time = as.integer(gsub("event_time::", "", term)),
         conf.low   = estimate - 1.96 * se,
         conf.high  = estimate + 1.96 * se) %>%
  filter(!is.na(event_time))

es_df %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "steelblue3", alpha = 0.5, width = 0, linewidth = 2) +
  geom_point(color = "black", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0,  linetype = "dashed", color = "red", alpha = 0.8) +
  labs(x = "Months relative to pandemic onset", y = "Coefficient (log IRR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))

ggsave(paste0(basefolder, "Figures/Extended Data Fig 1.png"), 
       width = 10, height = 6, dpi = 300)
# -----------------------------------------------------------------------------
# A2. PLACEBO TESTS ----
# -----------------------------------------------------------------------------
# Iterates over placebo break dates (Jan 2017 – Feb 2020) in the pre-COVID sample
# Reports post coefficient with 95% CI for each placebo date

break_dates <- seq.Date(as.Date("2017-01-01"), as.Date("2020-02-01"), by = "month")

placebo <- lapply(break_dates, function(bd) {
  dat <- final_reg_pois %>%
    filter(!is.na(log_denom8), date < covid_date) %>%
    mutate(post_alt      = if_else(date >= bd, 1L, 0L),
           time_post_alt = if_else(date >= bd, (year(date) - year(bd)) * 12 + (month(date) - month(bd)), 0L))
  m <- tryCatch(
    fepois(homicide_count ~ time + post_alt + time_post_alt + offset(log_denom8) | month + ori9,
           cluster = ~ori9, data = dat),
    error = function(e) NULL, warning = function(w) NULL)
  if (is.null(m) || !"post_alt" %in% names(coef(m))) return(NULL)
  data.frame(break_date = bd, estimate  = coef(m)["post_alt"],
             conf_low   = coef(m)["post_alt"] - 1.96 * se(m)["post_alt"],
             conf_high  = coef(m)["post_alt"] + 1.96 * se(m)["post_alt"])
}) %>% bind_rows()

placebo %>%
  ggplot(aes(x = break_date, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 10, linewidth = 0.5) +
  geom_point(size = 1.5) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(subtitle = "", x = "", y = "Coefficient / log IRR (Post)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), panel.grid.minor = element_blank())

ggsave(paste0(basefolder, "Figures/Extended Data Fig 2.png"), 
       width = 10, height = 6, dpi = 300)
# -----------------------------------------------------------------------------
# A3. ROBUSTNESS: INVERSE PROBABILITY WEIGHTING (1/population) ----
# -----------------------------------------------------------------------------

pois_ipw <- fepois(homicide_count ~ time + post + time_post + offset(log_denom8) | month + ori9,
                   data    = filter(final_reg_pois, !is.na(log_denom8)),
                   cluster = ~ori9, weights = ~I(1 / population))

etable(pois_ipw)

p_ipw <- plot_pois_fig(make_pois_fig(pois_ipw, "log_denom8", final_reg_pois), "")
p_ipw

ggsave(paste0(basefolder, "Figures/Extended Data Fig 3.png"), 
       width = 10, height = 5, dpi = 300)
# -----------------------------------------------------------------------------
# A4. ROBUSTNESS: LARGE-CITY SUBSAMPLE (population > 100,000) ----
# -----------------------------------------------------------------------------

pois_popth <- fepois(homicide_count ~ time + post + time_post + offset(log_denom8) | month + ori9,
                     data    = filter(final_reg_pois, !is.na(log_denom8), population > 100000),
                     cluster = ~ori9)

etable(pois_popth)

p8_popth <- plot_pois_fig(make_pois_fig(pois_popth, "log_denom8",
                                        filter(final_reg_pois, population > 100000)),
                          "")
p8_popth
ggsave(paste0(basefolder, "Figures/Extended Data Fig 4.png"), 
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# A5. ROBUSTNESS: STATE- AND REGION-STRATIFIED MODELS ----
# -----------------------------------------------------------------------------

final_reg_pois <- final_reg_pois %>%
  mutate(
    state_abb = substr(ori9, 1, 2),
    region    = case_when(
      state_abb %in% c("CT","ME","MA","NH","RI","VT","NJ","NY","PA") ~ "Northeast",
      state_abb %in% c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD") ~ "Midwest",
      state_abb %in% c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS","TN","AR","LA","OK","TX") ~ "South",
      state_abb %in% c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA") ~ "West"
    )
  )

# Helper: run main ITS model separately by group
run_stratified <- function(data, group_var) {
  group_nm <- deparse(substitute(group_var))
  
  data %>%
    filter(!is.na(log_denom8), !is.na({{ group_var }})) %>%
    group_by({{ group_var }}) %>%
    group_split() %>%
    lapply(function(d) {
      m <- tryCatch(
        fepois(homicide_count ~ time + post + time_post + offset(log_denom8) | month + ori9,
               cluster = ~ori9, data = d),
        error = function(e) NULL)
      if (is.null(m) || !"post" %in% names(coef(m))) return(NULL)
      data.frame(group     = d[[group_nm]][1],
                 estimate  = coef(m)["post"],
                 conf_low  = coef(m)["post"] - 1.96 * se(m)["post"],
                 conf_high = coef(m)["post"] + 1.96 * se(m)["post"])
    }) %>%
    bind_rows() %>%
    mutate(significant = !(conf_low < 0 & conf_high > 0)) %>%
    arrange(estimate)
}

coef_state  <- run_stratified(final_reg_pois, state_abb)
coef_region <- run_stratified(final_reg_pois, region)

# Helper: forest plot for stratified results
plot_stratified <- function(coef_df) {
  ggplot(coef_df, aes(x = reorder(group, estimate), y = estimate, color = significant)) +
    geom_point(size = 2.3) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), size = 2, width = 0, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "gray70")) +
    labs(title = "", y = "log IRR (Post)", x = NULL) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}

plot_stratified(coef_state)
plot_stratified(coef_region)

combined_plot <- grid.arrange(
  plot_stratified(coef_state),
  plot_stratified(coef_region),
  ncol = 1
)

ggsave(paste0(basefolder, "Figures/Extended Data Fig 5.png"), 
       combined_plot, width = 10, height = 10, dpi = 300)

# -----------------------------------------------------------------------------
# A6. ROBUSTNESS: OLS ON LETHALITY RATIO (deprecated main spec) ----
# -----------------------------------------------------------------------------
# DV: lethality8 (ratio); FE: month + ori9; SE: clustered by ori9
# Note: Poisson ITS (Section 7) is the preferred specification

ols <- feols(lethality8 ~ time + post + time_post | month + ori9, data = final_reg_kp, cluster = ~ori9)
etable(ols)

p_ols <- plot_pois_fig(
  final_reg_kp %>%
    mutate(fitted         = predict(ols, newdata = .),
           counterfactual = predict(ols, newdata = mutate(., post = 0, time_post = 0))) %>%
    group_by(date) %>%
    summarise(observed       = mean(lethality8, na.rm = TRUE),
              fitted         = mean(fitted,      na.rm = TRUE),
              counterfactual = mean(counterfactual, na.rm = TRUE), .groups = "drop"),
  "")
p_ols

ggsave(paste0(basefolder, "Figures/Extended Data Fig 6.png"), 
       width = 10, height = 5, dpi = 300)
# -----------------------------------------------------------------------------
# A7. ROBUSTNESS: COUNTERFACTUAL DENOMINATOR ----
# -----------------------------------------------------------------------------
# Post-COVID denominator replaced with agency-specific pre-COVID seasonal averages
# to isolate the lethality effect from denominator changes

pre_avg <- final_reg_pois %>%
  filter(date < covid_date) %>%
  group_by(ori9, month_num) %>%
  summarise(avg_denom8 = mean(exp(log_denom8), na.rm = TRUE), .groups = "drop")

final_reg_cf <- final_reg_pois %>%
  left_join(pre_avg, by = c("ori9", "month_num")) %>%
  mutate(log_denom8_cf = if_else(date >= covid_date & avg_denom8 > 0, log(avg_denom8), log_denom8))

cf_pois_main <- fepois(homicide_count ~ time + post + time_post + offset(log_denom8_cf) | month + ori9,
                       data = filter(final_reg_cf, !is.na(log_denom8_cf)), cluster = ~ori9)

etable(cf_pois_main)

p8_cf <- plot_pois_fig(make_pois_fig(cf_pois_main, "log_denom8_cf", final_reg_cf), "")
p8_cf

ggsave(paste0(basefolder, "Figures/Extended Data Fig 7.png"), 
       width = 10, height = 5, dpi = 300)
# -----------------------------------------------------------------------------
# A8. ROBUSTNESS: ALTERNATIVE DENOMINATOR (lethality7: any weapon, rob + assault) ----
# -----------------------------------------------------------------------------

pois_alternative <- fepois(homicide_count ~ time + post + time_post + offset(log_denom7) | month + ori9,
                           data = filter(final_reg_pois, !is.na(log_denom7)), cluster = ~ori9)

etable(pois_alternative)

main_plot <- plot_pois_fig(make_pois_fig(pois_alternative, "log_denom7", final_reg_pois), "")
main_plot

ggsave(paste0(basefolder, "Figures/Extended Data Fig 8.png"), 
       width = 10, height = 5, dpi = 300)
# =============================================================================
# END OF REPLICATION CODE ----
# =============================================================================