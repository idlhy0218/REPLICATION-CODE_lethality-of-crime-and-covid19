# =============================================================================
# REPLICATION CODE: Lethality of Crime and COVID-19 — UCR Arrest Data ----
# =============================================================================
# Description : Agency-month panel analysis using UCR Monthly Arrests (2016–2024)
#               Parallel to main analysis (1_UCR_known offense_agency.R)
#               but uses arrest counts as denominators for lethality measures
# Data        : UCR Monthly Arrests (2016–2024), UCR Offenses Known (balanced panel)
# Unit        : Agency-month (ori9)
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


# -----------------------------------------------------------------------------
# 1. LOAD AND FILTER UCR ARRESTS DATA (2016–2024) ----
# -----------------------------------------------------------------------------
basefolder <- "C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/"

# Offenses retained: homicide, manslaughter, aggravated assault, other assault, robbery

years <- 2016:2024

ucr_list <- lapply(years, function(yr) {
  cat("  Processing", yr, "...\n")
  df <- readRDS(paste0(basefolder, "Data/UCR arrest_2016_2024/arrests_monthly_", yr, ".rds"))
  df <- df %>%
    filter(offense_code %in% c("murder and nonnegligent manslaughter",
                               "negligent manslaughter",
                               "aggravated assault",
                               "other assault",
                               "robbery"))
  cat("    Kept", nrow(df), "rows\n")
  return(df)
})

df <- bind_rows(ucr_list)
cat("  Final rows (agency-months):", nrow(df), "\n")
cat("  Unique ori9:", n_distinct(df$ori9), "\n")


# -----------------------------------------------------------------------------
# 2. CONSTRUCT OFFENSE INDICATORS ----
# -----------------------------------------------------------------------------

df2 <- df %>%
  filter(number_of_months_reported == 12) %>%
  mutate(
    homicide_ucr       = if_else(offense_code %in% c("murder and nonnegligent manslaughter",
                                                     "negligent manslaughter"), total_arrests, NA_real_),
    robbery_ucr        = if_else(offense_code == "robbery",           total_arrests, NA_real_),
    assault_simple_ucr = if_else(offense_code == "other assault",     total_arrests, NA_real_),
    assault_aggra_ucr  = if_else(offense_code == "aggravated assault", total_arrests, NA_real_),
    month_num          = match(tolower(month), tolower(month.name))
  )


# -----------------------------------------------------------------------------
# 3. COLLAPSE TO AGENCY-MONTH LEVEL ----
# -----------------------------------------------------------------------------

df3 <- df2 %>%
  filter(!is.na(ori9)) %>%
  group_by(ori9, year, month_num) %>%
  reframe(
    homicide_ucr       = sum(homicide_ucr,       na.rm = TRUE),
    robbery_ucr        = sum(robbery_ucr,         na.rm = TRUE),
    assault_simple_ucr = sum(assault_simple_ucr,  na.rm = TRUE),
    assault_aggra_ucr  = sum(assault_aggra_ucr,   na.rm = TRUE),
    population_ucr     = mean(population,          na.rm = TRUE)
  )

cat("  Final rows (agency-months):", nrow(df3), "\n")
cat("  Unique ori9:", n_distinct(df3$ori9), "\n")


# -----------------------------------------------------------------------------
# 4. LOAD UCR OFFENSES KNOWN DATA (homicide counts from balanced panel) ----
# -----------------------------------------------------------------------------

finaldata_kp_merged <- readRDS(paste0(basefolder, "Data/UCR known offense_2016_2024/agencylevel_balanced panel_2016_2024 (known to pol).RDS")) %>%
  filter(year >= 2016 & year <= 2024) %>%
  select(ori9, year, month_num, actual_homicide)


# -----------------------------------------------------------------------------
# 5. MERGE ARREST AND OFFENSES KNOWN DATA ----
# -----------------------------------------------------------------------------

df4 <- df3 %>%
  left_join(finaldata_kp_merged, by = c("ori9", "year", "month_num"))

cat("  Final rows (agency-months):", nrow(df4), "\n")
cat("  Unique ori9:", n_distinct(df4$ori9), "\n")


# -----------------------------------------------------------------------------
# 6. CONSTRUCT BALANCED PANEL ----
# -----------------------------------------------------------------------------
# Criteria: (1) population > 25,000; (2) complete 108 agency-months (2016–2024)

finaldata <- df4 %>%
  filter(year >= 2016 & year <= 2024) %>%
  rename(month = month_num) %>%
  group_by(ori9) %>%
  filter(mean(population_ucr, na.rm = TRUE) > 25000) %>%
  filter(n_distinct(paste(year, month)) == 108) %>%
  ungroup() %>%
  mutate(
    date      = as.Date(paste(year, month, "01", sep = "-")),
    # ITS time variables: months since Jan 2016; post = March 2020
    time      = as.numeric(date - min(date)) / 30.44,
    post      = ifelse(date >= as.Date("2020-03-01"), 1, 0),
    time_post = ifelse(post == 1, time - time[which(date == as.Date("2020-03-01"))[1]], 0)
  ) %>%
  arrange(ori9, date)

cat("  Final rows (agency-months):", nrow(finaldata), "\n")
cat("  Unique ori9:", n_distinct(finaldata$ori9), "\n")
cat("  Unique time points:", n_distinct(paste(finaldata$year, finaldata$month)), "\n")

# Visual balance check
finaldata %>%
  group_by(date) %>%
  summarise(n_agencies = n_distinct(ori9)) %>%
  ggplot(aes(x = date, y = n_agencies)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
  labs(title = "Number of Agencies per Month", x = NULL, y = "N Agencies") +
  theme_minimal()


# -----------------------------------------------------------------------------
# 7. CONSTRUCT FINAL ANALYSIS DATASET ----
# -----------------------------------------------------------------------------
# Recode negatives to NA; compute lethality ratio (arrest-based denominator)
# lethality1: homicide / (homicide + aggravated assault + robbery), UCR arrest

finaldata_merged <- finaldata %>%
  mutate(
    homicide_ucr       = ifelse(homicide_ucr       < 0, NA, homicide_ucr),
    actual_homicide    = ifelse(actual_homicide    < 0, NA, actual_homicide),
    assault_simple_ucr = ifelse(assault_simple_ucr < 0, NA, assault_simple_ucr),
    assault_aggra_ucr  = ifelse(assault_aggra_ucr  < 0, NA, assault_aggra_ucr),
    robbery_ucr        = ifelse(robbery_ucr        < 0, NA, robbery_ucr),
    lethality1         = actual_homicide / (actual_homicide + assault_aggra_ucr + robbery_ucr)
  )

cat("  Final rows (agency-months):", nrow(finaldata_merged), "\n")
cat("  Unique ori9:", n_distinct(finaldata_merged$ori9), "\n")
cat("  Unique time points:", n_distinct(paste(finaldata_merged$year, finaldata_merged$month)), "\n")


# -----------------------------------------------------------------------------
# 8. SAVE / LOAD FINAL DATASET ----
# -----------------------------------------------------------------------------

saveRDS(finaldata_merged,
        paste0(basefolder, "Data/UCR arrest_2016_2024/agencylevel_balanced panel_2016_2024 (arrest).RDS"))

finaldata_merged <- readRDS(paste0(basefolder, "Data/UCR arrest_2016_2024/agencylevel_balanced panel_2016_2024 (arrest).RDS")) %>%
  filter(year >= 2016 & year <= 2024)
  


# -----------------------------------------------------------------------------
# 9. MAIN ANALYSIS: FIXED-EFFECTS POISSON ITS ----
# -----------------------------------------------------------------------------
# DV:     homicide count (integer)
# Offset: log(aggravated assault arrests + robbery arrests)
# FE:     agency (ori9) + calendar month
# SE:     clustered by ori9

final_reg_pois <- finaldata_merged %>%
  mutate(
    homicide_count = as.integer(round(actual_homicide)),
    log_assault1   = log(assault_aggra_ucr + robbery_ucr)
  ) %>%
  filter(!is.na(homicide_count), !is.na(log_assault1), population_ucr > 0)

pois_m1 <- fepois(homicide_count ~ time + post + time_post + offset(log_assault1) | month + ori9,
                  data = final_reg_pois, cluster = ~ori9)

etable(pois_m1)
exp(coef(pois_m1))   # IRR


## 9-1. Figure helpers: observed / fitted / counterfactual trend --------------

make_pois_fig <- function(model, denom_var, data) {
  cf_trend <- feols(as.formula(paste(denom_var, "~ time | ori9 + month")),
                    data = filter(data, post == 0, !is.infinite(.data[[denom_var]])),
                    cluster = ~ori9)
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
    geom_line(aes(y = observed,       color = "Observed"),  linewidth = 0.8) +
    geom_line(aes(y = fitted,         color = "Fitted"),    linewidth = 0.7) +
    geom_line(aes(y = counterfactual, color = "Pretrend"),  linewidth = 0.7, linetype = "dashed") +
    geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red", alpha = 0.6) +
    scale_color_manual(values = c("Observed" = "orange", "Fitted" = "steelblue", "Pretrend" = "darkgreen")) +
    labs(title = title_label, x = NULL, y = "Homicide Count", color = NULL) +
    theme_minimal() +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = c(0, 0)) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10))
}

fig_pois_m1 <- make_pois_fig(pois_m1, "log_assault1", final_reg_pois)
plot_pois_fig(fig_pois_m1, "")

ggsave(paste0(basefolder, "Figures/Extended Data Fig 9.png"), 
       width = 10, height = 5, dpi = 300)
# =============================================================================
# END OF REPLICATION CODE ----
# =============================================================================