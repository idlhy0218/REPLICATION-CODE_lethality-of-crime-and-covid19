# =============================================================================
# REPLICATION CODE: Lethality of Crime and COVID-19 — CCJ City-Month Data ----
# =============================================================================
# Description : City-month panel ITS analysis using CCJ crime data (2018–2022)
# Data        : CCJ City Month Master (Stata), city-level monthly crime counts
# Unit        : City-month (city_id)
# Author      : Heeyoung Lee
# Last Update : 2026-04-29 (YYYY-MM-DD)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACKAGES ----
# -----------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyverse)
library(haven)
library(fixest)


# -----------------------------------------------------------------------------
# 1. LOAD AND PREPARE DATA ----
# -----------------------------------------------------------------------------
basefolder <- "C:/Users/User/OneDrive/Github Desktop/REPLICATION-CODE_lethality-of-crime-and-covid19/"

df <- read_dta(paste0(basefolder, "Data/CCJ crime_2018_2022/CCJ_City_Month_Master.dta"))

final_reg_ccj <- df %>%
  mutate(
    # Assign population by year (ACS vintage)
    pop = case_when(
      year %in% 2018:2019 ~ pop2019,
      year == 2020        ~ pop2020,
      year == 2021        ~ pop2021,
      year >= 2022        ~ pop2022
    ),
    lethality1 = share,
    lethality2 = share_2,
    assault1   = agg_assault,
    assault2   = agg_assault + robbery
  ) %>%
  filter(!is.na(pop), pop > 0)


# -----------------------------------------------------------------------------
# 2. MAIN ANALYSIS: FIXED-EFFECTS POISSON ITS ----
# -----------------------------------------------------------------------------
# DV:     homicide count (integer)
# Offset: log(agg assault + robbery)
# FE:     city (city_id) + calendar month
# SE:     clustered by city_id

final_reg_pois <- final_reg_ccj %>%
  mutate(
    homicide_count = as.integer(round(homicide)),
    log_assault    = log(agg_assault + robbery)
  ) %>%
  filter(!is.na(homicide_count), !is.na(log_assault))

pois <- fepois(homicide_count ~ t + post + t_post + offset(log_assault) | month + city_id,
               data = final_reg_pois, cluster = ~city_id)

etable(pois)
exp(coef(pois))   # IRR


## 2-1. Figure helpers: observed / fitted / counterfactual trend --------------

make_pois_fig <- function(model, denom_var, data) {
  cf_trend <- feols(as.formula(paste(denom_var, "~ t | city_id + month")),
                    data = filter(data, post == 0, !is.infinite(.data[[denom_var]])),
                    cluster = ~city_id)
  data %>%
    mutate(
      denom_cf       = predict(cf_trend, newdata = .),
      fitted         = predict(model, newdata = ., type = "response"),
      counterfactual = predict(model,
                               newdata = mutate(., post = 0, t_post = 0,
                                                !!denom_var := denom_cf),
                               type = "response")
    ) %>%
    group_by(month_start) %>%
    summarise(observed       = mean(homicide_count, na.rm = TRUE),
              fitted         = mean(fitted,         na.rm = TRUE),
              counterfactual = mean(counterfactual, na.rm = TRUE), .groups = "drop")
}

plot_pois_fig <- function(fig_data, title_label) {
  ggplot(fig_data, aes(x = month_start)) +
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

plot_pois_fig(make_pois_fig(pois, "log_assault", final_reg_pois),
              "")

ggsave(paste0(basefolder, "Figures/Extended Data Fig 10.png"), 
       width = 10, height = 5, dpi = 300)
# =============================================================================
# END OF REPLICATION CODE ----
# =============================================================================