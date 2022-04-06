# Packages
library(tidyverse)
library(readxl)

#------------------------------------------------------------------------------
# Import World TB Data
#------------------------------------------------------------------------------
# Data obtained from UNAIDS:
# https://www.unaids.org/en/resources/documents/2021/HIV_estimates_with_uncerta
# inty_bounds_1990-present

# HIV Estimates
hiv_estimates <- read_excel('Data/HIV_estimates_from_1990-to-present.xlsx',
                     sheet = "HIV estimates - by Area",
                     skip = 6,
                     col_names = 
                       c('year','iso3','region',
                         'e_prev_100','e_prev_100_lo','e_prev_100_hi',
                         'e_prev_w15_24_100','e_prev_w15_24_100_lo',
                         'e_prev_w15_24_100_hi',
                         'e_prev_m15_24_100','e_prev_m15_24_100_lo',
                         'e_prev_m15_24_100_hi',
                         'e_mort','e_mort_lo','e_mort_hi',
                         'e_mort_child','e_mort_child_lo','e_mort_child_hi',
                         'e_mort_adults','e_mort_adults_lo','e_mort_adults_hi',
                         'e_hiv_child','e_hiv_child_lo','e_hiv_child_hi',
                         'e_hiv_adults_w','e_hiv_adults_w_lo',
                         'e_hiv_adults_w_hi',
                         'e_hiv_adults_m','e_hiv_adults_m_lo',
                         'e_hiv_adults_m_hi',
                         'e_hiv','e_hiv_lo','e_hiv_hi',
                         'e_incidence_15_49_1000','e_incidence_w15_49_1000_lo',
                         'e_incidence_w15_49_1000_hi',
                         'e_incidence_1000','e_incidence_1000_lo',
                         'e_incidence_1000_hi',
                         'e_pmtct','e_pmtct_lo','e_pmtct_hi',
                         'e_new_child','e_new_child_lo','e_new_child_hi',
                         'e_new_adults','e_new_adults_lo','e_new_adults_hi',
                         'e_new','e_new_lo','e_new_hi'), 
                     col_types = c(
                       "numeric", "text", "text", 
                       "numeric", "numeric", "numeric",
                       "numeric", "numeric", "numeric", "numeric",
                       "numeric", "numeric", "text", "text", "text", 
                       "text", "text", "text", "text", "text", 
                       "text", "text", "text", "text", "text", 
                       "text", "text", "text", "text", "text", 
                       "text", "text", "text", "numeric", "numeric", 
                       "numeric", "numeric", "numeric", "numeric", "text", 
                       "text", "text", "text", "text", "text", 
                       "text", "text", "text", "text", "text", 
                       "text")) %>% 
  filter(iso3 == "MOZ")

hiv_estimates[4:51] <- lapply(hiv_estimates[4:51], gsub, pattern = " ",
                              replacement = "")

hiv_estimates[4:51] <- lapply(hiv_estimates[4:51], as.numeric)

# HIV Estimates Dictionary
dictionary_hiv_estimates <- tibble(
  variable_name = names(hiv_estimates),
  description = names(
    # HIV Estimates Variable Definitions
    read_excel('Data/HIV_estimates_from_1990-to-present.xlsx',
               sheet = "HIV estimates - by Area",
               skip = 4, n_max = 1) %>% 
      cbind(list(year = NA, iso3 = NA, region = NA), .)) # add 3 first cols
                      ) %>% 
  .[!grepl("lo", .$variable_name) & !grepl("hi", .$variable_name),]

# HIV Test & Treat
hiv_test_treat <- read_excel('Data/HIV_estimates_from_1990-to-present.xlsx',
                            sheet = "HIV Test & Treat - by Area",
                            skip = 6, 
                            na = "...") %>% 
  filter(...2 == "MOZ") %>% 
  select(...1, ...2, Estimate...4, Low...5, High...6, Estimate...19, Low...20,
         High...21, Estimate...49, Low...50, High...51) %>% 
  rename(year = ...1, iso3 = ...2,
         # Know their status/PLHIV
         e_know_100_hiv = Estimate...4,
         e_know_100_hiv_lo = Low...5,
         e_know_100_hiv_hi = High...6,
         # On ART/PLHIV
         e_art_100_hiv = Estimate...19,
         e_art_100_hiv_lo = Low...20,
         e_art_100_hiv_hi = High...21,
         # Suppressed viral load/PLHIV
         e_suppressed_100_hiv = Estimate...49,
         e_suppressed_100_hiv_lo = Low...50,
         e_suppressed_100_hiv_hi = High...51)

hiv_test_treat[3:11] <- lapply(hiv_test_treat[3:11], as.numeric)

# HIV Test & Treat Dictionary
dictionary_hiv_estimates <- tibble(
  variable_name = names(hiv_estimates),
  description = names(
    # HIV Test & Treat Variable Definitions
    read_excel('Data/HIV_estimates_from_1990-to-present.xlsx',
               sheet = "HIV Test & Treat - by Area",
               skip = 4, n_max = 1) %>% 
      cbind(list(year = NA, iso3 = NA, region = NA), .)) # add 3 first cols
) %>% 
  .[!grepl("lo", .$variable_name) & !grepl("hi", .$variable_name),]

# Population values from 
population <- read_csv('data/TB_burden_countries_2022-03-22.csv') %>% 
  # filter for Mozambique
  filter(iso3 == "MOZ") %>% 
  select(year, e_pop_num)
  

#------------------------------------------------------------------------------
# Tidy Data
#------------------------------------------------------------------------------

hiv_estimates_modif <- hiv_estimates %>% 
  left_join(
    hiv_test_treat,
    by = "year") %>% 
  left_join(
    population,
    by = "year") %>% 
  mutate(e_prev_100 = e_prev_100,
         e_prev_100_lo = e_prev_100_lo,
         e_prev_100_hi = e_prev_100_hi,
         e_mort_100 = e_mort/e_pop_num * 100,
         
         e_know_100 = e_know_100_hiv/100 * e_prev_100,
         e_know_100_lo = e_know_100_hiv_lo/100 * e_prev_100,
         e_know_100_hi = e_know_100_hiv_hi/100 * e_prev_100,
         
         e_art_100 = e_art_100_hiv/100 * e_prev_100,
         e_art_100_lo = e_art_100_hiv_lo/100 * e_prev_100,
         e_art_100_hi = e_art_100_hiv_hi/100 * e_prev_100,
         
         e_suppressed_100 = e_suppressed_100_hiv/100 * e_prev_100,
         e_suppressed_100_lo = e_suppressed_100_hiv_lo/100 * e_prev_100,
         e_suppressed_100_hi = e_suppressed_100_hiv_hi/100 * e_prev_100,
         ) %>% 
  select(year, e_prev_100, e_prev_100_lo, e_prev_100_hi, e_mort_100, e_pop_num,
         
         e_know_100, e_know_100_lo, e_know_100_hi,
         
         e_art_100, e_art_100_lo, e_art_100_hi,
         
         e_suppressed_100, e_suppressed_100_lo, e_suppressed_100_hi)

#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------

# Evolution of Prevalence
plot_hiv_prev <- hiv_estimates_modif %>% 
  ggplot(aes(x = year)) +
  # Prevalence
  geom_line(aes(y = e_prev_100, color = "Prevalence"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_prev_100_lo, ymax = e_prev_100_hi),
              fill = "#e41a1c",
              alpha = 0.1) +
  # Legend
  scale_colour_manual("", 
                      breaks = c("Prevalence"),
                      values = c("#e41a1c")) +
  # Aesthetics
  theme_bw(base_size = 14) +
  theme(legend.position="top") +
  scale_x_continuous(breaks = seq(min(hiv_estimates_modif$year),
                                  max(hiv_estimates_modif$year), by = 2),
                     minor_breaks = seq(
                       min(hiv_estimates_modif$year),
                       max(hiv_estimates_modif$year),
                       1)) +
  scale_y_continuous(breaks = seq(0, max(hiv_estimates_modif$e_prev_100_hi),
                                  by = 2)) +
  xlab("Year") +
  ylab("Percentage of population")

ggsave("figures/plot_hiv_prev.png", plot_hiv_prev, width = 8, height = 6)

# Evolution of main epidemiologic variables
plot_hiv_evol <- hiv_estimates_modif %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(x = year)) +
  # Prevalence
  geom_line(aes(y = e_prev_100, color = "Prevalence"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_prev_100_lo, ymax = e_prev_100_hi),
              fill = "#e41a1c",
              alpha = 0.1) +
  # Know their status
  geom_line(aes(y = e_know_100, color = "Know their status"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_know_100_lo, ymax = e_know_100_hi),
              fill = "#377eb8",
              alpha = 0.1) +
  # On ART
  geom_line(aes(y = e_art_100, color = "On ART"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_art_100_lo, ymax = e_art_100_hi),
              fill = "#984ea3",
              alpha = 0.1) +
  # Suppressed viral load
  geom_line(aes(y = e_suppressed_100, color = "Suppressed viral load"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_suppressed_100_lo, ymax = e_suppressed_100_hi),
              fill = "#4daf4a",
              alpha = 0.1) +
  # Mortality
  geom_line(aes(y = e_mort_100, color = "Mortality"),
            size = 1.4) +
  # Legend
  scale_colour_manual("", 
                      breaks = c(
                        "Prevalence", "Know their status", "On ART",
                        "Suppressed viral load", "Mortality"),
                      values = c(
                        "#e41a1c", "#377eb8", "#4daf4a",
                        "#ff7f00", "#984ea3")) +
  # Aesthetics
  theme_bw(base_size = 14) +
  theme(legend.position="top") +
  scale_x_continuous(breaks = seq(min(hiv_estimates_modif$year),
                                  max(hiv_estimates_modif$year), by = 1),
                     minor_breaks = seq(
                       min(hiv_estimates_modif$year),
                       max(hiv_estimates_modif$year),
                       1)) +
  scale_y_continuous(breaks = seq(0, max(hiv_estimates_modif$e_prev_100_hi),
                                  by = 2)) +
  xlab("Year") +
  ylab("Percentage of population")

ggsave("figures/plot_hiv_evol.png", plot_hiv_evol, width = 8, height = 6)
