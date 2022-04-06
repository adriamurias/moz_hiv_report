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

hiv_estimates[4:51] <- lapply(hiv_estimates[4:51], gsub, pattern = " ", replacement = "")

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

# Population values from 
population <- read_csv('data/TB_burden_countries_2022-03-22.csv') %>% 
  # filter for Mozambique
  filter(iso3 == "MOZ") %>% 
  select(year, e_pop_num)
  

#------------------------------------------------------------------------------
# Tidy Data
#------------------------------------------------------------------------------

hiv_estimates %>% 
  left_join(
    population,
    by = "year") %>% View()


hiv_estimates_modif <- hiv_estimates %>% 
  left_join(
    population,
    by = "year") %>% 
  mutate(e_prev_100k = e_prev_100/100 * 100000,
         e_prev_100k_lo = e_prev_100_lo/100 * 100000,
         e_prev_100k_hi = e_prev_100_hi/100 * 100000,
         e_mort_100k = e_mort/e_pop_num * 100000,
         ) %>% 
  select(year, e_prev_100k, e_prev_100k_lo, e_prev_100k_hi, e_mort_100k, e_pop_num)

#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------

# Evolution of main epidemiologic variables
hiv_estimates_modif %>% 
  ggplot(aes(x = year)) +
  # Prevalence
  geom_line(aes(y = e_prev_100k, color = "Prevalence"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_prev_100k_lo, ymax = e_prev_100k_hi),
              fill = "#4daf4a",
              alpha = 0.1) +
  # Mortality
  geom_line(aes(y = e_mort_100k, color = "Mortality"),
            size = 1.4) +
  # Legend
  scale_colour_manual("", 
                      breaks = c(
                        "Prevalence", "Mortality"),
                      values = c(
                        "#4daf4a", "#984ea3"))




  # Treatment
  geom_line(aes(y = cases_100k, color = "On treatment"),
            size = 1.4) +
  # HIV Positive Proportion
  geom_line(aes(y = e_inc_tbhiv_100k, color = "TB & HIV"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_inc_tbhiv_100k_lo, ymax = e_inc_tbhiv_100k_hi),
              fill = "#e41a1c",
              alpha = 0.1) +
  # Mortality
  geom_line(aes(y = e_mort_tbhiv_100k, color = "Mortality"),
            size = 1.4) + 
  geom_ribbon(aes(ymin = e_mort_tbhiv_100k_lo, ymax = e_mort_tbhiv_100k_hi),
              fill = "#984ea3",
              alpha = 0.1) +
  # Legend
  scale_colour_manual("", 
                      breaks = c(
                        "Inicidence", "On treatment", "TB & HIV", "Mortality"),
                      values = c(
                        "#4daf4a", "#377eb8", "#e41a1c", "#984ea3")) +
  # Aesthetics
  theme_bw(base_size = 14) +
  theme(legend.position="top") +
  scale_x_continuous(breaks = seq(min(moz_tb_modif$year),
                                  max(moz_tb_modif$year), by = 2),
                     minor_breaks = seq(2000, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, max(moz_tb_modif$e_inc_100k_hi), by = 50),
                     minor_breaks = seq(20000, 100000, 10000),
                     labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Year") +
  ylab("Rate per 100,000 population")

ggsave("figures/plot_tb_evol.png", plot_tb_evol, width = 8, height = 6)
