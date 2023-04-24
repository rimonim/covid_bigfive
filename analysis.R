library(tidyverse)
library(lubridate)

covid <- read_csv("train.csv")

# Select the columns we need
covid <- covid %>% 
  arrange(`Country/Region`, Date) %>% 
  rename(country = `Country/Region`, date = Date, cases = ConfirmedCases) %>% 
  group_by(country, date) %>% 
  summarise(cases = sum(cases)) %>% 
  select(country, date, cases)

# Filter only countries with at least 50 cases and at least 14 days of data afterwards
covid <- covid %>% 
  group_by(country) %>% 
  filter(cummax(cases) > 50) %>% 
  arrange(country, date) %>% 
  mutate(day = 1:n()) %>%
  filter(day %in% c(1, 14)) %>% 
  pivot_wider(id_cols = 'country', names_from = 'day', values_from = 'cases', names_prefix = "cases_day") %>% 
  mutate(growth = cases_day14 - cases_day1) %>% 
  drop_na()


bigfive <- read_tsv("/Users/louisteitelbaum/Projects/covid_bigfive/data-final.csv")
bigfive <- bigfive %>% select(EXT1:OPN10, country)

