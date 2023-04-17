library(tidyverse)
library(lubridate)

covid <- bind_rows(read_csv("test.csv") %>% rename(Id = ForecastId) %>% 
                     mutate(ConfirmedCases = NA, 
                            Fatalities = NA), 
                   read_csv("train.csv"))

# Select the columns we need
covid <- covid %>% 
  arrange(Date) %>% 
  mutate(country = `Country/Region`, date = Date, cases = ConfirmedCases) %>% 
  select(country, date, cases)

# Filter only countries with at least 50 cases and at least 14 days of data afterwards
covid %>% 
  group_by(country) %>% 
  filter(max(cases, na.rm = TRUE) > 50)

bigfive <- read_tsv("/Users/louisteitelbaum/Projects/covid_bigfive/data-final.csv")
bigfive <- bigfive %>% select(EXT1:OPN10, country)

