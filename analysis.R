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

covid <- covid %>% 
  # Filter only countries with at least 50 cases
  # (this will also cut out all days before arriving at 50 cases)
  group_by(country) %>% 
  filter(cummax(cases) >= 50) %>% 
  # ...and at least 14 days of data afterwards 
  # (this will happen automatically when a country has no value for day 14)
  arrange(country, date) %>% 
  mutate(day = 1:n()) %>%
  filter(day %in% c(1, 14)) %>% 
  pivot_wider(id_cols = 'country', names_from = 'day', values_from = 'cases', names_prefix = "cases_day") %>% 
  # compute growth over 14 days
  mutate(growth = cases_day14 - cases_day1) %>% 
  drop_na()

# Note: filepath should be changed as needed
bigfive <- read_tsv("bigfive_data.csv")
bigfive <- bigfive %>% select(EXT1:OPN10, country) %>% 
  mutate(across(-country, as.numeric))

negatively_keyed = c('EXT2', 'EXT4', 'EXT6', 'EXT8', 'EXT10',
                    'EST2', 'EST4',
                    'AGR1', 'AGR3', 'AGR5', 'AGR7', 
                    'CSN2', 'CSN4', 'CSN6', 'CSN8', 
                    'OPN2', 'OPN4', 'OPN6')

# WARNING: this takes a long time to run
bigfive <- bigfive %>% 
  # reverse negatively keyed variables
  mutate(across(negatively_keyed, ~(6-.x))) %>% 
  # average each trait within individual
  mutate(EXT = rowMeans(pick(EXT1:EXT10), na.rm = TRUE),
         EST = rowMeans(pick(EST1:EST10), na.rm = TRUE),
         AGR = rowMeans(pick(AGR1:AGR10), na.rm = TRUE),
         CSN = rowMeans(pick(CSN1:CSN10), na.rm = TRUE),
         OPN = rowMeans(pick(OPN1:OPN10), na.rm = TRUE)) %>% 
  select(EXT:OPN, country)

bigfive <- bigfive %>% 
  # average by country, noting how many cases are in each
    # (we'll keep countries with 1000 or fewer cases for now, 
    # just in case we want them for visualization down the line)
  # just for kicks, lets take note of standard deviation too
  group_by(country) %>% 
  summarise(EXT_sd = sd(EXT, na.rm = TRUE),
            EST_sd = sd(EST, na.rm = TRUE),
            AGR_sd = sd(AGR, na.rm = TRUE),
            CSN_sd = sd(CSN, na.rm = TRUE),
            OPN_sd = sd(OPN, na.rm = TRUE),
            
            EXT = mean(EXT, na.rm = TRUE),
            EST = mean(EST, na.rm = TRUE),
            AGR = mean(AGR, na.rm = TRUE),
            CSN = mean(CSN, na.rm = TRUE),
            OPN = mean(OPN, na.rm = TRUE),
            
            n = n()) 

# Import country codes
country_codes <- read_csv("country_codes.csv") %>% select(1:2) %>% 
  rename(country = `English short name lower case`,
         code = `Alpha-2 code`)

# Put all the data together
unified_data <- bigfive %>% 
  rename(code = country) %>% 
  left_join(country_codes) %>% 
  # I'm noticing that not all names match up, so we'll need to change a couple manually before the big join
  mutate(country = case_when(country == 'United States Of America' ~ 'US',
                             country == 'Korea, Republic of (South Korea)' ~ 'Korea, South',
                             .default = country)) %>% 
  right_join(covid)

# Example Plot
unified_data %>% 
  # MAKE SURE TO FILTER OUT ONLY n > 1000
  filter(n > 1000) %>% 
  ggplot(aes(EXT, growth)) +
    geom_point(aes(size = n), color = 'orange') +
    ggrepel::geom_text_repel(aes(label = country), size = 2) +
    geom_smooth(method = 'lm', alpha = .2) +
    theme_minimal() +
    labs(x = "Mean Extraversion", y = "Increase in Cases Over 14 Days",
         size = "Number of\nObservations")




