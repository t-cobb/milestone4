library(readxl)
library(tidyverse)
library(ipumsr)
library(rstanarm)


ddi <- read_ipums_ddi("raw_data/meps_00001.xml")
data <- read_ipums_micro(ddi)

saveRDS(data, "raw_data/ipumsmilestone4data.rds")

summary(data)
glimpse(data)
# NOUSLYLANG: no usual source of care because they speak a different language (0,1)
# USCPRPREV: Preventative care
# USCPRREFRL: Would go to PCP for referral 
# (work on reviewing the codebook for interpretation of the above variables)

# question: how has the utilization of care for preventative treatment changed over from 2010 - 2018?

x <- data %>% select(YEAR, NOUSLYLANG) %>%
  group_by(YEAR) %>%
  summarise(trend = sum(NOUSLYLANG)/n()) %>%
  drop_na() %>%
  ggplot(aes(x = YEAR, y = trend )) +
  geom_line()

x
