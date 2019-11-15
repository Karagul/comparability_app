library(povcalnetR)
library(haven)
library(tidyverse)
library(ggthemes)
library(plotly)



# constants ----------------------------------------------------------------

metadata_path <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/506801/povcalnet_comparability.csv"


# Load data ---------------------------------------------------------------
# Comparabilty metadata
metadata <- read_csv(metadata_path)
cov_lkup <- c("Rural", "Urban", "National", "National (Aggregate)")
metadata$coveragetype <- cov_lkup[metadata$coveragetype]
dat_lkup <- c("Consumption", "Income")
metadata$datatype <- dat_lkup[metadata$datatype]
# Poverty data
cov_lkup <- c("National", "Urban", "Rural", "National (Aggregate)")
names(cov_lkup) <- c("N", "U", "R", "A")
pcn <- povcalnet()
# pcn <- pcn %>%
#   filter(coveragetype == "N")
pcn$coveragetype <- cov_lkup[pcn$coveragetype]
pcn$datatype <- str_to_title(pcn$datatype)

# Data prep ---------------------------------------------------------------

df <- pcn %>%
  inner_join(metadata, by = c("countrycode", "year", "coveragetype", "datatype")) %>%
  # filter(countrycode %in% country_list,
  #        year %in% year_range) %>%
  mutate(
    gini = gini * 100,
    code_break = paste0(countrycode, comparability),
    code_break = ifelse(str_detect(code_break, "NA$"), NA, code_break)
  ) %>%
  group_by(code_break) %>%
  arrange(year) %>% 
  mutate(
    min_year = min(year), 
    max_year = max(year),
    legend_keys = paste0(countryname, " (", unique(min_year), "-", unique(max_year), ")")
  ) %>%
  ungroup() %>%
  #select(countrycode, countryname, gini, year, code_break, legend_keys) %>%
  distinct()

country_selection <- unique(df[, c("countryname", "countrycode")])
country_names <- country_selection$countryname
country_selection <- country_selection$countrycode
names(country_selection) <- country_names
country_selection <- sort(country_selection)

indicator_selection <- c("headcount", "povertygap", "povertygapsq", "watts", "gini", "mean", "median", "mld")
names(indicator_selection) <- c("Poverty rate", "Poverty gap", "Poverty severity", "Watts index", "Gini", "Mean", "Median", "Mean Log Deviation")

coverage_selection <- c("National", "Urban", "Rural", "National (Aggregate)")
names(coverage_selection) <- c("National", "Urban", "Rural", "National (Aggregate)")

datatype_selection <- c("Consumption", "Income")
names(datatype_selection) <- c("Consumption", "Income")