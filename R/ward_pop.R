library(readxl)
library(tidyverse)
library(data.table)

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

age_levels = c(
  "UNDER 1", "1-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
  "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
  "85-89", "90+"
)

esp2013 = data.frame(
  age_band = age_levels,
  standard_pop = c(
    1000, 4000, 5500, 5500, 5500,
    6000, 6000, 6500, 7000, 7000,
    7000, 7000, 6500, 6000, 5500,
    5000, 4000, 2500, 1500, 1000
  )
) %>%
  mutate(age_band = factor(age_band, levels = age_levels))

keep_lads = c(
  "Birmingham", "Solihull", "Walsall",
  "Sandwell", "Dudley", "Wolverhampton"
)

keep_years = 2011:2020

# -------------------------------------------------------------------------
# Read ward population data
# -------------------------------------------------------------------------

wardpop_2001_2020 = read_excel(
  "~/R_project/Shinyapp_spatial/data/Wards21_01_20_pop_est.xlsx",
  sheet = "Ward Populations"
) %>%
  rename(
    WD21CD = `WD21CD 1`,
    WD21NM = `WD21NM 1`
  )

names(wardpop_2001_2020)
glimpse(wardpop_2001_2020)

# -------------------------------------------------------------------------
# Read local WD21 to LAD21 lookup
# Replace this path with your actual local lookup file if different
# -------------------------------------------------------------------------

ward_lad_lookup = read_excel(
  "~/R_project/Shinyapp_spatial/data/WD21_REGD21_LAD21_EW_LU.xlsx"
) %>%
  select(WD21CD, LAD21NM) %>%
  distinct()

# -------------------------------------------------------------------------
# Helper: collapse single year age into age bands
# -------------------------------------------------------------------------

age_to_band = function(age) {
  case_when(
    age == 0 ~ "UNDER 1",
    age >= 1  & age <= 4  ~ "1-4",
    age >= 5  & age <= 9  ~ "5-9",
    age >= 10 & age <= 14 ~ "10-14",
    age >= 15 & age <= 19 ~ "15-19",
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    age >= 90 ~ "90+",
    TRUE ~ NA_character_
  )
}

# -------------------------------------------------------------------------
# Identify male and female single-year age columns
# Example columns: m00, m01, ..., m90, f00, f01, ..., f90
# -------------------------------------------------------------------------

sex_age_cols = names(wardpop_2001_2020)[
  str_detect(str_to_lower(names(wardpop_2001_2020)), "^[mf][0-9]{2,3}$")
]

# -------------------------------------------------------------------------
# Clean ward population data
# -------------------------------------------------------------------------

ward21pop_2001_2020 = wardpop_2001_2020 %>%
  select(WD21CD, WD21NM, year, all_of(sex_age_cols)) %>%
  mutate(
    WD21CD = as.character(WD21CD),
    WD21NM = as.character(WD21NM),
    year = as.integer(str_extract(as.character(year), "\\d{4}"))
  ) %>%
  left_join(ward_lad_lookup, by = "WD21CD") %>%
  filter(
    LAD21NM %in% keep_lads,
    year %in% keep_years
  ) %>%
  pivot_longer(
    cols = all_of(sex_age_cols),
    names_to = "sex_age",
    values_to = "count"
  ) %>%
  mutate(
    count = as.numeric(count),
    age = as.integer(str_extract(sex_age, "\\d+")),
    age_band = age_to_band(age)
  ) %>%
  group_by(LAD21NM, WD21CD, WD21NM, year, age_band) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year = as.character(year),
    fin_year = paste0(
      substr(year, 3, 4),
      substr(as.numeric(year) + 1, 3, 4)
    ),
    age_band = factor(age_band, levels = age_levels)
  ) %>%
  left_join(esp2013, by = "age_band") %>%
  select(
    LAD21NM,
    WD21CD,
    WD21NM,
    year,
    fin_year,
    age_band,
    count,
    standard_pop
  ) %>%
  arrange(LAD21NM, WD21CD, year, age_band)

# -------------------------------------------------------------------------
# Validation checks
# -------------------------------------------------------------------------

ward_counts_by_lad = ward21pop_2001_2020 %>%
  distinct(LAD21NM, WD21CD, WD21NM) %>%
  count(LAD21NM, name = "n_wards") %>%
  arrange(LAD21NM)

ward_year_age_band_check = ward21pop_2001_2020 %>%
  distinct(WD21CD, year, age_band) %>%
  count(WD21CD, year, name = "n_age_bands") %>%
  filter(n_age_bands != length(age_levels))

missing_standard_pop_check = ward21pop_2001_2020 %>%
  filter(is.na(standard_pop))

missing_or_negative_count_check = ward21pop_2001_2020 %>%
  filter(is.na(count) | count < 0)

annual_lad_population_totals = ward21pop_2001_2020 %>%
  group_by(LAD21NM, year, fin_year) %>%
  summarise(total_population = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(LAD21NM, year)

print(ward_counts_by_lad)
print(ward_year_age_band_check)
print(missing_standard_pop_check)
print(missing_or_negative_count_check)
print(annual_lad_population_totals)

# -------------------------------------------------------------------------
# Stop if validation fails
# -------------------------------------------------------------------------

stopifnot(identical(
  names(ward21pop_2001_2020),
  c("LAD21NM", "WD21CD", "WD21NM", "year", "fin_year", "age_band", "count", "standard_pop")
))

stopifnot(nrow(ward_year_age_band_check) == 0)
stopifnot(nrow(missing_standard_pop_check) == 0)
stopifnot(nrow(missing_or_negative_count_check) == 0)

# -------------------------------------------------------------------------
# Save
# -------------------------------------------------------------------------

# write_rds(
#   ward21pop_2001_2020,
#   "data/ward21pop_2001_2020.rds"
# )
# # -------------------------------------------------------------------------
# Save final ward-level population output
# -------------------------------------------------------------------------

################################################################################################
################################################################################################
################################################################################################


Ward_mid_2021 = read_excel("data/Ward_mid_202122.xlsx", 
                              sheet = "Mid-2021 Ward 2022", skip = 3)


Ward_mid_2022 = read_excel("data/Ward_mid_202122.xlsx", 
                            sheet = "Mid-2022 Ward 2022", skip = 3)




# -------------------------------------------------------------------------
# Function to clean one WD22 population sheet
# -------------------------------------------------------------------------

clean_ward22_pop = function(data, pop_year) {
  
  sex_age_cols = names(data)[
    str_detect(names(data), "^[FM][0-9]{1,2}$")
  ]
  
  data %>%
    filter(`LAD 2022 Name` %in% keep_lads) %>%
    select(
      LAD22NM = `LAD 2022 Name`,
      WD22CD = `Ward 2022 Code`,
      WD22NM = `Ward 2022 Name`,
      all_of(sex_age_cols)
    ) %>%
    pivot_longer(
      cols = all_of(sex_age_cols),
      names_to = "sex_age",
      values_to = "count"
    ) %>%
    mutate(
      year = as.character(pop_year),
      age = as.integer(str_extract(sex_age, "\\d+")),
      age_band = age_to_band(age)
    ) %>%
    group_by(LAD22NM, WD22CD, WD22NM, year, age_band) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      fin_year = paste0(
        substr(year, 3, 4),
        substr(as.numeric(year) + 1, 3, 4)
      ),
      age_band = factor(age_band, levels = age_levels)
    ) %>%
    left_join(esp2013, by = "age_band") %>%
    select(
      LAD22NM,
      WD22CD,
      WD22NM,
      year,
      fin_year,
      age_band,
      count,
      standard_pop
    ) %>%
    arrange(LAD22NM, WD22CD, year, age_band)
}

# -------------------------------------------------------------------------
# Clean 2021 and 2022
# -------------------------------------------------------------------------

ward22pop_2021 = clean_ward22_pop(Ward_mid_2021, 2021)
ward22pop_2022 = clean_ward22_pop(Ward_mid_2022, 2022)

ward22pop_2021_2022 = bind_rows(
  ward22pop_2021,
  ward22pop_2022
)


# -------------------------------------------------------------------------
# Check whether WD21CD and WD22CD match for your six LADs
# -------------------------------------------------------------------------

ward21_codes = ward21pop_2001_2020 %>%
  distinct(LAD21NM, WD21CD, WD21NM) %>%
  rename(
    LADNM = LAD21NM,
    WDCD = WD21CD,
    WDNM = WD21NM
  )

ward22_codes = ward22pop_2021_2022 %>%
  distinct(LAD22NM, WD22CD, WD22NM) %>%
  rename(
    LADNM = LAD22NM,
    WDCD = WD22CD,
    WDNM = WD22NM
  )



# Check whether WD21 and WD22 ward codes are identical
# If both sets are empty, geographies are consistent = safe to stack

codes_in_21_not_22 = setdiff(ward21_codes$WDCD, ward22_codes$WDCD)
codes_in_22_not_21 = setdiff(ward22_codes$WDCD, ward21_codes$WDCD)

codes_in_21_not_22
codes_in_22_not_21

# More detailed mismatch tables
#Helps identify boundary changes or missing wards
ward21_not_22_detail = ward21_codes %>%
  filter(WDCD %in% codes_in_21_not_22)

ward22_not_21_detail = ward22_codes %>%
  filter(WDCD %in% codes_in_22_not_21)

ward21_not_22_detail
ward22_not_21_detail





# Only valid if Step 2 confirms ward codes match

ward21pop_2021_2022_as21 = ward22pop_2021_2022 %>%
  rename(
    LAD21NM = LAD22NM,
    WD21CD = WD22CD,
    WD21NM = WD22NM
  ) %>%
  select(
    LAD21NM,
    WD21CD,
    WD21NM,
    year,
    fin_year,
    age_band,
    count,
    standard_pop
  )

#Combine datasets (2011–2020 + 2021–2022)
ward21pop_2011_2022 = bind_rows(
  ward21pop_2001_2020,
  ward21pop_2021_2022_as21
) %>%
  arrange(LAD21NM, WD21CD, year, age_band)




write_rds(
  ward21pop_2011_2022,
  "data/ward21pop_2011_2022.rds"
)


































