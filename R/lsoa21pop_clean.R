library(readxl)
library(tidyverse)
library(data.table)

###############################################################################
# CONFIGURATION
###############################################################################

# Define ordered age groups (used to ensure consistent age band ordering)
age_levels <- c(
  "UNDER 1", "1-4",  "5-9",   "10-14", "15-19", "20-24",
  "25-29",   "30-34","35-39", "40-44", "45-49", "50-54",
  "55-59",   "60-64","65-69", "70-74", "75-79", "80-84",
  "85-89",   "90+"
)
#ESP 2013
esp2013 = data.frame(
  age_band     = c("UNDER 1", "1-4",  "5-9",   "10-14", "15-19",
                   "20-24",   "25-29","30-34", "35-39", "40-44",
                   "45-49",   "50-54","55-59", "60-64", "65-69",
                   "70-74",   "75-79","80-84", "85-89", "90+"),
  standard_pop = c(1000,  4000,  5500,  5500,  5500,
                   6000,  6000,  6500,  7000,  7000,
                   7000,  7000,  6500,  6000,  5500,
                   5000,  4000,  2500,  1500,  1000)
) %>% 
  mutate(age_band = factor(age_band, levels = age_levels))

# File paths to ONS LSOA mid-year population estimate workbooks
path1 = "data/lsoa pop/sapelsoasyoa20222024.xlsx"  # 2022-2024
path2 = "data/lsoa pop/sapelsoasyoa20192022.xlsx"  # 2019-2022
path3 = "data/lsoa pop/sapelsoasyoa20152018.xlsx"  # 2015-2018
path4 = "data/lsoa pop/sapelsoasyoa20112014.xlsx"  # 2011-2014


###############################################################################
# BUILD DATALIST: one row per sheet to process
###############################################################################

datalist = rbind(
  expand.grid(path = path1, sheets = excel_sheets(path1)[5:length(excel_sheets(path1))]),
  expand.grid(path = path2, sheets = excel_sheets(path2)[5:length(excel_sheets(path2))]),
  expand.grid(path = path3, sheets = excel_sheets(path3)[5:length(excel_sheets(path3))]),
  expand.grid(path = path4, sheets = excel_sheets(path4)[5:length(excel_sheets(path4))])
) %>% 
  mutate(
    # Extract calendar year from sheet name (e.g. "Mid-2024 LSOA 2021" -> "2024")
    year          = str_extract(sheets, "\\d{4}"),
    # Derive financial year label (e.g. 2024 -> "2425")
    financialyear = paste0(substr(as.numeric(year), 3, 4), substr(as.numeric(year) + 1, 3, 4)),
    # Row index used for loop iteration
    id            = row_number()
  )


###############################################################################
# FUNCTION: clean and reshape a single sheet
###############################################################################

cleandata = function(path, sheet) {
  
  # Read raw data, skipping the first 3 header rows
  data = read_excel(path, sheet = sheet, skip = 3)
  
  # Detect whichever LAD name column exists in this sheet
  lad_col = names(data)[grepl("^LAD.*Name$", names(data))][1]
  
  data %>% 
    filter(.data[[lad_col]] %in% c("Birmingham", "Solihull", "Walsall","Dudley", "Sandwell", "Wolverhampton")) %>% 
    # Pivot male (M) and female (F) age columns to long format
    pivot_longer(
      cols      = starts_with(c("F", "M")),
      values_to = "count",
      names_to  = "age"
    ) %>% 
    mutate(age = as.numeric(str_remove(age, "^[a-zA-Z]+"))) %>% 
    # Sum male and female counts per LSOA per single year of age
    group_by(`LSOA 2021 Code`, `LSOA 2021 Name`, age) %>% 
    summarise(count = sum(count), .groups = "drop") %>% 
    mutate(age_band = case_when(
      age == 0              ~ "UNDER 1",
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
      age >= 90             ~ "90+",
      TRUE                  ~ NA_character_   
    )) %>% 
    
    # Collapse single years into age band totals per LSOA
    group_by(`LSOA 2021 Code`, `LSOA 2021 Name`, age_band) %>% 
    summarise(count = sum(count), .groups = "drop") %>% 
    mutate(age_band = factor(age_band, levels = age_levels)) %>%
    left_join(esp2013, by = ("age_band")) %>% 
    arrange(`LSOA 2021 Code`, age_band)
}


###############################################################################
# LOOP: apply cleandata() to every sheet and collect results
###############################################################################

cleaned_datalist = list()

for (i in 1:nrow(datalist)) {
  
  datatoworkwith = datalist %>% filter(id == i)
  
  cleaned_datalist[[i]] = cleandata(
    path  = as.character(datatoworkwith$path),
    sheet = as.character(datatoworkwith$sheets)
  ) %>% 
    mutate(
      year     = datatoworkwith$year,
      fin_year = datatoworkwith$financialyear
    )
}


###############################################################################
# COMBINE AND SAVE
###############################################################################

All_combined_cleandata = rbindlist(cleaned_datalist)

colnames(All_combined_cleandata) = gsub(" ", "_", colnames(All_combined_cleandata))

All_combined_cleandata = All_combined_cleandata %>% 
  mutate(LAD21NM = str_extract(LSOA_2021_Name, "^.+(?=\\s+\\d+[A-Z])")) %>% 
  select(LAD21NM, 
         LSOA21CD = LSOA_2021_Code,
         LSOA21NM =LSOA_2021_Name,
         year,
         fin_year,
         age_band,
         count,
         standard_pop)

write_rds(All_combined_cleandata, "data/lsoa21pop.rds")

