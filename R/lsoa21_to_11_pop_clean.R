library(sf)

# ------------------------------------------------------------
# 1. Read LSOA boundary files (FULL UK)
# ------------------------------------------------------------


lsoafulluk_21 = read_sf("data/LSOA21_fullUK/LSOA_2021_EW_BFC_V10.shp")

lsoafulluk_11 = read_sf("data/LSOA11_fullUK/LSOA_2011_EW_BFC_V3.shp")


# ------------------------------------------------------------
# 1. Read LSOA boundary files (bsol_bc)
# ------------------------------------------------------------

lsoabsolbc_21 = read_sf("data/WM_LSOA21_table/LSOA_(2021)_to_Electoral_Ward_(2024)_to_LAD_(2024)_Best_Fit_Lookup_in_EW.shp")

lsoabsolbc_11 = read_sf("data/WM_LSOA11_table/Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2017)_Lookup_in_England_and_Wales.shp")

#clean the data
cleaned_lsoa21 = lsoabsolbc_21 %>% 
  select(LSOA21CD, LSOA21NM, LAD24NM) %>% 
  filter(LAD24NM %in% c("Birmingham", "Solihull", "Walsall", "Sandwell", "Dudley", "Wolverhampton")) %>% 
  distinct(LSOA21CD, LSOA21NM, LAD24NM) %>% 
  inner_join(lsoafulluk_21, by = c("LSOA21CD", "LSOA21NM")) %>% 
  st_as_sf() 


cleaned_lsoa11 = lsoabsolbc_11 %>% 
  select(LSOA11CD, LSOA11NM,LAD17NM) %>% 
  filter(LAD17NM %in% c("Birmingham", "Solihull", "Walsall", "Sandwell", "Dudley", "Wolverhampton")) %>% 
  distinct(LSOA11CD, LSOA11NM, LAD17NM) %>% 
  inner_join(lsoafulluk_11, by = c("LSOA11CD", "LSOA11NM")) %>% 
  st_as_sf() 

# ------------------------------------------------------------
# 2. Reproject BOTH layers to British National Grid (EPSG:27700)
#    (required for valid area calculations)
# ------------------------------------------------------------

cleaned_lsoa11_bng = st_transform(cleaned_lsoa11, 27700)
cleaned_lsoa21_bng = st_transform(cleaned_lsoa21, 27700)

# ------------------------------------------------------------
# 3. Compute geometric intersections
#    (returns polygon fragments, lines, and points)
# ------------------------------------------------------------

intersections = st_intersection(
  cleaned_lsoa11_bng  %>% select(lsoa11 = LSOA11CD),
  cleaned_lsoa21_bng %>% select(lsoa21 = LSOA21CD)
)

# ------------------------------------------------------------
# 4. Keep polygonal overlaps only (area-based intersections)
# ------------------------------------------------------------

intersections_poly = intersections %>%
  filter(st_dimension(geometry) == 2)

# ------------------------------------------------------------
# 5. Collapse geometry fragments to LSOA11–LSOA21 pairs
#    and remove negligible boundary slivers
# ------------------------------------------------------------

weights_pair = intersections_poly %>%
  mutate(area_intersection = as.numeric(st_area(geometry))) %>%
  filter(area_intersection > 1) %>%        # remove numerical artefacts
  st_drop_geometry() %>%
  group_by(lsoa11, lsoa21) %>%
  summarise(
    area_intersection = sum(area_intersection),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 6. Compute total area of each LSOA21 (denominator)
# ------------------------------------------------------------

lsoa21_area = cleaned_lsoa21_bng %>%
  mutate(area_lsoa21 = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  select(lsoa21 = LSOA21CD, area_lsoa21)

# ------------------------------------------------------------
# 7. Calculate area-based weights
# ------------------------------------------------------------

weights_pair = weights_pair %>%
  left_join(lsoa21_area, by = "lsoa21") %>%
  mutate(weight = area_intersection / area_lsoa21)



# ------------------------------------------------------------
#  Renormalise weights within each LSOA21
# so each LSOA21 population is fully allocated to overlapping LSOA11s
# ------------------------------------------------------------

weights_pair = weights_pair %>%
  group_by(lsoa21) %>%
  mutate(weight = weight / sum(weight)) %>%
  ungroup()


pop_lsoa11 = All_combined_cleandata %>% 
  left_join(
    weights_pair,
    by = c("LSOA21CD" = "lsoa21"),
    relationship = "many-to-many"
  ) %>% 
  mutate(count_lsoa11 = count * weight) %>% 
  group_by(lsoa11, age_band, year, fin_year, standard_pop) %>% 
  summarise(count = sum(count_lsoa11, na.rm = TRUE), .groups = "drop") %>% 
  left_join(
    cleaned_lsoa11 %>% 
      st_drop_geometry() %>% 
      select(LSOA11CD, LSOA11NM, LAD17NM) %>% 
      distinct(),
    by = c("lsoa11" = "LSOA11CD")
  ) %>% 
  select(
    LAD17NM,
    LSOA11CD = lsoa11,
    LSOA11NM,
    year,
    fin_year,
    age_band,
    count,
    standard_pop
  )



write_rds(pop_lsoa11, "data/lsoa11pop.rds")







     