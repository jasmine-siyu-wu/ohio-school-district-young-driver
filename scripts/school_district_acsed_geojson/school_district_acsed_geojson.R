library(sf)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tigris)
library(ggrepel)
library(ggplot2)




# 1. Load ACS-ED school district population data, 2019-2023
acs_cw <- readxl::read_xlsx("./../../docs/data/ACSED_SexAge_Crosswalk.xlsx")
# the acsed table is pipe-delimited
readLines("./../../docs/data/acsed_sexage_table.txt", n = 3)
acs <- read_delim(
  "./../../docs/data/acsed_sexage_table.txt",
  delim = "|",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  trim_ws = TRUE
)
glimpse(acs) #589


# 2. Load school district boundaries
# # 2025 Ohio school-district boundaries
# school_district_url <- paste0(
#   "https://maps.ohio.gov/arcgis/rest/services/",
#   "Hosted/Ohio_School_Districts_2025/FeatureServer/0/query",
#   "?where=1%3D1",
#   "&outFields=*",
#   "&returnGeometry=true",
#   "&outSR=4326",
#   "&f=geojson"
# )
# 
# districts <- st_read(
#   school_district_url,
#   quiet = TRUE
# )

options(tigris_use_cache = TRUE)

districts <- school_districts(
  state = "OH",
  type = "unified",
  year = 2023,
  cb = TRUE
)

nrow(districts)
names(districts)

# 3. Standardize ACS columns
# Replace source column names as needed.
acs_clean <- acs %>%
  transmute(
    LEAID = as.character(LEAID),
    name = `School District`,
    
    # Total population
    total_pop_est = as.numeric(CDP05_1_EST),
    total_pop_moe = as.numeric(CDP05_1_EST),
    
    # Male population
    male_total_est = as.numeric(CDP05_2_EST),
    male_total_moe = as.numeric(CDP05_2_EST),
    
    male_15_17_est = as.numeric(CDP05_6_EST),
    male_15_17_moe = as.numeric(CDP05_6_MOE),
    male_18_19_est = as.numeric(CDP05_7_EST),
    male_18_19_moe = as.numeric(CDP05_7_MOE),
    
    # Female population
    female_total_est = as.numeric(CDP05_8_EST),
    female_total_moe = as.numeric(CDP05_8_EST),
    
    female_15_17_est = as.numeric(CDP05_12_EST),
    female_15_17_moe = as.numeric(CDP05_12_MOE),
    female_18_19_est = as.numeric(CDP05_13_EST),
    female_18_19_moe = as.numeric(CDP05_13_MOE)
  ) %>%
  mutate(
    # Combined ages 15-17
    pop_15_17_est = male_15_17_est + female_15_17_est,
    pop_15_17_moe = sqrt(male_15_17_moe^2 + female_15_17_moe^2),
    
    # Combined ages 18-19
    pop_18_19_est =  male_18_19_est + female_18_19_est,
    pop_18_19_moe = sqrt(male_18_19_moe^2 + female_18_19_moe^2),
    
    # Combined ages 15-19
    pop_15_19_est = pop_15_17_est + pop_18_19_est,
    pop_15_19_moe = sqrt(male_15_17_moe^2 + female_15_17_moe^2 +
                           male_18_19_moe^2 + female_18_19_moe^2)
  )

acs_clean %>%
  mutate(
    # Male + female should equal total population
    total_pop_check = male_total_est + female_total_est - total_pop_est,
    
    # Relative MOE (useful for identifying uncertainty)
    pop_15_17_rmoe = if_else(pop_15_17_est > 0,
                             pop_15_17_moe / pop_15_17_est,
                             NA_real_),
    pop_18_19_rmoe = if_else(pop_18_19_est > 0,
                             pop_18_19_moe / pop_18_19_est,
                             NA_real_)
  ) %>%
  select(total_pop_check, pop_15_17_rmoe, pop_18_19_rmoe)%>%
  summary()


# 4. Standardize district identifiers
acs_ids <- acs_clean %>%
  mutate(GEOID = str_pad(as.character(LEAID), 7, pad = "0"),
         match_id = str_sub(GEOID, 3, 7))
district_ids <- districts %>%
  mutate(GEOID = str_pad(as.character(GEOID), 7, pad = "0"),
         match_id = str_sub(GEOID, 3, 7))

# Check uniqueness of transformed IDs
acs_ids %>%
  st_drop_geometry() %>%
  count(match_id) %>%
  filter(n > 1)
district_ids %>%
  st_drop_geometry() %>%
  count(match_id) %>%
  filter(n > 1)

# Match coverage
acs_matched <- acs_ids %>%
  semi_join(
    st_drop_geometry(district_ids),
    by = "match_id"
  )
cat("ACS districts matched:", nrow(acs_matched), "\n")
# ACS districts matched: 588 


# 5. Check for duplicate district identifiers
stopifnot(!anyDuplicated(acs_clean$LEAID))
stopifnot(!anyDuplicated(districts$GEOID))


# 6. Check unmatched districts
unmatched <- anti_join(
  st_drop_geometry(district_ids),
  acs_ids,
  by = "GEOID"
)

print(unmatched %>% select(GEOID, NAME))
# GEOID                                             NAME
# 1  3904896                 North Bass Local School District
# 2  3904897                 Put-in-Bay Local School District
# 3  3904835                    Sebring Local School District
# 4  3905020                  Lordstown Local School District
# 5  3904915                    Western Local School District
# 6  3905022                   McDonald Local School District
# 7  3905071                  Northwood Local School District
# 8  3904868         Jefferson Township Local School District
# 9  3904871                New Lebanon Local School District
# 10 3904503                 Wellsville Local School District
# 11 3906496             College Corner Local School District
# 12 3904446                 New Boston Local School District
# 13 3904679             Kelleys Island Local School District
# 14 3999997                      School District Not Defined
# 15 3904895                Middle Bass Local School District
# 16 3905051                 Wolf Creek Local School District
# 17 3904536 Fairport Harbor Exempted Village School District
# 18 3904952               Southeastern Local School District
# 19 3904670                 Ayersville Local School District
# 20 3904554   Newcomerstown Exempted Village School District
# 21 3910026                       Clay Local School District
# 22 3904833                Lowellville Local School District
# 23 3904961                      Green Local School District
# 24 3904537      Georgetown Exempted Village School District


# 7. Join population data to boundaries
map_data <- district_ids %>%
  left_join(acs_ids, by = "GEOID") 

map_data %>%
  st_drop_geometry() %>%
  filter(is.na(pop_15_17_est)) %>%
  select(GEOID, NAME)


# 8. Convert to GeoJSON coordinates (WGS84)
map_data <- st_transform(map_data, 4326)


# 9. Simplify geometries for faster web loading
# Simplification is performed in a projected CRS.
map_data <- map_data %>%
  st_transform(5070) %>%
  st_simplify(dTolerance = 100,
              preserveTopology = TRUE) %>%
  st_transform(4326)


###########10. Export for GitHub Pages##################################
# dir.create("./../../docs/data",
#            recursive = TRUE,
#            showWarnings = FALSE)

names(map_data)[duplicated(names(map_data))]
anyDuplicated(names(map_data))

map_export <- map_data %>%
  select(
    GEOID,
    NAME,
    pop_15_17_est,
    pop_15_17_moe,
    pop_18_19_est,
    pop_18_19_moe,
    pop_15_19_est,
    pop_15_19_moe,
    geometry
  ) %>%
  st_transform(4326)
names(map_export)
anyDuplicated(names(map_export))

st_write(
  map_export,
  "./../../docs/data/ohio_school_districts.geojson",
  driver = "GeoJSON",
  quiet = TRUE
)

# map_data <- st_read(
#   "./../../docs/data/ohio_school_districts.geojson",
#   quiet = TRUE
# )



#Export csv file###################################
district_csv <- map_data %>%
  st_drop_geometry() %>%
  select(
    GEOID,
    NAME,
    STUSPS,
    HIGRADE,
    LOGRADE,
    total_pop_est,
    total_pop_moe,
    pop_15_17_est,
    pop_15_17_moe,
    pop_18_19_est,
    pop_18_19_moe,
    pop_15_19_est,
    pop_15_19_moe
  )

write_csv(
  district_csv,
  "./../../docs/data/ohio_district_population.csv",
  na = ""
)



#plot missing data school districts##############################
districts_map <- districts %>%
  mutate(
    data_status = if_else(
      GEOID %in% acs_ids$GEOID,
      "ACS-ED available",
      "Missing ACS-ED"
    )
  ) %>%
  filter(GEOID != "3999997")

ggplot() +
  geom_sf(
    data = districts_map,
    aes(fill = data_status),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      "ACS-ED available" = "grey85",
      "Missing ACS-ED" = "#D94841"
    ),
    name = NULL
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Ohio School Districts Missing ACS-ED Data",
    subtitle = "2019-2023 ACS-ED population estimates",
    caption = "Source: U.S. Census Bureau ACS-ED and 2023 TIGER/Line boundaries"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# The missing districts vary considerably in size: 
# We should not assume that all missing records correspond to very small populations.
# The Lake Erie islands are worth checking: 
# Several missing districts were island districts,
# which may have unusual population or reporting characteristics.


ggsave(
  "ohio_missing_acsed_districts.png",
  width = 10,
  height = 8,
  dpi = 300
)
