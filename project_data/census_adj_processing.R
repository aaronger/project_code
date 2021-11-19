library(tidyverse)

# load census adjacency file
cty_adj_raw <- read_tsv(
  "/Users/aaron_new/Dropbox/math797ns/project_code/project_data/county_adjacency.txt",
  col_names = c("county", "fips", "nghb_county", "nghb_fips")) %>% 
  tidyr::fill(county, fips)

census_ctys <- cty_adj_raw %>% select(county, fips) %>% distinct()

# anti_join(census_ctys, covidData::fips_codes, by = c("county" = "location_name_with_state")) %>% 
#   print(n=235)

# anti_join(
#   census_ctys %>% filter(!grepl("PR|VI|AS|GU|MP", county)),
#   covidHubUtils::hub_locations,
#   by = c("county" = "full_location_name")
# ) %>%
# left_join(covidHubUtils::hub_locations) %>%
#   select(-geo_type, -geo_value) %>% print(n=138)

# anti_join(
#   census_ctys %>% filter(!grepl("PR|VI|AS|GU|MP", county)),
#   covidHubUtils::hub_locations) 

# anti_join(covidHubUtils::hub_locations %>% filter(geo_type == "county"),
#           census_ctys, by = c("full_location_name" = "county")) %>% print(n=139)

# anti_join(covidHubUtils::hub_locations %>% filter(geo_type == "county"),
#           census_ctys)

# details about changes: 
# https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html

# Bedford "city" in VA
census_ctys %>% filter(grepl("[Bb]edford", county))
# confirm Bedford city only adjacent to Bedford County VA
cty_adj_raw %>% filter(grepl("[Bb]edford.*city.*VA", county) | grepl("[Bb]edford.*city.*VA", nghb_county))

# eliminate
cty_adj <- cty_adj_raw %>% filter(fips != "51515" & nghb_fips != "51515")

# extract former nghb of now defunct Valdez-Cordova Census Area, AK
valdez_nghb <- cty_adj %>% filter(grepl("Valdez", county) | grepl("Valdez", nghb_county))

# build new component by hand - see
# https://en.wikipedia.org/wiki/List_of_boroughs_and_census_areas_in_Alaska
valdez_repl <- tribble(
  ~county,                               ~fips,   ~nghb_county,                          ~nghb_fips,
  "Anchorage Municipality, AK",          "02020", "Chugach Census Area, AK",             "02063",
  "Kenai Peninsula Borough, AK",         "02122", "Chugach Census Area, AK",             "02063",
  "Matanuska-Susitna Borough, AK",       "02170", "Chugach Census Area, AK",             "02063",
  "Matanuska-Susitna Borough, AK",       "02170", "Copper River Census Area, AK",        "02066",
  "Southeast Fairbanks Census Area, AK", "02240", "Copper River Census Area, AK",        "02066",
  "Yakutat City and Borough, AK",        "02282", "Chugach Census Area, AK",             "02063",
  "Yakutat City and Borough, AK",        "02282", "Copper River Census Area, AK",        "02066",
  "Chugach Census Area, AK",             "02063", "Anchorage Municipality, AK",          "02020",
  "Chugach Census Area, AK",             "02063", "Kenai Peninsula Borough, AK",         "02122",
  "Chugach Census Area, AK",             "02063", "Matanuska-Susitna Borough, AK",       "02170",
  "Chugach Census Area, AK",             "02063", "Copper River Census Area, AK",        "02066",
  "Chugach Census Area, AK",             "02063", "Yakutat City and Borough, AK",        "02282",
  "Copper River Census Area, AK",        "02066", "Chugach Census Area, AK",             "02063",
  "Copper River Census Area, AK",        "02066", "Matanuska-Susitna Borough, AK",       "02170",
  "Copper River Census Area, AK",        "02066", "Southeast Fairbanks Census Area, AK", "02240",
  "Copper River Census Area, AK",        "02066", "Yakutat City and Borough, AK",        "02282",
)

# replace
cty_adj <- cty_adj %>% filter(!grepl("Valdez", county) & !grepl("Valdez", nghb_county)) %>% 
  rbind(valdez_repl)

cty_matches <- tribble(
  ~old_name,                       ~old_fips,  ~new_name,                  ~new_fips,
  "Wade Hampton Census Area, AK",  02270,      "Kusilvak County, AK",      02158,
  "Shannon County, SD",            46113,      "Oglala Lakota County, SD", 46102,
)

cty_adj <- cty_adj %>% mutate(
  county = replace(county, county == "Wade Hampton Census Area, AK", "Kusilvak County, AK"),
  county = replace(county, county == "Shannon County, SD", "Oglala Lakota County, SD"),
  nghb_county = replace(nghb_county, nghb_county == "Wade Hampton Census Area, AK", "Kusilvak County, AK"),
  nghb_county = replace(nghb_county, nghb_county == "Shannon County, SD", "Oglala Lakota County, SD"),
  fips = replace(fips, fips == "02270", "02158"),
  fips = replace(fips, fips == "46113", "46102"),
  nghb_fips = replace(nghb_fips, nghb_fips == "02270", "02158"),
  nghb_fips = replace(nghb_fips, nghb_fips == "46113", "46102"),
)

write_csv(cty_adj, paste0(here(), "/project_data/county_adjacency_fixed.csv"))

# library(igraph)
# g <- graph_from_data_frame(cty_adj %>% select(fips, nghb_fips))
# g_mat <- as_adjacency_matrix(g)
