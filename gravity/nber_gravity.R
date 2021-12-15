library(tidyverse)
library(igraph)
library(here)

counties <- read_csv(paste0(here(),"/project_data/counties.csv"))
cdists <- read_csv(paste0(here(),"/gravity/sf12010countydistancemiles.csv")) %>% 
  filter(county1 %in% counties$geo_value, county2 %in% counties$geo_value)
 
# don't use pop product, only destination product; see xia et al eq (5)                  
cdists <- cdists %>% 
  left_join(hub_locations %>% select(fips, pop2 = population), by = c("county2" = "fips")) %>% 
  mutate(weight = pop2/mi_to_county) %>% arrange(desc(weight))

g_grav <- graph_from_data_frame(
  cdists %>% select(county1, county2, weight),
  directed = TRUE)

G_grav <- Diagonal(length(strength(g_grav)), 1/strength(g_grav)) %*% 
  as_adjacency_matrix(g_grav, attr = "weight")

G_grav[G_grav < .001] <- 0
  
