library(tidyverse)
library(covidHubUtils)
library(igraph)
library(Matrix)
library(here)

counties <- read_csv(paste0(here(),"/project_data/counties.csv"))
# from https://www.nber.org/research/data/county-distance-database
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

p1 <- ggplot(data.frame(x = as.vector(G_grav)), aes(x=x)) + geom_histogram(bins = 100) +
  scale_x_continuous(trans='log10', labels = scales::comma, limits = c(1e-7,.02))
p1
p2 <- ggplot(data.frame(x = as.vector(G_grav)), aes(x=x)) + geom_histogram(bins = 100) +
  scale_x_continuous(trans='log10', labels = scales::comma, limits = c(.008,.1))
p2

p1 + annotation_custom(ggplotGrob(p2), xmin = .001, xmax = .01, ymin=1e+05, ymax = 1e+05)

G_grav[G_grav < .001] <- 0