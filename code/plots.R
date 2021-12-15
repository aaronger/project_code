
dat_bord <- bind_rows(
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesProp")),
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesPropBord")),
  .id = "graph"
) %>%
  group_by(forecast_date, ahead, graph) %>% summarisez(wis = mean(wis), ae =mean(ae))

dat_lexrn <- bind_rows(
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesProp")),
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesPropLEXrn")),
  .id = "graph"
) %>% left_join(counties, by = "geo_value") %>% 
  mutate(popcat = cut(population, c(1,.5e+6,1.5e+7))) %>% 
  group_by(forecast_date, ahead, graph, popcat) %>% summarise(wis = mean(wis), ae =mean(ae))

ggplot(dat_bord, aes(x=forecast_date,y=wis, color = factor(ahead), linetype = graph)) +geom_line() 
ggplot(dat_lexrn, aes(x=forecast_date,y=wis, color = factor(ahead), linetype = graph)) +geom_line()+ facet_wrap(~popcat)

library(wesanderson)
(pl_county <- ggplot(counties, aes(x=population)) + 
  #geom_histogram(bins = 100) + 
  geom_histogram(aes(weight = population, fill = location_name), bins = 50) +
  scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)+ 
    coord_cartesian(xlim=c(5000, 1.2e+7)) +
    guides(fill = "none") +
    scale_fill_manual(values=rep(wes_palette("Zissou1", 5),times=4000)) +
    theme_classic()
  )
