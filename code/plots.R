
dat <- bind_rows(
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesProp")),
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesPropBord")),
  .id = "graph"
) %>% 
  group_by(forecast_date, ahead, graph) %>% summarise(wis = mean(wis))

ggplot(dat, aes(x=forecast_date,y=wis, color = factor(ahead), linetype = graph)) +geom_line()


