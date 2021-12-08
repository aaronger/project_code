
dat_bord <- bind_rows(
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesProp")),
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesPropBord")),
  .id = "graph"
) %>% 
  group_by(forecast_date, ahead, graph) %>% summarise(wis = mean(wis), ae =mean(ae))

dat_lex <- bind_rows(
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesProp")),
  readRDS(paste0(here(),"/data/evaluations/evals_AR3casesPropLEX")),
  .id = "graph"
) %>% 
  group_by(forecast_date, ahead, graph) %>% summarise(wis = mean(wis), ae =mean(ae))

ggplot(dat_bord, aes(x=forecast_date,y=wis, color = factor(ahead), linetype = graph)) +geom_line()
ggplot(dat_lex, aes(x=forecast_date,y=wis, color = factor(ahead), linetype = graph)) +geom_line()


