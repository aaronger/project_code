
dat <- bind_rows(
  evals_AR3casesProp,
  evals_AR3casesPropBord,
  .id = "graph"
) %>% 
  group_by(forecast_date, ahead, graph) %>% summarise(wis = mean(wis))

ggplot(dat, aes(x=forecast_date,y=wis, color = factor(ahead), linetype = graph)) +geom_line()


