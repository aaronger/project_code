
library(usmap)

g <- graph_from_adjacency_matrix(
  adjmatrix = lex_sats$G[[3]], mode = "directed", diag = FALSE, weighted = TRUE
  )

vs <- embed_laplacian_matrix(g_grav, 3)
vskmns <- kmeans(vs$X,3)

vsks <- kmeans(embed_laplacian_matrix(g_grav, 3)$X, 3)

vsks$cluster
dat <- as_tibble(cbind(vs$X,vsks$cluster))
ggplot(data=dat,aes(x=V1,y=V2, color=as.factor(V3))) + geom_point()

emb <- 30; k <- 30
g <- g_grav
E(g)$weight[E(g)$weight<1000] <- 0
cclus <- counties %>% 
  right_join(tibble(geo_value = V(g)$name, 
                    clus = as.factor(kmeans(embed_adjacency_matrix(g, emb)$X, k)$cluster))) %>% 
  rename(fips=geo_value)

plot_usmap(
  regions = "counties", 
  data = cclus, 
  values = "clus",
  exclude = "AK", size = .1) + theme(legend.position = "none")

plot_gvec <- function(g, vec, no_legend = TRUE) {
  cclus <- counties %>% 
    right_join(tibble(
      geo_value = V(g)$name, 
      value = vec)) %>% 
    rename(fips=geo_value)
  
  p <- plot_usmap(
    regions = "counties", 
    data = cclus, 
    values = "value",
    exclude = "AK", size = .1)
  if (no_legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}

dat <- preds %>% left_join(hub_locations) %>% 
  transmute(
    model = forecaster,
    forecast_date = forecast_date,
    location = geo_value,
    horizon = ahead,
    temporal_resolution = "week",
    target_variable = "inc case",
    target_end_date = target_end_date,
    type = "quantile",
    quantile = quantile,
    value = value/7,
    location_name = location_name,
    geo_type = "county",
    geo_value = geo_value,
    abbreviation = abbreviation,
    full_location_name = full_location_name
  )

truth <- actuals %>%
  transmute(
    model = "Observed",
    target_variable = "inc case",
    target_end_date = target_end_date,
    location = geo_value,
    value = actual/7
  )

covidHubUtils::plot_forecasts(
  forecast_data = dat,
  locations = c("06037"),
  forecast_dates = mondays[seq(1,64, by = 4)],
  truth_data = truth %>% filter(target_end_date >= mondays[1]),
  truth_source = "CSSE",
  title = "7 day average daily case rate",
  subtitle = "none"
)+ labs(y="Case Rate")+theme_bw()
