
library(igraph)

make_nw_sig_df <- function(
	sig_df, 
	g, 
	time_val = "time_value",
	sig_val = "value",
	index = "geo_value"
) {
	G <- Diagonal(length(strength(g)), 1/strength(g)) %*% as_adjacency_matrix(g)
	nw_sig_df <- sig_df %>% nest(data = -time_value) %>% 
	mutate(data = map(
		data,
		function(df) {
			nw_df <- df
			locs <- match(df[[index]], rownames(G))
			if (any(is.na(locs))) {
				stop(paste(
					df[[index]][which(is.na(locs))],
					"are not included in graph on date",
					as.character(time_value)
				))
			}
			nw_df[[sig_val]] <- as.vector(G[locs, locs] %*% df[[sig_val]])
			return(nw_df)
		}
	)) %>% 
	unnest(c(data))
	return(as.covidcast_signal(nw_sig_df, signal = sig_val))
}

sig_dir <- here("data", "offline_signals", "honest_as_of")
st_dir <- here(sig_dir, "county_weekly") 

graph_df <- read_csv(paste0(here(),"/project_data/county_adjacency_fixed.csv"))

g <- graph_from_data_frame(graph_df %>% select(fips, nghb_fips))
g_name <- "bord"

for (file in list.files(path = st_dir)) {
	readRDS(file = file.path(
		here(st_dir), 
		file)) %>% 
	make_nw_sig_df(g) %>% 
	saveRDS(file = file.path(
		here(st_dir),
		sub(x = file, "_id_", paste0("_", g_name, "_"))))
}

