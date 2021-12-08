
library(igraph)

make_nw_sig_df <- function(
	sig_df, 
	G, 
	time_shift,
	gname,
	zero_impute = TRUE, # could later be made a function
	sig_name = "signal",
	time_val = "time_value",
	sig_val = "value",
	index = "geo_value"
) {
	nw_sig_df <- sig_df %>% as_tibble() %>% 
		nest(data = -time_value) %>% 
		filter(time_value + time_shift > min(G[["time_value"]])) %>% 
	mutate(data = pmap(.,
		function(data, time_value, ...) {
			if ("time_value" %in% names(G)) {
				G <- G %>% filter(time_value == time_value + time_shift) %>% 
				pluck("G", 1)
			}
			nw_df <- data
			V_sig <- data[[index]]
			V_G <- rownames(G)
			V <- intersect(V_sig, V_G)
			G_full <- matrix(
				nrow = length(V_sig), ncol = length(V_sig), 
				dimnames = list(V_sig, V_sig))
			G_full[V, V] <- G[V, V]
			# if counties are not in the graph impute or throw error
			if (length(V_sig) > length(V)) {
				V_miss <- setdiff(V_sig, V)
				if (zero_impute) {
					G_full[V_miss,] <- G_full[,V_miss] <- 0
				} else {
					stop(paste(
						V_miss,
						"are not included in graph on date",
						as.character(time_value)
					))					
				}
			}
			nw_df[[sig_val]] <- as.vector(G_full %*% data[[sig_val]])
			nw_df[[sig_name]] <- paste0(nw_df[[sig_name]], gname)
			return(nw_df)
		}
	)) %>% 
	unnest(c(data))
	return(as.covidcast_signal(nw_sig_df, signal = sig_val))
}

sig_dir <- here("data", "offline_signals", "honest_as_of")
st_dir <- here(sig_dir, "county_weekly") 

prop_files <- str_subset(list.files(path = st_dir), pattern = "Prop_")

# Adjacency graph
g <- graph_from_data_frame(
	read_csv(paste0(here(),"/project_data/county_adjacency_fixed.csv")) %>% 
	select(fips, nghb_fips))
G <- Diagonal(length(strength(g)), 1/strength(g)) %*% as_adjacency_matrix(g)
gname <- "Bord"

for (file in prop_files) {
	readRDS(file = file.path(
		here(st_dir), 
		file)) %>% 
	make_nw_sig_df(G = G, gname = gname) %>% 
	saveRDS(file = file.path(
		here(st_dir),
		sub("(Num|Prop)", paste0("\\1", gname), x = file)))
}

# LEX graph TS
files <- list.files("~/covid/COVIDExposureIndices/lex_data/", full.names = TRUE) %>% 
	str_subset(pattern = "county_lex") 
lex_dates <- as.Date(str_extract(files, "\\d{4}-\\d{2}-\\d{2}"))
late_inds <- lex_dates > "2020-04-01"
wed_inds <- lubridate::wday(lex_dates, label = TRUE) == "Wed"
sat_inds <- lubridate::wday(lex_dates, label = TRUE) == "Sat"

lex_sats <- tibble(
	time_value = lex_dates[late_inds & sat_inds],
	G = map(files[late_inds & sat_inds], function(file) {
		read_csv(file) %>% 
		column_to_rownames(var = "COUNTY_PRE") %>% 
		as.matrix()
	})
	)
saveRDS(lex_sats, file = "project_data/lex_sats")
lex_sats <- readRDS("project_data/lex_sats")

for (file in prop_files) {
	readRDS(file = file.path(
		here(st_dir), 
		file)) %>% 
	make_nw_sig_df(
		G = lex_sats, 
		time_shift = 0,
		gname = "LEX") %>% 
	saveRDS(file = file.path(
		here(st_dir),
		sub("(Num|Prop)", paste0("\\1", "LEX"), x = file)))
}















