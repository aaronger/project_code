
# Establish forecast dates (Modays) for analysis

first_monday <- "2020-07-27"
last_monday <- "2021-08-09"
mondays <- seq(from = as.Date(first_monday), to = as.Date(last_monday), by = 7)

offline_signal_dir = here(
	"data", 
	"offline_signals", 
	"honest_as_of", 
	"county_weekly"
)

excluded_locs <- c('72','60','66','69','74','78','02')
# exclude AK for now, until we know how to deal with valdez-cordova

for (monday in as.list(mondays[1:3])) {
	covidData::load_data(
		as_of = monday, 
		spatial_resolution = 'county',
		temporal_resolution = 'weekly',
		measure = 'cases') %>% 
	filter(
		!(substr(location, 1, 2) %in% excluded_locs)
	) %>% 
	rename(
		time_value = date, 
		value = inc, 
		geo_value = location) %>% 
	as.covidcast_signal(
		signal = "cases",
		geo_type = "county",
		time_type = "week",
		data_source = "covidData",
		issue = monday
	) %>% 
	as_tibble() %>% 
	saveRDS(file = file.path(
		offline_signal_dir,
		sprintf('%s_%s_%s_%s.RDS', 
			"covidData", "cases", "id", as.character(monday))))
}


cty_adj <- read_csv(paste0(here(),"/project_data/county_adjacency_fixed.csv"))


cty_adj %>% 
left_join(cd1 %>% select(geo_value, value), 
	by = c("nghb_fips" = "geo_value")) %>% 
group_by(fips) %>% 
summarise(ntw_value = sum(value))

cd2 <- covidData::load_data(
		as_of = mondays[1], 
		spatial_resolution = 'county',
		temporal_resolution = 'weekly',
		measure = 'cases')