
# some basic covidcast queries

covidcast_signal(
	data_source = "jhu-csse", 
	signal = "confirmed_incidence_num", 
	start_day = "2021-07-24", 
	end_day = "2021-08-02", 
	geo_type = 'county', 
	geo_values = c('46113', '46102'), 
	as_of = "2021-08-02"
)


covidcast_signal(
	data_source = "jhu-csse", 
	signal = "confirmed_incidence_prop", 
	start_day = "2021-07-24", 
	end_day = "2021-08-02", 
	geo_type = 'county', 
	geo_values = c('46113', '46102'), 
	as_of = "2021-08-02"
)

covidcast_signal(
	data_source = "jhu-csse", 
	signal = "confirmed_incidence_prop", 
	start_day = "2021-07-24", 
	end_day = "2021-08-02", 
	geo_type = 'county', 
	geo_values = c('46113', '46102', '02270','02158'), 
	as_of = "2021-08-02"
) 

covidcast_signal(
	data_source = "jhu-csse", 
	signal = "confirmed_incidence_prop", 
	start_day = "2021-07-24", 
	end_day = "2021-08-02", 
	geo_type = 'county', 
	geo_values = c('02270','02158'), 
	as_of = "2021-08-02"
)

cd <- covidData::load_data(
	as_of = as.Date("2020-07-01"), 
	spatial_resolution = 'county',
	temporal_resolution = 'weekly',
	measure = 'cases',
)

cdcc <- as.covidcast_signal(
       cd %>% rename(
       	time_value = date, 
       	value = inc, 
       	geo_value = location),
       signal = "cases",
       geo_type = "county",
       time_type = "week",
       data_source = "covidData",
       #issue = "2021-08-02"
     ) %>% as_tibble()