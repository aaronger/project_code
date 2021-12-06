library(usmap)


LEX.dir <- file.path("/Users/aaron_new/covid/COVIDExposureIndices/lex_data")
LEX1 <- read_csv(paste0(LEX.dir,"/county_lex_2021-08-21.csv.gz"))
LEX0 <- read_csv(paste0(LEX.dir,"/county_lex_2020-01-22.csv.gz"))

# Does the county list seem consistent?
all(LEX0$COUNTY_PRE==LEX1$COUNTY_PRE)


counties <- read_csv("project_data/counties.csv")

notinlex <- counties %>% 
	mutate(
		fips = geo_value,
		notinlex = geo_value %in% setdiff(counties$geo_value, LEX1$COUNTY_PRE))

lex_map <- plot_usmap(
	regions = "counties", 
	data = notinlex, 
	values = "notinlex",
	exclude = "AK", size = .1) + 
scale_fill_grey(
	start = 0.5, end = 1, 
	labels=c("included", "not included"), 
	name = "LEX graph")

ggsave("maps/lex_map.pdf",lex_map, device = "png")