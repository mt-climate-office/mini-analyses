library(magrittr)

pop <- sf::read_sf("~/data/census/tl_2020_30_tabblock20/tl_2020_30_tabblock20.shp") %>% 
  dplyr::select(POP20)

county_codes <- readr::read_delim(
  "https://www2.census.gov/geo/docs/reference/codes2020/cou/st30_mt_cou2020.txt", 
  delim = "|"
) %>% 
  dplyr::select(COUNTYFP20=COUNTYFP, county=COUNTYNAME)

drought <- sf::read_sf("/vsizip//vsicurl/https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip") %>% 
  sf::st_make_valid() %>% 
  dplyr::transmute(usdm_class = paste0("D", DM)) %>%
  # Group by date and class, and generate multipolygons
  dplyr::group_by(usdm_class) %>%
  dplyr::summarise(.groups = "keep") %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::arrange(usdm_class) %>% 
  sf::st_transform(sf::st_crs(pop)) %>% 
  sf::st_crop(pop) 

joined <- sf::st_join(pop, drought, join = sf::st_intersects)

pop_in_drought <- joined %>% 
  dplyr::left_join(county_codes) %>% 
  dplyr::select(county, population=POP20, usdm_class) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(county, usdm_class) %>% 
  dplyr::summarise(pop_in_drought = sum(population)) %>% 
  dplyr::mutate(usdm_class = ifelse(is.na(usdm_class), 'No Drought', usdm_class)) %>%
  dplyr::mutate(usdm_class = factor(
    usdm_class,
    levels = c("No Drought", "D0", "D1", "D2", "D3", "D4"),
    ordered = TRUE
  )) %>%
  dplyr::mutate(
    County = stringr::str_replace(county, " County", "")
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-county) %>%
  dplyr::group_by(County) %>% 
  tidyr::complete(usdm_class = factor(levels(usdm_class), levels = levels(usdm_class), ordered = TRUE)) %>%
  dplyr::mutate(
    pop_in_drought = ifelse(is.na(pop_in_drought), 0, pop_in_drought)
  )

summary <- sf::read_sf("https://data.climate.umt.edu/mca/fgb/mt_counties.fgb") %>% 
  sf::st_transform(sf::st_crs(pop)) %>% 
  dplyr::left_join(pop_in_drought, by = "County") 

map <- mapview::mapview()

for (d in c("No Drought", "D0", "D1", "D2", "D3", "D4")) {
  summary %>% 
    dplyr::filter(usdm_class == d) %>% 
    dplyr::select(-County) %>% 
    mapview::mapview(map = map, zcol = pop_in_drought)
}

map
sf::write_sf(summary, "./drought_stats/data/latest_pid.fgb")
