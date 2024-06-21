library(magrittr)

pop <- sf::read_sf("~/data/census/tl_2020_30_tabblock20.shp") %>% 
  dplyr::select(POP20)

drought <- sf::read_sf("/vsizip//vsicurl/https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip") %>% 
  sf::st_make_valid() %>% 
  dplyr::transmute(usdm_class = factor(paste0("D", DM),
                                       levels = paste0("D", 0:4),
                                       ordered = TRUE)) %>%
  # Group by date and class, and generate multipolygons
  dplyr::group_by(usdm_class) %>%
  dplyr::summarise(.groups = "keep") %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::arrange(usdm_class) %>% 
  sf::st_transform(sf::st_crs(pop)) %>% 
  sf::st_crop(pop) 

joined <- sf::st_join(pop, drought, join = sf::st_intersects)

counties <- sf::read_sf("https://data.climate.umt.edu/mca/fgb/mt_counties.fgb") %>% 
  sf::st_transform(sf::st_crs(pop))

summary <- sf::st_join(joined, counties, join = sf::st_intersects) %>% 
  dplyr::filter(!is.na(County), !is.na(POP20), !is.na(usdm_class)) %>% 
  dplyr::group_by(usdm_class, County) %>% 
  dplyr::summarise(pop_in_drought = sum(POP20, na.rm = T))
