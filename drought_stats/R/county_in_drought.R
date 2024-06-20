pop <- terra::rast("./drought_stats/data/pop_raster.tif")
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
  sf::st_crop(pop) %>% 
  terra::rasterize(pop, field = "usdm_class")


raster_to_points <- list(pop, drought) %>% 
  terra::rast() %>% 
  terra::as.data.frame(xy=TRUE) %>% 
  sf::st_as_sf(coords = c("x", "y")) %>% 
  sf::`st_crs<-`(sf::st_crs(pop)) 

counties <- sf::read_sf("https://data.climate.umt.edu/mca/fgb/mt_counties.fgb") %>% 
  sf::st_transform(sf::st_crs(pop))

sf::st_join(raster_to_points, counties, join = sf::st_intersects) %>% 
  dplyr::filter(!is.na(County), !is.na(focal_mean), !is.na(usdm_class)) %>% 
  dplyr::group_by(usdm_class, County) %>% 
  dplyr::summarise(pop_in_drought = sum(focal_mean, na.rm = T))
