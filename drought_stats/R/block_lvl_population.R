library(magrittr)

# 2020 Population Density Grid Following MT State Library
# https://mslservices.mt.gov/Geographic_Information/Data/DataList/datalist_Details.aspx?did=%7B5d01c4b6-a4f9-44cb-b642-0d4c3a54d8a1%7D

# Population data from https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
dat <- sf::read_sf("~/Downloads/tl_2020_30_tabblock20/tl_2020_30_tabblock20.shp") %>% 
  dplyr::select(POP20) %>% 
  sf::st_transform(2256)

# MT State Plane is in Feet so we convert to meters to match 
resolution <- 328.084

template <- terra::rast(
  extent=terra::ext(dat), crs="epsg:2256", res= resolution
)

density_raster <- terra::rasterize(dat, template, field = "POP20")

smoothed <- terra::focal(density_raster, w=5, fun = "mean")
aggregated <- terra::aggregate(smoothed, fact = 10, fun = "max")


normals::write_as_cog(aggregated, "./drought_stats/data/pop_raster.tif")
