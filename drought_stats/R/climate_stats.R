f = "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/MT-001/tavg/1/5/1895-2024.csv?base_prd=true&begbaseyear=1991&endbaseyear=2020"


get_climate_ts <- function(
  county_num = "MT-001",
  variable = "tavg",
  start_year = 1985,
  end_year = lubridate::today() |> lubridate::year(),
  timescale = "1",
  month = (lubridate::today() - lubridate::dmonths(1)) |> lubridate::month(),
  por_start_year = 1991,
  por_end_year = 2020
) {
  
}

readr::read_csv(f, skip=4)
