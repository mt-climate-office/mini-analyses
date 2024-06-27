f = "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/MT-001/tavg/1/5/1895-2024.csv?base_prd=true&begbaseyear=1991&endbaseyear=2020"


units_map <- tibble::tribble(
  ~base_long, ~base_short, ~si_short,
  "Fahrenheit Degree-Days", "Df"
  "Degrees Fahrenheit", "Deg F", "Deg C",
  "Inches", "in", "mm",
  
)

get_climate_ts <- function(
  county_num = "MT-001",
  variable = "tavg",
  start_year = 1895,
  end_year = lubridate::today() |> lubridate::year(),
  timescale = 1,
  month = (lubridate::today() - lubridate::dmonths(1)) |> lubridate::month(),
  por_start_year = 1991,
  por_end_year = 2020,
  ...
) {
  print(county_num)
  print(variable)
  response <- file.path(
    "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series",
    county_num, 
    variable,
    timescale,
    month,
    glue::glue("{start_year}-{end_year}.csv")
  ) |>
    httr::GET(
      query = list(
        base_prd = TRUE,
        begbaseyear = por_start_year,
        endbaseyear = por_end_year
      )
    )
  
  meta <- httr::content(response, n_max = 3, show_col_types = FALSE)
  
  na_val <- meta[ifelse(variable %in% c("pdsi", "phdi", "pmdi", "zndx"), 2, 3),1] |> 
    as.character() |> 
    stringr::str_replace("Missing: ", "") |> 
    as.numeric()
  
  units
  
  title <- colnames(meta)[[2]]
  
  httr::content(
    response, 
    skip = ifelse(variable %in% c("pdsi", "phdi", "pmdi", "zndx"), 3, 4), 
    show_col_types = FALSE
  ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      value = dplyr::na_if(value, na_val),
      anomaly = dplyr::na_if(anomaly, na_val),
      date = as.character(date) |>
        paste0("01") |>
        lubridate::as_date(),
      rank_asc = dplyr::dense_rank(value),
      ranc_desc = dplyr::dense_rank(dplyr::desc(value))
    )
}


get_all_county_data <- function() {
  
  variables <- tibble::tribble(
    ~code, ~name,
    "cdd", "Cooling Degree Days",
    "hdd", "Heating Degree Days",
    "tavg", "Average Temperature",
    "tmax", "Maximum Temperature",
    "tmin", "Minimum Temperature",
    "pcp", "Precipitation",
    "pdsi", "Palmer Drought Severity Index",
    "phdi", "Palmer Hydrological Drought Index",
    "pmdi", "Palmer Modified Drought Index",
    "zndx", "Palmer Z-Index"
  )
  
  future::plan(future::multisession, workers = future::availableCores() -1)
  
  dat <- tidycensus::fips_codes |>
    tibble::as_tibble() |>
    dplyr::filter(
      state == "MT",
      county_code != "113"
    ) |>
    dplyr::mutate(
      cnty_id = glue::glue("{state}-{county_code}")
    ) |>
    dplyr::select(county, cnty_id) |>
    tidyr::crossing(variables) |>
    dplyr::rowwise() |>
    # Todo: Make this actually work
    dplyr::mutate(dat = furrr::future_map2(cnty_id, code, ~ get_climate_ts(county_num = .x, variable = .y)))
}
# NOAA National Centers for Environmental information, Climate at a Glance: County Rankings, published June 2024, 
# retrieved on June 27, 2024 from https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/rankings 
library(ggplot2)
make_ts_plot <- function(county, name, df) {
  ggplot(df, aes(x=date, y=value)) + 
    geom_point() + 
    geom_line() + 
    theme_minimal() + 
    
}
