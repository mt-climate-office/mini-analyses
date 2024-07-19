library(magrittr)
library(ggplot2)

stations <- readr::read_csv(
  "https://mesonet.climate.umt.edu/api/stations?type=csv",
  show_col_types = FALSE
) %>% 
  dplyr::filter(
    sub_network == "HydroMet",
    date_installed <= as.Date("2024-01-01")
  ) %>%
  dplyr::select(station, name, sub_network) 

get_station_data <- function(station) {
  print(station)
  precip <- httr::GET(
    "https://mesonet.climate.umt.edu/api/observations",
    query = list(
      stations = station,
      elements = "snow_depth,ppt",
      type="csv",
      start_time="2023-01-01",
      units="si"
    )
  ) %>% 
    httr::content(show_col_types=FALSE) %>% 
    janitor::clean_names()
  
  wet_bulb <- httr::GET(
    "https://mesonet.climate.umt.edu/api/derived",
    query = list(
      stations = station,
      elements = "wet_bulb",
      keep = TRUE,
      type = "csv",
      start_time="2023-01-01",
      units="si"
    )
  ) %>%
    httr::content(show_col_types=FALSE) %>% 
    janitor::clean_names()
  
  precip %>% 
    dplyr::arrange(datetime) %>% 
    dplyr::mutate(
      snow_depth_cm = ifelse(is.na(snow_depth_cm), 0, snow_depth_cm),
      snow_diff = snow_depth_cm - dplyr::lag(snow_depth_cm, 1)
    ) %>%
    dplyr::filter(precipitation_mm != 0) %>% 
    dplyr::mutate(
      rain_or_snow = dplyr::case_when(
        precipitation_mm != 0 & snow_diff == 0 ~ 0,
        precipitation_mm != 0 & snow_diff > 0 ~ 1,
        TRUE ~ NA
      )
    ) %>%
    dplyr::left_join(
      wet_bulb, by = c("station", "datetime")
    ) %>% 
    dplyr::filter(
      !is.na(rain_or_snow)
    ) %>%
    readr::write_csv(glue::glue("./wet_bulb_precip/{station}.csv"))
}

all_data <- purrr::map(
  stations$station, function(x) {
    f = glue::glue("./wet_bulb_precip/{x}.csv")
    if (file.exists(f)) {
      return(readr::read_csv(f))
    } else {
      return(get_station_data(x))
    }
  }
)


fit_logistic_model <- function(dat) {
  model <- glm(rain_or_snow ~ wet_bulb_temperature_c, data = dat, family = binomial)
  preds <- predict(model, newdata = dat, type = "response")
  dat %>% 
    dplyr::mutate(
      snow_prob = preds
    ) %>% 
    ggplot(aes(x=wet_bulb_temperature_c, y=snow_prob, color=rain_or_snow)) + 
    geom_point()
}
