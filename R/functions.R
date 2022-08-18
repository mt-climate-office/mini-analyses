# This is the core of the lmomco::pargam function,
# abstracted out here for speed. All credit goes to
# Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments: Version 3, IBM Research Report RC20525, T.J. Watson Research Center, Yorktown Heights, New York.
# *and*
# Asquith, W.H., 2020, lmomco---L-moments, censored L-moments, trimmed L-moments, L-comoments, and many distributions. R package version 2.3.6, Texas Tech University, Lubbock, Texas.

pargam_slim <-
  function(LL) {
    if (any(is.na(LL[1:2]))) {
      return(c(
        alpha = NA,
        beta = NA
      ))
    }

    A1 <- -0.308
    A2 <- -0.05812
    A3 <- 0.01765
    B1 <- 0.7213
    B2 <- -0.5947
    B3 <- -2.1817
    B4 <- 1.2113
    L1 <- LL[1]
    LCV <- LL[2] / LL[1]
    if (LCV >= 0.5) {
      TT <- 1 - LCV
      ALPHA <- TT * (B1 + TT * B2) / (1 + TT * (B3 + TT *
                                                  B4))
    } else {
      TT <- pi * LCV^2
      ALPHA <- (1 + A1 * TT) / (TT * (1 + TT * (A2 + TT *
                                                  A3)))
    }
    return(c(
      alpha = ALPHA,
      beta = ALPHA / L1
    ))
  }

# This is the definition of SPI,
# using only functions from base R and vectorized for speed
calc_spi_from_gamma <-
  Vectorize(function(alpha, beta, precip) {
    if (any(is.na(c(alpha, beta, precip)))) {
      return(NaN)
    }

    # Set zero precip values to 0.01 mm
    precip[precip <= 0] <- 0.01

    # Calculate the percentiles for a given set of quantiles and a gamma dist,
    # then derive quantiles from the normal distribution with a mean of zero
    # and standard deviation of one
    qnorm(
      pgamma(
        q = precip,
        shape = alpha,
        rate = beta
      ),
      mean = 0,
      sd = 1
    )
  })

# This is a vectorized form of seq.Date
seq_vec <- Vectorize(seq.Date, vectorize.args = c("from"), SIMPLIFY = FALSE)

# This derives monthly series given a start and end date,
# and a climatology length
climatology_months <-
  function(climatology, start_date, end_date) {
    if (is.na(climatology)) {
      seq_vec(
        from = seq(start_date, end_date, "1 month"),
        to = lubridate::as_date("1895-01-01"),
        by = "-1 year"
      )
    } else {
      seq_vec(
        from = seq(start_date, end_date, "1 month"),
        length.out = climatology,
        by = "-1 year"
      )
    }
  }

# Gamma distributions are strictly positive
replace_if_zero <-
  function(x) {
    x[x <= 0] <- 0.01
    x
  }

# This uses a parallelized C++ function from the 'Lmoments' package
# to calculate the first two L-Moments of the distribution
calc_lmoments <-
  function(x) {
    out <- matrix(
      nrow = nrow(x),
      ncol = 2
    )
    not_na <- which(complete.cases(x))
    out[not_na, ] <- Lmoments:::Lmoments_calc(t(x[not_na, ]), rmax = 2L)
    out
  }

# Given a raster stack of precipitation values, this function
# replaces zeros with 0.01mm, calculates the first two L-moments,
# and estimate the alpha and beta parameters of a two-parameter gamma
# distribution.
# It returns a raster with 'alpha' and 'beta' layers
calc_gamma_from_rast <-
  function(x) {
    x[] %>%
      replace_if_zero() %>%
      calc_lmoments() %>%
      apply(1, pargam_slim) %>%
      t() %>%
      terra::setValues(terra::rast(x, nlyrs = 2), .) %>%
      magrittr::set_names(c("alpha", "beta"))
  }

cog_opts <-
  c("of=COG",
    "BLOCKSIZE=128",
    "COMPRESS=DEFLATE",
    "PREDICTOR=YES",
    "OVERVIEWS=AUTO",
    "OVERVIEW_RESAMPLING=AVERAGE")

calc_and_write <-
  function(x, dir_name){
    if(dir.exists(dir_name))
      return(dir_name)

    dir.create(dir_name,
               recursive = TRUE,
               showWarnings = FALSE)

    gamma <-
      calc_gamma_from_rast(x) %T>%
      {
        list(alpha = .$alpha,
             beta = .$beta) %>%
          purrr::imap(~terra::writeRaster(x = terra::project(.x, "epsg:4326"),
                                          filename = paste0(dir_name,"/",.y, ".tif"),
                                          filetype = "COG",
                                          overwrite = TRUE,
                                          gdal = cog_opts,
                                          # datatype='INT2U',
                                          memfrac = 0.9))
      }

    list(
      mode = ifelse(gamma$alpha < 1, 0, (gamma$alpha - 1)/gamma$beta),
      median = terra::lapp(gamma,
                           fun = function(alpha, beta){qgamma(0.5, shape = alpha, rate = beta)},
                           usenames = TRUE),
      mean = gamma$alpha/gamma$beta,
      variance = gamma$alpha/((gamma$beta)^2)
    ) %>%
      purrr::imap(~magrittr::set_names(.x, .y)) %>%
      purrr::map(round) %>%
      purrr::iwalk(~terra::writeRaster(x = terra::project(.x, "epsg:4326"),
                                       filename = paste0(dir_name,"/",.y, ".tif"),
                                       filetype = "COG",
                                       overwrite = TRUE,
                                       gdal = cog_opts,
                                       datatype='INT4S',
                                       memfrac = 0.9))

    return(dir_name)

  }

write_raster <-
  function(x, filename){
    if(file.exists(filename))
      return(filename)

    terra::writeRaster(
      x,
      filename = filename,
      overwrite = TRUE,
      gdal = c("COMPRESS=DEFLATE", "of=COG"),
      memfrac = 0.9)

    gc(verbose = FALSE); gc(verbose = FALSE)

    return(filename)

  }

