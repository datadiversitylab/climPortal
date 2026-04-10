# get_cru R script
#
#'
#' @description
#' Access environmental data from the CRU repository.
#' For more information see the description of the data provided by
#' \acronym{CRU}, \url{https://crudata.uea.ac.uk/cru/data/hrg/tmc/readme.txt}
#'
#' @param tmp Logical. Fetch  CRU temperature data (degrees Celsius) and return to the dataframe.
#' Defaults to \code{FALSE}.
#' @param tmin Logical. Fetch  CRU temperature data (degrees Celsius) and calculate monthly minimum temperature
#' and return to the dataframe.Defaults to \code{FALSE}.
#' @param tmax Logical. Fetch  CRU temperature data (degrees Celsius) and calculate monthly maximum temperature
#' and return to the dataframe. Defaults to \code{FALSE}.
#' @param pre Logical. Fetch  CRU precipitation data (milimeters/month) and return to the dataframe.
#' @param start_year Numeric. Define start year for fetched data. Defaults to \code{1901}.
#' @param end_year Numeric. Define end year for fetched data. Defaults to \code{2024}.
#' @param version Numeric. Define version of CRU data to fetch, 4.09 is current as of April 2026.
#' Defaults to \code{4.09}.
#'
#' @returns
#' @export
#'
#' @examples

# VARIABLES THAT I SHOULD FOCUS ON

#cru_avgtemp <- nc_open("cru_ts3.22.1901.2013.tmp.dat.nc")
#cru_mintemp <- nc_open("cru_ts3.22.1901.2013.tmn.dat.nc")
#cru_maxtemp <- nc_open("cru_ts3.22.1901.2013.tmx.dat.nc")
#cru_prec <- nc_open("cru_ts3.22.1901.2013.pre.dat.nc")

# temp
#url
#"https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmp/cru_ts4.09.1901.2024.tmp.dat.nc.gz"
# precip
#https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/pre/cru_ts4.09.1901.2024.pre.dat.nc.gz
# temp max - monthly average daily maximum temperature
#https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmx/cru_ts4.09.1901.2024.tmx.dat.nc.gz
# temp min - monthly average daily minimum temperature
#https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmn/cru_ts4.09.1901.2024.tmn.dat.nc.gz

######
#### draft 3

library(ncdf4)
library(CFtime)
library(R.utils)
library(curl)
library(lattice)
library(RColorBrewer) # might not be necessary but was with the pipeline that I followed
library(tidyverse)

get_cru <- function(tmp = FALSE,
                    tmin = FALSE,
                    tmax = FALSE,
                    pre = FALSE,
                    start_year = 1901,
                    end_year = 2024 #,
                   # version = 4.09, placeholder, need to implement this.
                    ) {

  # Validate year range
  if (!is.numeric(start_year) || !is.numeric(end_year)) {
    stop("`start_year` and `end_year` must be numeric.", call. = FALSE)
  }
  if (start_year > end_year) {
    stop("`start_year` must be <= `end_year`.", call. = FALSE)
  }
  if (start_year < 1901 || end_year > 2024) {
    stop("Year range must be within 1901-2024 for CRU TS 4.09.", call. = FALSE)
  }

  var_map <- list(
    tmp = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmp/cru_ts4.09.1901.2024.tmp.dat.nc.gz",
      var_name = "tmp"
    ),
    pre = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/pre/cru_ts4.09.1901.2024.pre.dat.nc.gz",
      var_name = "pre"
    ),
    tmax = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmx/cru_ts4.09.1901.2024.tmx.dat.nc.gz",
      var_name = "tmx"
    ),
    tmin = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmn/cru_ts4.09.1901.2024.tmn.dat.nc.gz",
      var_name = "tmn"
    )
  )

  # Collect requested variables
  requested <- list(tmp = tmp, pre = pre, tmin = tmin, tmax = tmax)
  selected  <- names(Filter(isTRUE, requested))

  if (length(selected) == 0) {
    stop("No variables requested. Set at least one argument to TRUE.", call. = FALSE)
  }

  # Increase timeout for large file downloads since I think the earlier error was from timing out
  # and restore the user's default on exit -- this is the solution from Claude
  old_timeout <- getOption("timeout")
  options(timeout = 3600)  # 1 hour
  on.exit(options(timeout = old_timeout), add = TRUE)

  fetch_var <- function(arg_name) {
    meta     <- var_map[[arg_name]]
    url_path <- meta$url
    var_name <- meta$var_name

    tmp_gz <- tempfile(fileext = ".nc.gz")
    tmp_nc <- tempfile(fileext = ".nc")
    on.exit({
      unlink(tmp_gz)
      unlink(tmp_nc)
    }, add = TRUE)

    message(sprintf("Downloading %s (this may take several minutes) ...", arg_name))

    h <- curl::new_handle(
      connecttimeout = 60,
      low_speed_limit = 1000,
      low_speed_time  = 300
    )

    dl_ok <- tryCatch(
      {
        curl::curl_download(url_path, destfile = tmp_gz, handle = h, quiet = FALSE)
        TRUE
      },
      error = function(e) {
        message(sprintf("Download failed for '%s': %s", arg_name, e$message))
        FALSE
      }
    )

    if (!dl_ok) return(invisible(NULL))

    # Verify the file isn't truncated
    if (file.size(tmp_gz) < 1e6) {
      message(sprintf("Downloaded file for '%s' appears truncated (%s bytes). Skipping.",
                      arg_name, file.size(tmp_gz)))
      return(invisible(NULL))
    }

    message(sprintf("Decompressing %s ...", arg_name))
    R.utils::gunzip(tmp_gz, destname = tmp_nc, overwrite = TRUE, remove = FALSE)

    message(sprintf("Reading %s ...", arg_name))
    nc <- ncdf4::nc_open(tmp_nc)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    lon  <- ncdf4::ncvar_get(nc, "lon")
    lat  <- ncdf4::ncvar_get(nc, "lat")
    time <- ncdf4::ncvar_get(nc, "time")
    vals <- ncdf4::ncvar_get(nc, var_name)

    # Build data frame
    df <- expand.grid(lon = lon, lat = lat, time = time) |>
      transform(value = as.vector(vals)) |>
      setNames(c("lon", "lat", "time", arg_name))

    # -- Year filtering ----------------------------------------------------
    # CRU TS time is "days since 1900-01-01"; convert to year
    origin <- as.Date("1900-01-01")
    df$year <- as.integer(format(origin + df$time, "%Y"))
    df <- df[df$year >= start_year & df$year <= end_year, ]
    df$year <- NULL

    df
  }
  results <- lapply(selected, fetch_var) # use the helper function to bring the selected variables together
  names(results) <- selected

  Reduce(function(a, b) merge(a, b, by = c("lon", "lat", "time")), results)
}

df <- get_cru(tmp=TRUE)

#########################################
# D R A F T F O U R #4
#########################################
library(ncdf4)
library(CFtime)
library(R.utils)
library(curl)
library(tidyverse)

get_cru <- function(tmp = FALSE,
                    tmin = FALSE,
                    tmax = FALSE,
                    pre = FALSE,
                    start_year = 1901,
                    end_year = 2024) {

  # Validate year range
  if (!is.numeric(start_year) || !is.numeric(end_year)) {
    stop("`start_year` and `end_year` must be numeric.", call. = FALSE)
  }
  if (start_year > end_year) {
    stop("`start_year` must be <= `end_year`.", call. = FALSE)
  }
  if (start_year < 1901 || end_year > 2024) {
    stop("Year range must be within 1901-2024 for CRU TS 4.09.", call. = FALSE)
  }

  var_map <- list(
    tmp = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmp/cru_ts4.09.1901.2024.tmp.dat.nc.gz",
      var_name = "tmp"
    ),
    pre = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/pre/cru_ts4.09.1901.2024.pre.dat.nc.gz",
      var_name = "pre"
    ),
    tmax = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmx/cru_ts4.09.1901.2024.tmx.dat.nc.gz",
      var_name = "tmx"
    ),
    tmin = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmn/cru_ts4.09.1901.2024.tmn.dat.nc.gz",
      var_name = "tmn"
    )
  )

  # Collect requested variables
  requested <- list(tmp = tmp, pre = pre, tmin = tmin, tmax = tmax)
  selected  <- names(Filter(isTRUE, requested))

  if (length(selected) == 0) {
    stop("No variables requested. Set at least one argument to TRUE.", call. = FALSE)
  }

  # Increase timeout for large file downloads and restore on exit
  old_timeout <- getOption("timeout")
  options(timeout = 3600)
  on.exit(options(timeout = old_timeout), add = TRUE)

  month_abbrevs <- c("Jan","Feb","Mar","Apr","May","Jun",
                     "Jul","Aug","Sep","Oct","Nov","Dec")

  fetch_var <- function(arg_name) {
    meta     <- var_map[[arg_name]]
    url_path <- meta$url
    var_name <- meta$var_name

    tmp_gz <- tempfile(fileext = ".nc.gz")
    tmp_nc <- tempfile(fileext = ".nc")
    on.exit({
      unlink(tmp_gz)
      unlink(tmp_nc)
    }, add = TRUE)

    message(sprintf("Downloading %s (this may take several minutes) ...", arg_name))

    h <- curl::new_handle(
      connecttimeout  = 60,
      low_speed_limit = 1000,
      low_speed_time  = 300
    )

    dl_ok <- tryCatch(
      {
        curl::curl_download(url_path, destfile = tmp_gz, handle = h, quiet = FALSE)
        TRUE
      },
      error = function(e) {
        message(sprintf("Download failed for '%s': %s", arg_name, e$message))
        FALSE
      }
    )

    if (!dl_ok) return(invisible(NULL))

    if (file.size(tmp_gz) < 1e6) {
      message(sprintf("Downloaded file for '%s' appears truncated (%s bytes). Skipping.",
                      arg_name, file.size(tmp_gz)))
      return(invisible(NULL))
    }

    message(sprintf("Decompressing %s ...", arg_name))
    R.utils::gunzip(tmp_gz, destname = tmp_nc, overwrite = TRUE, remove = FALSE)

    message(sprintf("Reading %s ...", arg_name))
    nc <- ncdf4::nc_open(tmp_nc)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    # get lon/lat
    lon  <- ncdf4::ncvar_get(nc, "lon")
    nlon <- dim(lon)
    lat  <- ncdf4::ncvar_get(nc, "lat")
    nlat <- dim(lat)

    # get time and decode with CFtime
    time   <- ncdf4::ncvar_get(nc, "time")
    tunits <- ncdf4::ncatt_get(nc, "time", "units")
    nt     <- dim(time)

    cf         <- CFtime(tunits$value, calendar = "proleptic_gregorian", time)
    timestamps <- as_timestamp(cf)
    time_cf    <- parse_timestamps(cf, timestamps)

    # filter to requested year range
    year_mask <- time_cf$year >= start_year & time_cf$year <= end_year
    time_cf   <- time_cf[year_mask, ]

    # get variable array and fill value
    var_array <- ncdf4::ncvar_get(nc, var_name)
    fillvalue <- ncdf4::ncatt_get(nc, var_name, "_FillValue")

    # replace fill values with NA
    var_array[var_array == fillvalue$value] <- NA

    # subset array to requested years along time dimension
    var_array <- var_array[, , year_mask]

    # reshape to long data frame
    var_vec_long <- as.vector(var_array)
    var_mat      <- matrix(var_vec_long, nrow = nlon * nlat, ncol = sum(year_mask))

    lonlat   <- as.matrix(expand.grid(lon, lat))
    var_df   <- data.frame(cbind(lonlat, var_mat))

    time_labels    <- paste0(month_abbrevs[time_cf$month], time_cf$year)
    names(var_df)  <- c("lon", "lat", time_labels)

    # pivot long
    var_df_long <- var_df |>
      pivot_longer(
        cols      = -c(lon, lat),
        names_to  = "time",
        values_to = arg_name       # column named after the variable, e.g. "pre", "tmp"
      ) |>
      select(lon, lat, time, all_of(arg_name))

    var_df_long
  }

  results       <- lapply(selected, fetch_var)
  names(results) <- selected

  Reduce(function(a, b) merge(a, b, by = c("lon", "lat", "time")), results)
}

###############

##### primitive below this line


# pseudo code I may come back to
# downloading

# use download.file to access the data

# evaluate where the trues are and replace the existing if else structure

todo <- c("") # vector of the names of the trues in the argument call

for(i in todo){
  download.file(...)

}
###. END DOWNLOADING
### READING
files <- list.files(...) # where extension is .dat.nc, use regex passed to the pattern argument in list.files
for(i in files){
  nc_open()
}
########### end READING

# run document() or command + shift + D to check that the documentation is working properly
# remember that you need values for @param, @returns, @export, @examples


# here's the workflow in the broken other functions that do this

library(getCRUCLdata)

t <- get_CRU_df(tmp = TRUE, pre = TRUE, tmx = TRUE)


# let the user decide to download
