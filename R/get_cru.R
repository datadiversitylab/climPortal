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
#' @param tmn Logical. Fetch  CRU temperature data (degrees Celsius) and calculate monthly minimum temperature
#' and return to the dataframe.Defaults to \code{FALSE}.
#' @param tmx Logical. Fetch  CRU temperature data (degrees Celsius) and calculate monthly maximum temperature
#' and return to the dataframe. Defaults to \code{FALSE}.
#' @param pre Logical. Fetch  CRU precipitation data (milimeters/month) and return to the dataframe.
#' @param dtr Logical. Fetch  CRU diurnal temperature range (degrees Celsius) and return to the dataframe.
#' @param vap Logical. Fetch  CRU vapor pressure data (hectoPascals (hPa)) and return to the dataframe.
#' @param start_year Numeric. Define start year for fetched data. Defaults to \code{1901}.
#' @param end_year Numeric. Define end year for fetched data. Defaults to \code{2024}.
#' @param version Numeric. Define version of CRU data to fetch, 4.09 is current as of April 2026.
#' Defaults to \code{4.09}.
#'
#' @returns
#' @export
#'
#' @examples


#########################################
# D R A F T F O U R #4
#########################################
library(ncdf4)
library(CFtime)
library(R.utils)
library(curl)
#library(tidyverse)

get_cru <- function(tmp = TRUE,
                    tmn = FALSE,
                    tmx = FALSE,
                    pre = FALSE,
                    vap = FALSE,
                    dtr = FALSE,
                    start_year = 1901,
                    end_year = 2024 #,
                    # version = 4.09, placeholder, need to implement this.
                    # further, hesitant to include this option. CRU v < 2.0 used an archaic storage
                    #format (GRIM),pre netCDF, so I don't think this workflow will work.
                    # might be easier to just maintain the package to use CRU's most recent version
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
    tmx = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmx/cru_ts4.09.1901.2024.tmx.dat.nc.gz",
      var_name = "tmx"
    ),
    tmn = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmn/cru_ts4.09.1901.2024.tmn.dat.nc.gz",
      var_name = "tmn"
    ),
    dtr = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/dtr/cru_ts4.09.1901.2024.dtr.dat.nc.gz",
      var_name = "dtr"
    ),
    vap = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/vap/cru_ts4.09.1901.2024.vap.dat.nc.gz",
      var_name = "vap"
    )
  )

  # Collect requested variables
  requested <- list(tmp = tmp, pre = pre, tmn = tmn, tmx = tmx, dtr = dtr, vap = vap)
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

    # Subset to requested years
    var_array <- var_array[, , year_mask]

    # To long
    time_labels <- paste0(month_abbrevs[time_cf$month], time_cf$year)
    grid <- expand.grid(
      lon  = lon,
      lat  = lat,
      time = time_labels,
      stringsAsFactors = FALSE
    )
    grid[[arg_name]] <- as.vector(var_array)
    #all(is.na(grid[["tmp"]])) #Check that not all of them are NAs
    grid
  }

  results <- lapply(selected, fetch_var)
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    message("All downloads failed.")
    return(invisible(NULL))
  }

  # All grids share lon/lat/time. Use the skeleton from the first,
  # then bind only the value columns from the rest
  out <- results[[1]]
  if (length(results) > 1) {
    for (i in seq(2, length(results))) {
      out[[selected[i]]] <- results[[i]][[selected[i]]]
    }
  }

  return(out)
}


test1 <- get_cru(tmp = TRUE, pre = TRUE)

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

########
######
#### draft 3, some edits made, ignore since this is not current

library(ncdf4)
library(CFtime)
library(R.utils)
library(curl)
library(lattice)
library(RColorBrewer) # might not be necessary but was with the pipeline that I followed
library(tidyverse)

get_cru <- function(tmp = FALSE,
                    tmn = FALSE,
                    tmx = FALSE,
                    pre = FALSE,
                    vap = FALSE,
                    dtr = FALSE,
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
    tmx = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmx/cru_ts4.09.1901.2024.tmx.dat.nc.gz",
      var_name = "tmx"
    ),
    tmn = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmn/cru_ts4.09.1901.2024.tmn.dat.nc.gz",
      var_name = "tmn"
    ),
    dtr = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/dtr/cru_ts4.09.1901.2024.tmn.dat.nc.gz",
      var_name = "dtr"
    ),
    vap = list(
      url      = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/vap/cru_ts4.09.1901.2024.vap.dat.nc.gz",
      var_name = "vap"
    ),
  )

  # Collect requested variables
  requested <- list(tmp = tmp, pre = pre, tmn = tmn, tmx = tmx, dtr = dtr, vap = vap)
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
