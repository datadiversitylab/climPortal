# get_cru R script
#
#' Access environmental data from the CRU repository
#'
#' @param temp XX
#' @param tmin YY
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

# if you want the data I was testing with
#"https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/pre/cru_ts4.09.1901.1910.pre.dat.nc.gz"


precip <- nc_open("cru_ts4.09.1901.1910.pre.dat.nc")
# this list of lists structure is mindboggling
# how do I get just the values and the months/years?

# function that might work but the data are so huge that it times out

library(ncdf4)
library(R.utils)
library(curl)

get_cru <- function(temp = FALSE,
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
    temp = list(
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
  requested <- list(temp = temp, pre = pre, tmin = tmin, tmax = tmax)
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

  Reduce(function(a, b) merge(a, b, by = c("lon", "lat", "month")), results)
}

df <- get_cru(temp=TRUE)

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
