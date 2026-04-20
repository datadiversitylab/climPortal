get_cru2 <- function(tmp = TRUE,
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
    r <- terra::rast(tmp_nc, subds = var_name)

    # Filter to requested year range
    dates <- terra::time(r)
    year_mask <- as.integer(format(dates, "%Y")) >= start_year &
      as.integer(format(dates, "%Y")) <= end_year
    r <- r[[which(year_mask)]]

    # Name layers: variable_MonYear
    months <- format(terra::time(r), "%b")
    years  <- format(terra::time(r), "%Y")
    names(r) <- paste0(arg_name, "_", months, years)

    r
  }

  results <- lapply(selected, fetch_var)
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    message("All downloads failed.")
    return(invisible(NULL))
  }

  # Stack all variables into a single SpatRaster
  out <- terra::rast(results)
  return(out)
}


test1 <- get_cru2(tmp = TRUE, tmn = TRUE, tmx = TRUE, pre = TRUE)
