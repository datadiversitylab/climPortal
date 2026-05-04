#' Calculate Bioclimatic Variables from CRU Climate Data
#'
#' @description Calculates the 19 BioClim variables from monthly climate data,
#'   either a data frame or SpatRasters.
#'
#' @param data Tidy data frame output from \code{get_cru} function. Must
#'   contain at least CRU variables \code{"pre"}, \code{"tmn"}, and
#'   \code{"tmx"}. User may also provide similar data in
#'   \code{\link[terra]{SpatRaster}} format.
#' @param start_year Integer. Year, contained in the data, that the user wants
#'   to start calculating bioclimatic variables from.
#' @param end_year Integer. Year where the user wants the calculation to stop.
#'
#' @returns A \code{\link[terra]{SpatRaster}} with 19 bioclimatic variables.
#'
#' @examples
#' \dontrun{
#' tmin <- c(10, 12, 14, 16, 18, 20, 22, 21, 19, 17, 15, 12)
#' tmax <- tmin + 5
#' prec <- c(0, 2, 10, 30, 80, 160, 80, 20, 40, 60, 20, 0)
#' biovars(prec, tmin, tmax)
#'
#' tmn <- tmx <- prc <- terra::rast(nrow = 1, ncol = 1, nlyr = 12)
#' terra::values(tmn) <- t(matrix(c(10, 12, 14, 16, 18, 20, 22, 21, 19, 17, 15, 12)))
#' tmx <- tmn + 5
#' terra::values(prc) <- t(matrix(c(0, 2, 10, 30, 80, 160, 80, 20, 40, 60, 20, 0)))
#' b <- biovars(prc, tmn, tmx)
#' as.matrix(b)
#' }

calc_bioclim <- function(data, start_year, end_year){

  # Extract year from the "time" column explicit in the get_cru output, ex: Jan2005
  year_index <- as.integer(substr(data$time, 4, 7))

  # --- Input validation ---

  if(start_year > end_year){
    stop("start_year must be less than end_year.")
  }

  data_start <- min(year_index)
  data_end   <- max(year_index)

  if(start_year < data_start | start_year > data_end){
    stop(paste("start_year", start_year, "is outside the range of the data (", data_start, "to", data_end, ")."))
  }

  if(end_year < data_start | end_year > data_end){
    stop(paste("end_year", end_year, "is outside the range of the data (", data_start, "to", data_end, ")."))
  }

  # --- Filter and summarize ---

  year_mask <- year_index >= start_year & year_index <= end_year

  prec <- data$pre[year_mask]
  tmin <- data$tmn[year_mask]
  tmax <- data$tmx[year_mask]

  if(length(prec) > 12 | length(tmin) > 12 | length(tmax) > 12){
    month_index <- ((seq_along(prec) - 1) %% 12) + 1

    prec <- tapply(prec, month_index, mean, na.rm = TRUE)
    tmin <- tapply(tmin, month_index, mean, na.rm = TRUE)
    tmax <- tapply(tmax, month_index, mean, na.rm = TRUE)
  }

  bioclims <- biovars(prec = prec, tmin = tmin, tmax = tmax)
  return(bioclims)
}

