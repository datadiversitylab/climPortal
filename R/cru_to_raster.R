# cru_to_raster() script
#
#' Convert CRU data to raster
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
cru_to_raster <- function(df) {

  var_cols <- setdiff(names(df), c("lon", "lat", "time"))
  times    <- unique(df$time)

  layers <- lapply(var_cols, function(v) {
    r_list <- lapply(times, function(t) {
      sub <- df[df$time == t, c("lon", "lat", v)]
      terra::rast(sub, type = "xyz", crs = "EPSG:4326")
    })
    terra::rast(r_list)
  })

  out <- terra::rast(layers)
  names(out) <- paste(rep(var_cols, each = length(times)), times, sep = "_")
  terra::time(out) <- rep(times, length(var_cols))

  out
}

test2 <- cru_to_raster(test1)
# run document() or command + shift + D to check that the documentation is working properly
# remember that you need values for @param, @returns, @export, @examples
