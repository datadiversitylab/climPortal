# get_cru R script
#
#' Access environmental data from the CRU repository
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
get_cru <- function(pre=FALSE, tmp=FALSE, rdO=FALSE, dtr=FALSE, reh=FALSE,
                    sunp=FALSE, frs=FALSE, wnd=FALSE, elv=FALSE) {
  if(pre == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_pre.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()
  if(tmp == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_tmp.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()
  if(rdO == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_rd0.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()
  if(dtr == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_dtr.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()
  if(reh == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_reh.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()
  if(sunp == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_sunp.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()
  if(frs == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_frs.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()
  if(wnd == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_wnd.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()
  if(wnd == TRUE){
    url_path <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_elv.dat.gz"

    con <- gzcon(url(url_path, open = "rb"))
    on.exit(close(con))

    df <- read.table(con, header = FALSE)
    df
  }else()

}

# different approach, needs testing

get_cru <- function(pre = FALSE, tmp = FALSE, rdO = FALSE, dtr = FALSE,
                    reh = FALSE, sunp = FALSE, frs = FALSE, wnd = FALSE,
                    elv = FALSE) {

  var_map <- list(
    pre  = "pre",
    tmp  = "tmp",
    rdO  = "rd0",
    dtr  = "dtr",
    reh  = "reh",
    sunp = "sunp",
    frs  = "frs",
    wnd  = "wnd",
    elv  = "elv"
  )

  base_url <- "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_%s.dat.gz"

  requested <- list(pre = pre, tmp = tmp, rdO = rdO, dtr = dtr,
                    reh = reh, sunp = sunp, frs = frs, wnd = wnd,
                    elv = elv)

  selected <- names(Filter(isTRUE, requested))

  if (length(selected) == 0) {
    stop("No variables requested. Set at least one argument to TRUE.", call. = FALSE)
  }

  fetch_var <- function(var_name) {
    slug <- var_map[[var_name]]
    url_path <- sprintf(base_url, slug)

    message(sprintf("Fetching %s ...", var_name))

    tryCatch({
      con <- gzcon(url(url_path, open = "rb"))
      on.exit(close(con), add = TRUE)
      df <- read.table(con, header = FALSE)
      # Name all columns after the variable (e.g. pre_V1, pre_V2, ...)
      colnames(df) <- paste0(var_name, "_", colnames(df))
      df
    }, error = function(e) {
      warning(sprintf("Failed to fetch '%s': %s", var_name, e$message), call. = FALSE)
      NULL
    })
  }

  # Fetch all requested variables
  results <- lapply(selected, fetch_var)
  names(results) <- selected

  # Drop any NULLs from failed fetches
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    stop("All variable fetches failed.", call. = FALSE)
  }

  # Bind all dataframes column-wise into one
  do.call(cbind, results)
}

# run document() or command + shift + D to check that the documentation is working properly
# remember that you need values for @param, @returns, @export, @examples


# here's the workflow in the broken other functions that do this

library(getCRUCLdata)

t <- get_CRU_df(tmp = TRUE, pre = TRUE, tmx = TRUE)
