#' Prints OM object
#' 
#' prints a \link[=OM_class]{OM} object returned from one of the simulation functions.
#' 
#' @param x OM object
#' @param ... Not used
#' @method print OM
#' @export
print.OM <- function(x, ...) {
  str <- c(
    paste0("Simulated species abundance and suitability from WRAP version ", x$meta$version, " and function ", x$meta$func, "\n"),
    paste0("  covariates: ", x$meta$covariates, "\n"),
    paste0("  grid.dimensions: ", paste(x$meta$grid.dimensions, collapse=", "), " (nrow, ncol, cells) \n"),
    paste0("  grid.resolution ", paste(x$meta$grid.resolution, collapse=", "), " (x, y) \n"),
    paste0("  grid.extent ", paste(x$meta$grid.extent, collapse=", "), " (xmin, xmax, ymin, ymax) \n"),
    paste0("  grid.units: ", paste(x$meta$grid.unit), "\n"),
    paste0("  time.extent: ", paste(x$meta$time, collapse=" to "), " ", x$meta$time.unit, "\n" )
  )
  cat(str)
}
