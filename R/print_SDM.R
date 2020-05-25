#' Prints SDM object
#' 
#' prints a \link[=SDM_class]{SDM} object returned from one of the simulation functions.
#' 
#' @param x SDM object
#' @param ... Not used
#' @method print SDM
#' @export
print.SDM <- function(x, ...) {
  str <- c(
    paste0("Fitted SDM of class ", class(x)[2], " from WRAP version ", x$meta$version, " fit to OM from function ", x$meta$func, "\n"),
    paste0("presence fit is in x$presence and abundance fit in x$abundance\n"),
    paste0("  covariates: ", x$meta$covariates, "\n"),
    paste0("  grid.dimensions: ", paste(x$meta$grid.dimensions, collapse=", "), " (nrow, ncol, cells) \n"),
    paste0("  grid.resolution ", paste(x$meta$grid.resolution, collapse=", "), " (x, y) \n"),
    paste0("  grid.extent ", paste(x$meta$grid.extent, collapse=", "), " (xmin, xmax, ymin, ymax) \n"),
    paste0("  grid.units: ", paste(x$meta$grid.unit), "\n"),
    paste0("  time.extent: ", paste(x$meta$time, collapse=" to "), " ", x$meta$time.unit, "\n" )
  )
  cat(str)
}
