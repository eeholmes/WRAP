#' Plots OM object
#' 
#' plots the center of gravity and average abundance for a SDM object
#' 
#' @param x An \code{\link[=OMclass]{OM}} object output by one of the simulation functions.
#' @param ... SDMs to compare. Must be an \link[=SDM_class]{SDM} object output from one of the fitting functions:
#' \code{\link{gam_sdm}}, \code{\link{brt_sdm}}, or \code{\link{mlp_sdm}}.
#' @method plot SDM
#' @export
plot.SDM <- function(x, ...) {
  p1 <- plot_cog(x, ...)
  p2 <- plot_abund(x, ...)
  gridExtra::grid.arrange(p1, p2)
}
