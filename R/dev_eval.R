#' Extract explained deviance from BRT
#' 
#' function to extract explained deviance from BRT
#' 
#' @param x A fitted BRT model object, e.g. as returned from \code{\link{brt_sdm}}
#' 
#' @return The deviance
#' 
#' @examples
#' # use defaults
#' sim <- SimulateWorld()
#' fit <- brt_sdm(sim, "temp")
#' dev_eval(fit)
#' 
#' @export
dev_eval <- function(x){
  null <- x$self.statistics$mean.null
  res <- x$self.statistics$mean.resid
  dev <- ((null - res)/null)*100 
  return(dev)
}
