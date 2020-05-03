#' Plots abundance from SDM
#' 
#' plots the total abundance versus time
#' 
#' @param x An \code{\link[=OMclass]{OM}} object output by one of the simulation functions.
#' @param ... SDMs to compare. Must be an \link[=SDM_class]{SDM} object output from one of the fitting functions:
#' \code{\link{gam_sdm}}, \code{\link{brt_sdm}}, or \code{\link{mlp_sdm}}.
#' 
#' @examples
#' \dontrun{
#' sim <- SimulateWorld()
#' mlp.fit <- mlp_sdm(sim, "temp")
#' plot_abund(sim, mlp.fit)
#' }
#' 
#' @export
plot_abund <- function(x, ...) {
  if(!inherits(x, "OM")) stop("plot_abund requires an OM object as returned by one of the simulate functions.")
  
  sdms <- list(...)
  
  cl <- deparse( sys.call() )
  cl <- stringr::str_split(cl, "[(]")[[1]][2]; cl <- stringr::str_split(cl, "[)]")[[1]][1]
  mod.names <- stringr::str_trim(stringr::str_split(cl, ",")[[1]])
  mod.names <- mod.names[-1]
  
  # First compute the true cog
  if (abund_enviro == "poisson") x$grid$abundance <- round(x$grid$abundance)
  abund <- stats::aggregate(abundance~year, x$grid, FUN="sum")
  abund$model <- "true"
  abund <- abund[,c("year", "model", "abundance")]

  nm <- length(sdms)
  if(nm != 0){
    abund_enviro <- x$meta$abund_enviro
    
    for(i in 1:nm){
      if(!inherits(sdms[[i]], "SDM")) stop("plot_cog requires SDM objects as returned by one of the fitting functions. See ?plot_cog for info.")
    }
    fyr <- unique(unlist(lapply(sdms, function(x){x$meta$start.forecast.year})))
    if(length(fyr) != 1) stop("The start.forecast.year in the SDM objects (x$meta) is different.")
    
    pred.all <- NULL
    for(i in 1:nm){
      mod <- sdms[[i]]
      mod.a <- mod$abundance
      mod.p <- mod$presence
      cat("getting prediction with", mod.names[i], "\n")
      pred <- stats::predict(x, p.sdm=mod.p, a.sdm=mod.a, silent=TRUE)
      pred$model <- mod.names[i]
      pred.all <- dplyr::bind_rows(pred.all, pred)
    }
    
    # Now add the predicted abundances
    tmp <- stats::aggregate(pred~year+model, pred.all, FUN="sum")
    colnames(tmp) <- c("year", "model", "abundance")
    abund <- rbind(abund, tmp)
  }
  
  p <- ggplot(abund, aes(x=year, y=abundance, color=model)) + 
    geom_line() +
    ggtitle("Comparison of lnorm_low models") +
    geom_vline(xintercept=fyr-0.5) +
    annotate("text", x=fyr-0.5, y=max(abund$abundance), label="  forecast", hjust=0) +
    annotate("text", x=fyr-0.5, y=max(abund$abundance), label="hindcast  ", hjust=1)
  p}
