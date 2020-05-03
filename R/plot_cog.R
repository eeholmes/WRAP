#' Plots COG from SDM
#' 
#' plots the center of gravity versus time
#' 
#' @param x An \code{\link[=OMclass]{OM}} object output by one of the simulation functions.
#' @param ... SDMs to compare. Must be an \link[=SDM_class]{SDM} object output from one of the fitting functions:
#' \code{\link{gam_sdm}}, \code{\link{brt_sdm}}, or \code{\link{mlp_sdm}}.
#' 
#' @examples
#' \dontrun{
#' sim <- SimulateWorld()
#' mlp.fit <- mlp_sdm(sim, "temp")
#' plot_cog(sim, mlp.fit)
#' }
#' 
#' @export
plot_cog <- function(x, ...) {
  if(!inherits(x, "OM")) stop("plot_cog requires an OM object as returned by one of the simulate functions.")
  
  sdms <- list(...)
  
  cl <- deparse( sys.call() )
  cl <- stringr::str_split(cl, "[(]")[[1]][2]; cl <- stringr::str_split(cl, "[)]")[[1]][1]
  mod.names <- stringr::str_trim(stringr::str_split(cl, ",")[[1]])
  mod.names <- mod.names[-1]
  
  # First compute the true cog
  cog_lat <- x$grid %>% dplyr::group_by(year) %>% 
    dplyr::summarize(cog=stats::weighted.mean(x=lat, w=abundance))
  cog_lat <- cbind(model="true", cog_lat, stringsAsFactors = FALSE)
  
  nm <- length(sdms)
  if(nm != 0){
    for(i in 1:nm)
      if(!inherits(sdms[[i]], "SDM")) stop("plot_cog requires SDM objects as returned by one of the fitting functions. See ?plot_cog for info.")
  fyr <- unique(unlist(lapply(sdms, function(x){x$meta$start.forecast.year})))
  if(length(fyr) != 1) stop("The start.forecast.year in the SDM objects (x$meta) is different.")
    
    pred.all <- NULL
    for(i in 1:nm){
      mod <- sdms[[i]]
      mod.a <- mod$abundance
      mod.p <- mod$presence
      cat("getting prediction with", mod.names[i], "\n")
      pred <- predict(x, p.sdm=mod.p, a.sdm=mod.a, silent=TRUE)
      pred$model <- mod.names[i]
      pred.all <- dplyr::bind_rows(pred.all, pred)
    }
    
    # Now add the predicted cogs
    tmp <- pred.all %>% dplyr::group_by(model, year) %>% 
      dplyr::summarize(cog=stats::weighted.mean(x=lat, w=pred))
    
    # dplyr uses tibbles and they return a matrix when you use rbind(). ug.
    # so use 
    cog_lat <- dplyr::bind_rows(cog_lat, tmp)
  }
  p <- ggplot(cog_lat, aes(x=year, y=cog, color=model)) + 
    geom_line() +
    ggtitle("Center of Gravity") +
    ylab("Centre of Gravity (deg lat)") +
    geom_vline(xintercept=fyr-0.5) +
    annotate("text", x=fyr-0.5, y=max(cog_lat$cog), label="  forecast", hjust=0) +
    annotate("text", x=fyr-0.5, y=max(cog_lat$cog), label="hindcast  ", hjust=1)
  
  p
}
