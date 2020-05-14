#' Plots abundance from SDM
#' 
#' plots the total abundance versus time
#' 
#' The SDMs which will be compared are \link[=SDM_class]{SDM} objects and
#'  have the `start.forecast.year` as part of their meta data (in $meta).
#'  The average abudance will be plotted for the training data (to which the SDM was fit) 
#'  and the test data (after `start.forecast.year`). If `start.forecast.year`
#'  is greater than the last year of data, then the forecast years are not
#'  plotted (since there are not data for those years).
#'  
#' @param x An \code{\link[=OMclass]{OM}} object output by one of the simulation functions.
#' @param ... SDMs to compare. Must be an \link[=SDM_class]{SDM} object output from one of the fitting functions (\code{\link{gam_sdm}},
#'  \code{\link{brt_sdm}}, or \code{\link{mlp_sdm}}) or \link[=POM_class]{POM} object output by \code{\link{predict.OM}}.
#' 
#' @examples
#' \dontrun{
#' sim <- SimulateWorld(start.year=2015, n.year=20)
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
  
  # First compute the true abundance
  if (abund_enviro == "poisson") x$grid$abundance <- round(x$grid$abundance)
  abund <- stats::aggregate(abundance~year, x$grid, FUN="sum")
  abund$model <- "true"
  abund <- abund[,c("year", "model", "abundance")]
  
  nm <- length(sdms)
  if(nm != 0){
    abund_enviro <- x$meta$abund_enviro
    fyr <- NULL
    for(i in 1:nm){
      if(!inherits(sdms[[i]], "SDM") & !inherits(sdms[[i]], "POM")) stop("plot_cog requires SDM or POM objects as returned by one of the fitting functions or predict.OM. See ?plot_abund for info.")
    if(inherits(sdms[[i]], "SDM"))
      fyr <- c(fyr, sdms[[i]]$meta$start.forecast.year)
    if(inherits(sdms[[i]], "POM"))
      fyr <- c(fyr, attr(sdms[[i]], "start.forecast.year"))
    }
    fyr <- unique(fyr)
    if(length(fyr) != 1) stop("The start.forecast.year in the SDM (or POM) objects are different.")
    
    pred.all <- NULL
    for(i in 1:nm){
      mod <- sdms[[i]]
      if(inherits(sdms[[i]], "SDM")){
      cat("getting prediction with", mod.names[i], "\n")
      pred <- stats::predict(x, sdm=mod, silent=TRUE)
      }else{
        pred <- mod
      }
      pred$model <- mod.names[i]
      pred.all <- dplyr::bind_rows(pred.all, pred)
    }
    
    # Now add the predicted abundances
    tmp <- stats::aggregate(pred~year+model, pred.all, FUN="sum")
    colnames(tmp) <- c("year", "model", "abundance")
    abund <- rbind(abund, tmp)
  }
  abund$date <- as.Date(paste(abund$year, 1, 1, sep = "-"))
  
  p <- ggplot(abund, aes(x=date, y=abundance, color=model)) + 
    geom_line() +
    ggtitle("Average Abundance") +
    ylab("Abundance") +
    scale_x_date(date_labels = "%Y") +
    xlab("Year")
  if(fyr < max(abund$year)){
    fyr.date <- as.Date(paste(fyr, 1, 1, sep="-")) - 182
    p <- p +
      geom_vline(xintercept=fyr.date) +
      annotate("text", x=fyr.date, y=max(abund$abundance), label="  forecast", hjust=0) +
      annotate("text", x=fyr.date, y=max(abund$abundance), label="hindcast  ", hjust=1)
  }
  p
}
