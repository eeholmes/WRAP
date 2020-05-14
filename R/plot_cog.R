#' Plots COG from SDM
#' 
#' plots the center of gravity versus time
#' 
#' The SDMs which will be compared are \link[=SDM_class]{SDM} objects and
#'  have the `start.forecast.year` as part of their meta data (in $meta).
#'  The COG will be plotted for the training data (to which the SDM was fit) 
#'  and the test data (after `start.forecast.year`). If `start.forecast.year`
#'  is greater than the last year of data, then the forecast years are not
#'  plotted (since there is not data for those years).
#' 
#' @param x An \code{\link[=OMclass]{OM}} object output by one of the simulation functions.
#' @param ... SDMs to compare. Must be an \link[=SDM_class]{SDM} object output from one of the fitting functions (\code{\link{gam_sdm}},
#'  \code{\link{brt_sdm}}, or \code{\link{mlp_sdm}}) or \link[=POM_class]{POM} object output by \code{\link{predict.OM}}.
#' 
#' @examples
#' sim <- SimulateWorld(start.year=2015, n.year=20)
#' mlp.fit <- mlp_sdm(sim, "temp")
#' plot_cog(sim, mlp.fit)
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
  # this returns a grouped data frame needed for the next step
  cog_lat <- dplyr::group_by(x$grid, year) 
  cog_lat <- dplyr::summarize(cog_lat, cog=stats::weighted.mean(x=lat, w=abundance))
  cog_lat <- cbind(model="true", cog_lat, stringsAsFactors = FALSE)
  
  nm <- length(sdms)
  if(nm != 0){
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
      cat("getting prediction with", mod.names[i], "\n")
      pred <- predict(x, sdm=mod, silent=TRUE)
      pred$model <- mod.names[i]
      pred.all <- dplyr::bind_rows(pred.all, pred)
    }
    
    # Now add the predicted cogs
    tmp <-  dplyr::group_by(pred.all, model, year) 
    tmp <-  dplyr::summarize(tmp, cog=stats::weighted.mean(x=lat, w=pred))
    
    # dplyr uses tibbles and they return a matrix when you use rbind(). ug.
    # so use 
    cog_lat <- dplyr::bind_rows(cog_lat, tmp)
  }
  cog_lat$date <- as.Date(paste(cog_lat$year, 1, 1, sep = "-"))
  p <- ggplot(cog_lat, aes(x=date, y=cog, color=model)) + 
    geom_line() +
    ggtitle("Center of Gravity") +
    ylab("Centre of Gravity (deg lat)") +
    scale_x_date(date_labels = "%Y") +
    xlab("Year")
  if(fyr < max(cog_lat$year)){
    fyr.date <- as.Date(paste(fyr, 1, 1, sep="-")) - 182
    p <- p +
    geom_vline(xintercept=fyr.date) +
    annotate("text", x=fyr.date, y=max(cog_lat$cog), label="  forecast", hjust=0) +
    annotate("text", x=fyr.date, y=max(cog_lat$cog), label="hindcast  ", hjust=1)
  }
    
  p
}
