#' Plot the total yearly abundance
#' 
#' Plots the total abundance versus year for objects of class OM (returned by simulations functions), SDM (returned by fitting functions) and/or POM (returned by predict function).
#' 
#' If any of the objects to be compared are \link[=SDM_class]{SDM} objects, then x must be a
#' OM object and predicts will be made for the grid points in the OM object.

#' If the objects to be compared are \link[=SDM_class]{SDM} objects and
#'  have the `start.forecast.year` as part of their meta data (in $meta), a reference 
#'  line will be plotted showing the training versus test years.
#'  If `start.forecast.year`
#'  is greater than the last year of data, then no reference line will be plotted.
#'  
#' @param x Object to plot. Can be a \link[=OM_class]{OM} or \link[=POM_class]{POM} object. 
#' @param ... more objects to compare. Can be a \link[=SDM_class]{SDM}, \link[=POM_class]{POM} or \link[=OM_class]{OM}.
#' 
#' @examples
#' sim1 <- SimulateWorld(start.year=2015, n.year=20)
#' sim2 <- SimulateWorld(start.year=2040, n.year=20)
#' mlp.fit <- mlp_sdm(sim1, "temp")
#' plot_abund(sim1, mlp.fit, sim2)
#' 
#' @export
plot_abund <- function(x, ...) {
  if(!inherits(x, "POM") &
     !inherits(x, "OM")) stop("plot_abund requires that x be a OM or POM object as returned by one of the simulation functions or predict.OM. See ?plot_abund for info.")
  
  sdms <- list(x, ...)
  
  cl <- deparse( sys.call() )
  cl <- stringr::str_split(cl, "[(]")[[1]][2]; cl <- stringr::str_split(cl, "[)]")[[1]][1]
  mod.names <- stringr::str_trim(stringr::str_split(cl, ",")[[1]])
  if(any(duplicated(mod.names))){
    stop("The same object has been passed in twice. Check arguments passed to plot_abund().")
  }
  classes <- unlist(lapply(sdms, function(x){class(x)[1]}))
  if(any(classes[-1]=="SDM") && !classes[1]=="OM") stop("If any of the objects to be compared is a SDM, then x must be a OM object. See ?plot_abund.")
  
  nm <- length(sdms)

  fyr <- NULL
  for(i in 1:nm){
    if(!inherits(sdms[[i]], "SDM") &
       !inherits(sdms[[i]], "POM") &
       !inherits(sdms[[i]], "OM")) stop("plot_abund requires SDM, POM or OM objects as returned by one of the fitting functions or predict.OM. See ?plot_abund for info.")
    if(inherits(sdms[[i]], "SDM"))
      fyr <- c(fyr, sdms[[i]]$meta$start.forecast.year)
    if(inherits(sdms[[i]], "POM"))
      fyr <- c(fyr, attr(sdms[[i]], "start.forecast.year"))
  }
  fyr <- unique(fyr)
  if(!is.null(fyr) && length(fyr) > 1) stop("The start.forecast.year in the SDM (or POM) objects are different.")
  if(is.null(fyr)) fyr <- Inf #only OMs passed in
  
  pred.all <- NULL
  for(i in 1:nm){
    mod <- sdms[[i]]
    if(inherits(sdms[[i]], "SDM")){
      cat("getting prediction with", mod.names[i], "\n")
      pred <- stats::predict(x, sdm=mod, silent=TRUE)[,c("lon","lat","year","pred")]
    }
    if(inherits(sdms[[i]], "POM")) pred <- mod[,c("lon","lat","year","pred")]
    if(inherits(sdms[[i]], "OM")){
      pred <- mod$grid[,c("lon","lat","year","abundance")]
      colnames(pred) <- c("lon","lat","year","pred")
    }
    pred$model <- mod.names[i]
    pred.all <- dplyr::bind_rows(pred.all, pred)
  }
  
  # Summarize by year
  vals <- stats::aggregate(pred~year+model, data=pred.all, FUN="sum")
  colnames(vals) <- c("year", "model", "abundance")
  # So that plots are in the order supplied
  vals$model <- factor(vals$model, levels=mod.names)
  
  vals$date <- as.Date(paste(vals$year, 1, 1, sep = "-"))
  
  p <- ggplot(vals, aes(x=.data$date, y=.data$abundance, color=.data$model)) + 
    geom_line() +
    ggtitle("Total Abundance") +
    ylab("Abundance") +
    scale_x_date(date_labels = "%Y") +
    xlab("Year")
  if(fyr < max(vals$year)){
    fyr.date <- as.Date(paste(fyr, 1, 1, sep="-")) - 182
    p <- p +
      geom_vline(xintercept=fyr.date) +
      annotate("text", x=fyr.date, y=max(vals$abundance), label="  forecast", hjust=0) +
      annotate("text", x=fyr.date, y=max(vals$abundance), label="hindcast  ", hjust=1)
  }
  if(nm==1) p <- p + scale_colour_manual(values = c("black"))
  p
}
