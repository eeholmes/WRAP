#' Plot yearly COG
#' 
#' Plots the center of gravity versus year for objects of class OM (returned by simulations functions), SDM (returned by fitting functions) and/or POM (returned by predict function).
#' 
#' If any of the objects to be compared are \link[=SDM_class]{SDM} objects, then x must be a
#' OM object and predicts will be made for the grid points in the OM object.
#' 
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
#' sim <- SimulateWorld(start.year=2015, n.year=20)
#' mlp.fit <- mlp_sdm(sim, "temp")
#' plot_cog(sim, mlp.fit)
#' 
#' @export
plot_cog <- function(x, ...) {
  
  sdms <- list(x, ...)
  
  cl <- deparse( sys.call() )
  cl <- stringr::str_split(cl, "[(]")[[1]][2]; cl <- stringr::str_split(cl, "[)]")[[1]][1]
  mod.names <- stringr::str_trim(stringr::str_split(cl, ",")[[1]])
  if(any(duplicated(mod.names))){
    stop("The same object has been passed in twice. Check arguments passed to plot_pres().")
  }
  classes <- unlist(lapply(sdms, function(x){class(x)[1]}))
  if(any(classes[-1]=="SDM") && !classes[1]=="OM") stop("If any of the objects to be compared is a SDM, then x must be a OM object. See ?plot_cog.")
  
  nm <- length(sdms)
  
  fyr <- NULL
  for(i in 1:nm){
    if(!inherits(sdms[[i]], "SDM") &
       !inherits(sdms[[i]], "POM") &
       !inherits(sdms[[i]], "OM")) stop("plot_pres requires SDM, POM or OM objects as returned by one of the fitting functions or predict.OM. See ?plot_pres for info.")
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
    
    # Now add the predicted cogs
    vals <-  dplyr::group_by(pred.all, .data$model, .data$year) 
    vals <-  dplyr::summarize(vals, cog=stats::weighted.mean(x=.data$lat, w=.data$pred))
    # So that plots are in the order supplied
    vals$model <- factor(vals$model, levels=mod.names)
    
  vals$date <- as.Date(paste(vals$year, 1, 1, sep = "-"))
  p <- ggplot(vals, aes(x=.data$date, y=.data$cog, color=.data$model)) + 
    geom_line() +
    ggtitle("Center of Gravity") +
    ylab("Centre of Gravity (deg lat)") +
    scale_x_date(date_labels = "%Y") +
    xlab("Year")
  if(fyr < max(vals$year)){
    # substract 182 days to put at middle of year
    fyr.date <- as.Date(paste(fyr, 1, 1, sep="-")) - 182
    p <- p +
    geom_vline(xintercept=fyr.date) +
    annotate("text", x=fyr.date, y=max(vals$cog), label="  forecast", hjust=0) +
    annotate("text", x=fyr.date, y=max(vals$cog), label="hindcast  ", hjust=1)
  }
  if(nm==1) p <- p + scale_colour_manual(values = c("black"))
  p
}
