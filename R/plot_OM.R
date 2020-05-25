#' Plots OM object
#' 
#' Plots the yearly averages of the values in the OM object.
#' 
#' @param x An \code{\link[=OM_class]{OM}} object output by one of the simulation functions.
#' @param start.forecast.year If passed in, the training data will be blue and the test data will be grey. Otherwise the yearly averages will be in black.
#' @param ... Not used.
#' @method plot OM
#' @export
plot.OM <- function(x, ..., start.forecast.year=NULL) {
  if(!missing(start.forecast.year)){
    if(!is.numeric(start.forecast.year)) stop("start.forecast.year must be an integer.")
    if(!(start.forecast.year%%1 == 0)) stop("start.forecast.year must be an integer.")
    if(start.forecast.year > max(x$grid$year)){
      cat("start.forecast.year is greater than the max year in OM model.\n")
      start.forecast.year <- NULL
    }
    if(start.forecast.year < min(x$grid$year)){
      cat("start.forecast.year is less than the min year in OM model.\n")
      start.forecast.year <- NULL
    }
  }else{
    start.forecast.year <- NULL
  }
  cols <- colnames(x$grid)[!colnames(x$grid) %in% c("lon", "lat", "year")]
  year_mean <-  aggregate(.~year, x$grid, FUN="mean", na.action=na.pass) 
  year_mean <-  reshape2::melt(year_mean, id.vars=c("year"),
                   measure.vars=cols,
                   variable.name="variable",
                   value.name="value")
  year_mean$date <- as.Date(paste(year_mean$year, "01-01", sep="-"))
 
  if(!is.null(start.forecast.year)){
    p <- ggplot(year_mean, aes(x=.data$date, y=.data$value)) + geom_line(col="grey") +
    geom_line(aes(x=.data$date, y=.data$value), col="blue", subset(year_mean, .data$year<=start.forecast.year)) +
    facet_wrap(~variable, scales = "free_y") +
    xlab("year")
  }else{
    p <- ggplot(year_mean, aes(x=.data$date, y=.data$value)) + geom_line() +
      facet_wrap(~.data$variable, scales = "free_y") +
      xlab("year")
  }
  p
  }
