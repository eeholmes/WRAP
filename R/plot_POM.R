#' Plots POM object
#' 
#' Plots the yearly averages of the values in the POM object.
#' 
#' @param x An \code{\link[=POM_class]{POM}} object output by \code{\link{predict.OM}}.
#' @param ... Not used.
#' @method plot POM
#' @export
plot.POM <- function(x, ...) {

  start.forecast.year <- attr(x, "start.forecast.year")
  
  cols <- colnames(x)[!colnames(x) %in% c("year", "lon", "lat","pred.type")]
  year_mean <- aggregate(.~.data$year, x, FUN="mean", na.action=na.pass) 
  year_mean <- reshape2::melt(year_mean, id.vars=c("year"),
         measure.vars=cols,
         variable.name="variable",
         value.name="value")
  year_mean$date <- as.Date(paste(year_mean$year, "01-01", sep="-"))
 
  all.nas <- tapply(year_mean$value, year_mean$variable, function(x){all(is.na(x))})
  good.vars <- names(all.nas)[!all.nas]
  year_mean <- subset(year_mean, .data$variable %in% good.vars)
  if(start.forecast.year<max(x$year)){
    p <- ggplot(year_mean, aes(x=.data$date, y=.data$value)) + geom_line(col="grey") +
    geom_line(aes(x=.data$date, y=.data$value), col="blue", subset(year_mean, year<=start.forecast.year)) +
    facet_wrap(~.data$variable, scales = "free_y") +
    xlab("") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
  }else{
    p <- ggplot(year_mean, aes(x=.data$date, y=.data$value)) + geom_line() +
      facet_wrap(~.data$variable, scales = "free_y") +
      xlab("") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
  }
  p
  }
