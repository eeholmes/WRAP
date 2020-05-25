#' Plots grid of abundance from OM and SDMs
#' 
#' Plots the abundance grid for one year for OM and any SDMs passed in.
#' 
#' The SDMs which will be compared are \link[=SDM_class]{SDM} objects and
#'  have the `start.forecast.year` as part of their meta data (in $meta).
#'  The total abudance (sum across all grid squares) will be plotted for
#'   the training data (to which the SDM was fit) 
#'  and the test data (after `start.forecast.year`). If `start.forecast.year`
#'  is greater than the last year of data, then the forecast years are not
#'  plotted (since there are not data for those years).
#'  
#' @param x An \code{\link[=OM_class]{OM}} object output by one of the simulation functions.
#' @param year Year(s) to show. Can be one or a range.
#' @param ... SDMs to compare. Must be an \link[=SDM_class]{SDM} object output from one of the fitting functions (\code{\link{gam_sdm}},
#'  \code{\link{brt_sdm}}, or \code{\link{mlp_sdm}}) or \link[=POM_class]{POM} object output by \code{\link{predict.OM}}.
#' 
#' @examples
#' sim <- SimulateWorld(start.year=2015, n.year=20)
#' mlp.fit <- mlp_sdm(sim, "temp")
#' plot_abund(sim, mlp.fit)
#' 
#' @export
plot_grid <- function(x, ..., year=2020) {
  
  sdms <- list(x, ...)
  
  cl <- deparse( sys.call() )
  cl <- stringr::str_split(cl, "[(]")[[1]][2]; cl <- stringr::str_split(cl, "[)]")[[1]][1]
  mod.names <- stringr::str_trim(stringr::str_split(cl, ",")[[1]])
  mod.names <- mod.names[!stringr::str_detect(mod.names, "year = ")]
  if(any(duplicated(mod.names))){
    stop("The same object has been passed in twice. Check arguments passed to plot_pres().")
  }
  classes <- unlist(lapply(sdms, function(x){class(x)[1]}))
  if(any(classes[-1]=="SDM") && !classes[1]=="OM") stop("If any of the objects to be compared is a SDM, then x must be a OM object. See ?plot_abund.")
  
  nm <- length(sdms)
  
  for(i in 1:nm){
    if(!inherits(sdms[[i]], "SDM") &
       !inherits(sdms[[i]], "POM") &
       !inherits(sdms[[i]], "OM")) stop("plot_grid requires SDM, POM or OM objects as returned by one of the fitting functions or predict.OM. See ?plot_grid for info.")
  }

  # Create prediction data.frame
  pred.all <- NULL
  for(i in 1:nm){
    mod <- sdms[[i]]
    if(inherits(sdms[[i]], "SDM")){
      cat("getting prediction with", mod.names[i], "\n")
      pred <- stats::predict(x, sdm=mod, silent=TRUE)
    }
    if(inherits(sdms[[i]], "POM")){
      pred <- mod
    }
    if(inherits(sdms[[i]], "OM")){
      pred <- x$grid[,c("year", "lat", "lon", "pres", "abundance")]
      colnames(pred) <- c("year", "lat", "lon", "pred.p", "pred")
    }
    pred$model <- mod.names[i]
    pred.all <- dplyr::bind_rows(pred.all, pred)
  }
  pred.all <- pred.all[,c("year", "lat", "lon", "pred.p", "pred", "model")]
  colnames(pred.all) <- c("year", "lat", "lon", "pres", "abundance", "model")
  # So that plots are in the order supplied
  pred.all$model <- factor(pred.all$model, levels=mod.names)
  
  subpred <- pred.all[pred.all$year %in% year,]
  p <- ggplot(subpred, 
              aes(x=.data$lon,y=.data$lat, 
                  width=x$meta$grid.resolution[1], height=x$meta$grid.resolution[2])) +
    geom_tile(aes(fill=.data$abundance)) +
    theme_classic() +
    labs(y="Latitude", x="Longitude") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous( expand = c(0, 0)) +
    theme(legend.title=element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    viridis::scale_fill_viridis() +
    facet_wrap(~.data$model)
  if(length(year)==1) p <- p + ggtitle(paste(year, "abundance")) 
  if(length(year)>1 && all(diff(year)==1)){
    p <- p + ggtitle(paste(paste(min(year),max(year),sep=":"), "abundance")) 
  }else{
    p <- p + ggtitle(paste(paste(year, collapse=", "), "abundance")) 
  }
  
  p
}
