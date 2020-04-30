#' SDM fit with BRT
#'
#' Build a boosted regression tree species distribution model from the output from a operating model using the dismo package. 
#' Models fit to both the
#' presence (pres column) and log(abundance) are returned.
#'
#' @param x (required) An operating model as output from one of the operating model functions (such as \code{sim <- \link{SimulateWorld}()} OR list with meta$abund_enviro and grid from the operating model (\code{sim$grid}).
#' @param covariates Covariates to use in the SDM. Must be in the operating model output (in the columns of x$grid). If left off, all covariates in x (in x$meta$covariates) are used.
#' @param start.forecast.year The years less will be used for fitting and the years greater than are the forecasted years.
#' @param control Parameters for the `dismo::gbm.step` call.
#' 
#' @return A list with the presence and abundance fits and the meta data.
#'
#' @examples
#' sim <- SimulateWorld()
#' fit <- brt_sdm(sim, "temp")
#' dev_eval(fit)
#' plot(fit)
#' 
#' # you can also pass in the grid from sim
#' # this allows you to modify the grid (add 0s or error)
#' bad.grid <- sim$grid
#' bad.grid$abundance <- bad.grid$abundance + rnorm(nrow(bad.grid),0,0.05)
#' fit <- brt_sdm(bad.grid, "temp", response="abundance")
#' 
#' @export
brt_sdm <- function(x, covariates=NULL,
                    start.forecast.year=2021,
                    control = list(tree.complexity = 3, learning.rate = 0.01, bag.fraction = 0.6)){
  if(!inherits(x, "OM") & !all(c("meta", "grid") %in% names(x))) stop("Something is wrong. x should be a OM (operating model) object or a list with meta and grid.")
  if(!(c("abund_enviro") %in% names(x$meta))) stop("Something is wrong. x$meta needs 'abund_enviro' value.")
  abund_enviro <- x$meta$abund_enviro
  if(missing(covariates)) covariates <- x$meta$covariates
  
  if(!all(covariates %in% colnames(x$grid))) stop("The operating model does not have all the covariates specified.")
  
  # --- Data set-up section ----
  dat <- x$grid
  
  # If lognormal and response is abundance, make the resp column logged
  if ((abund_enviro == "lnorm_low" || abund_enviro == "lnorm_high")){
    dat$log.abundance <- log(dat$abundance) # log
    dat$log.abundance[dat$abundance==0] <- NA # remove 0s
  }
  if (abund_enviro == "poisson"){
    dat$round.abundance <- round(dat$abundance)
  }
  
  #Create dataframe with historical/forecast data
  dat_hist <- dat[dat$year < start.forecast.year,]
  dat_fcast <- dat[dat$year>=start.forecast.year,]
  
  # --- Model fitting section ----

  ## --- Presence fit
  
  fam <- "bernoulli"
  fit.p <- dismo::gbm.step(
    data=dat_hist, 
    gbm.x = covariates, 
    gbm.y = 'pres', 
    family = fam, 
    tree.complexity = control$tree.complexity, 
    learning.rate = control$learning.rate, 
    bag.fraction = control$bag.fraction)
  
  ## --- Abundance fit
  
  if(str_detect(abund_enviro, "lnorm")){ fam <- "gaussian"; resp <- "log.abundance" }
  if(abund_enviro=="poisson"){ fam <- "poisson"; resp <- "round.abundance" }
  fit.a <- dismo::gbm.step(
    data=dat_hist, 
    gbm.x = covariates, 
    gbm.y = resp, 
    family = fam, 
    tree.complexity = control$tree.complexity, 
    learning.rate = control$learning.rate, 
    bag.fraction = control$bag.fraction)
  
  # Add on the meta info from the OM object
  fit <- list(presence=fit.p, abundance=fit.a, meta=x$meta)
  class(fit) <- c(class(fit), "brt", "SDM")

  return(fit)
}