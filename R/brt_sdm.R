#' SDM fit with BRT
#'
#' Build a boosted regression tree species distribution model from the output from a operating model using the dismo package.
#'
#' @param x (required) An operating model as output from one of the operating model functions (such as \code{sim <- \link{SimulateWorld}()} OR list with meta$abund_enviro and grid from the operating model (\code{sim$grid}).
#' @param covariates Covariates to use in the SDM. Must be in the operating model output (in the columns of x$grid). If left off, all covariates in x (in x$meta$covariates) are used.
#' @param start.forecast.year The years less will be used for fitting and the years greater than are the forecasted years.
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
                    response=c("pres", "abundance"),
                    start.forecast.year=2021){
  if(!inherits(x, "OM") & !all(c("meta", "grid") %in% names(x))) stop("Something is wrong. x should be a OM (operating model) object or a list with meta and grid.")
  if(!(c("abund_enviro") %in% names(x$meta))) stop("Something is wrong. x$meta needs 'abund_enviro' value.")
  abund_enviro <- x$meta$abund_enviro
  if(missing(covariates)) covariates <- x$meta$covariates
  
  resp <- match.arg(response)
  if(!all(covariates %in% colnames(x$grid))) stop("The operating model does not have all the covariates specified.")
  
  # --- Data set-up section ----
  # The repsonse variable (pres or abundance) is put in new 'resp' column
  # If needed, resp is transformed (0s removed and logged or rounded)
  dat <- x$grid
  # add a column with name resp to use in the fitting functions
  dat$resp <- dat[,resp]
  
  # If lognormal and resp is abundance, make the resp column logged
  if (resp=="abundance" && str_detect(abund_enviro, "lnorm")){
    dat <- dat[dat$abundance>0,] # remove 0s
    dat$resp <- log(dat$resp) # log
  }
  if (resp=="abundance" && abund_enviro == "poisson"){
    dat$resp <- round(dat$resp)
  }
  
  #Create dataframe with historical/forecast data
  dat_hist <- dat[dat$year < start.forecast.year,]
  dat_fcast <- dat[dat$year>=start.forecast.year,]
  
  # --- Model fitting section ----
  # The response is in the resp column and has been transformed (if needed) already
  
  if(resp=="pres") fam <- "bernoulli"
  if(resp=="abundance" & str_detect(abund_enviro, "lnorm")) fam <- "gaussian"
  if(resp=="abundance" & abund_enviro=="poisson") fam <- "poisson"
  
  fit <- dismo::gbm.step(
    data=dat_hist, 
    gbm.x = covariates, 
    gbm.y = 'resp', 
    family = fam, 
    tree.complexity = 3, 
    learning.rate = 0.01, 
    bag.fraction = 0.6)
  
  class(fit) <- c(class(fit), "brt")
  
  return(fit)
}