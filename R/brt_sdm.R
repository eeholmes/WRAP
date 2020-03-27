#' SDM fit with gam
#'
#' Build a boosted regression tree species distribution model from the output from a operating model using the dismo package.
#'
#' @param x (required) An operating model as output from one of the operating model functions (such as \code{sim <- \link{SimulateWorld}()} OR just the grid from the operating model (\code{sim$grid}).
#' @param covariates (required) covariates to use in the SDM. Must be in the operating model output (in the columns of x$grid).
#' @param start.forecast.year The years equal or less will be used for fitting and the years greater than are the forecasted years.
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
brt_sdm <- function(x, covariates,
                    response=c("pres", "abundance"),
                    start.forecast.year=2020){
  # Allow the user to just pass in a grid or an OM object
  if(inherits(x, "OM")) x <- x$grid
  if(!inherits(x, "data.frame")) stop("Something is wrong. x should be a data.frame or a OM (operating model) object.")
  
  resp <- match.arg(response)
  if(!all(covariates %in% colnames(x))) stop("The operating model does not have all the covariates specified.")
  
  abund_enviro <- x$meta$abund_enviro
  
  # --- Data set-up section ----
  # The repsonse variable (pres or abundance) is put in new 'resp' column
  # If needed, resp is transformed (0s removed and logged or rounded)
  dat <- x
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
  dat_hist <- dat[dat$year<=start.forecast.year,]
  dat_fcast <- dat[dat$year>start.forecast.year,]
  
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
  
  return(fit)
}