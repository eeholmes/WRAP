#' SDM fit with gam
#' 
#' Build a gam species distribution model from the output from a operating model.
#' 
#' @param x (required) An operating model as output from one of the operating model functions (such as \code{sim <- \link{SimulateWorld}()} OR list with meta$abund_enviro and grid from the operating model (\code{sim$grid}).
#' @param covariates Covariates to use in the SDM. Must be in the operating model output (in the columns of x$grid). If left off, all covariates in x (in x$meta$covariates) are used.
#' @param response "pres" or "abundance"
#' @param start.forecast.year The years less will be used for fitting and the years greater than are the forecasted years.
#' @param k The smoothness of the fit can be restricted by passing in k. If left off, the default will be used.
#' 
#' @examples
#' # use defaults
#' sim <- SimulateWorld()
#' fit <- gam_sdm(sim, "temp")
#' summary(fit)
#' plot(fit)
#' 
#' sim <- SimulateWorld_ROMS()
#' fit <- gam_sdm(sim, "sst", response="abundance", k=4)
#' summary(fit)
#' plot(fit)
#' 
#' @export
gam_sdm <- function(x, covariates=NULL,
                    response=c("pres", "abundance"),
                    start.forecast.year=2021, k=NULL){
  if(!inherits(x, "OM") & !all(c("meta", "grid") %in% names(x))) stop("Something is wrong. x should be a OM (operating model) object or a list with meta and grid.")
  if(!(c("abund_enviro") %in% names(x$meta))) stop("Something is wrong. x$meta needs 'abund_enviro' value.")
  abund_enviro <- x$meta$abund_enviro
  if(missing(covariates)) covariates <- x$meta$covariates
  
  resp <- match.arg(response)
  if(!all(covariates %in% colnames(x$grid))) stop("The operating model does not have all the covariates specified.")
  
  
  # --- Data set-up section ----
  dat <- x$grid
  # add a column with name resp to use in the fitting functions
  dat$resp <- dat[,resp]
  
  # If lognormal and response is abundance, make the resp column logged
  if (resp=="abundance" && (abund_enviro == "lnorm_low" || abund_enviro == "lnorm_high")){
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
  k.text <- ""    # JS: default smoothness if k not passed in
  if(!missing(k) & !is.null(k)) k.text <- ", k=k"
  
  if(resp=="pres"){
    frm.text <- paste0("s(", covariates, k.text, ", bs='gp')", collapse="+")
    frm.text <- paste("resp ~", frm.text)
    fit <- mgcv::gam(as.formula(frm.text), data=dat_hist, family=binomial)
    return(fit)
  }
  
  
  #Run if lognormal response was simulated
  if (abund_enviro == "lnorm_low" | abund_enviro == "lnorm_high"){
    # write formula as text so we don't have to know the covariate
    frm.text <- paste0("s(", covariates, k.text, ", bs='gp')", collapse="+")
    frm.text <- paste("resp ~", frm.text)
    fit <- mgcv::gam(as.formula(frm.text), data=dat_hist, family=gaussian)
  }
  
  #Run if poisson response was simulated
  if (abund_enviro == "poisson"){
    # write formula as text so we don't have to know the covariate
    frm.text <- paste0("s(", covariates, k.text, ")", collapse="+")
    frm.text <- paste("resp ~", frm.text)
    fit <- mgcv::gam(as.formula(frm.text), data=dat_hist, family=poisson)
  }
  return(fit)
}