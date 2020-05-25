#' SDM fit with gam
#' 
#' Build a gam species distribution model from the output from an operating model. 
#' Models fit to both the
#' presence (pres column) and log(abundance) are returned.  k can be passed
#' in to change the smoothing.
#' 
#' @param x (required) An operating model as output from one of the operating model functions (such as \code{sim <- \link{SimulateWorld}()} OR list with meta$abund_enviro and grid from the operating model (\code{sim$grid}).
#' @param covariates Covariates to use in the SDM. Must be in the operating model output (in the columns of x$grid). If left off, all covariates in x (in x$meta$covariates) are used.
#' @param start.forecast.year The years less will be used for fitting and the years greater than are the forecasted years.
#' @param k The smoothness of the fit can be restricted by passing in k. If left off, the default will be used.
#' 
#' @return A \link[=SDM_class]{SDM} object, which is a list with the presence and abundance fits and the meta data.
#' 
#' @examples
#' # use defaults
#' sim <- SimulateWorld(n.year=10)
#' fit <- gam_sdm(sim, "temp")
#' summary(fit$abundance)
#' # plot center of gravity
#' \dontrun{ #plot center of gravity (slow)
#' plot_cog(sim, fit)
#' }
#' 
#' sim <- SimulateWorld_ROMS(roms.years = 1980:1989)
#' # presence fit
#' fit <- gam_sdm(sim, "sst", k=4)
#' summary(fit$presence)
#' 
#' @export
gam_sdm <- function(x, covariates=NULL,
                    start.forecast.year=2021, k=NULL){
  
  if(!inherits(x, "OM") & !all(c("meta", "grid") %in% names(x))) stop("Something is wrong. x should be a OM (operating model) object or a list with meta and grid.")
  if(!(c("abund_enviro") %in% names(x$meta))) stop("Something is wrong. x$meta needs 'abund_enviro' value.")
  abund_enviro <- x$meta$abund_enviro
  if(missing(covariates)) covariates <- x$meta$covariates
  if(!all(covariates %in% colnames(x$grid))) stop("The operating model does not have all the covariates specified.")
  if(start.forecast.year>(max(x$grid$year)+1)) cat("start.forecast.year is greater than the last year in the data. model will be fit to all the data.\n")
  
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
  k.text <- ""    # JS: default smoothness if k not passed in
  if(!missing(k) & !is.null(k)) k.text <- ", k=k"
  
    # --- Presence fit
  
    frm.text <- paste0("s(", covariates, k.text, ", bs='gp')", collapse="+")
    frm.text <- paste("pres ~", frm.text)
    fit.p <- mgcv::gam(stats::as.formula(frm.text), data=dat_hist, family="binomial")

    # --- Abundance fit ---
    
    #Run if lognormal response was simulated
  if (abund_enviro == "lnorm_low" | abund_enviro == "lnorm_high"){
    # write formula as text so we don't have to know the covariate
    frm.text <- paste0("s(", covariates, k.text, ", bs='gp')", collapse="+")
    frm.text <- paste("log.abundance ~", frm.text)
    fit.a <- mgcv::gam(stats::as.formula(frm.text), data=dat_hist, family="gaussian")
  }
  
  #Run if poisson response was simulated
  if (abund_enviro == "poisson"){
    # write formula as text so we don't have to know the covariate
    frm.text <- paste0("s(", covariates, k.text, ")", collapse="+")
    frm.text <- paste("round.abundance ~", frm.text)
    fit.a <- mgcv::gam(stats::as.formula(frm.text), data=dat_hist, family="poisson")
  }
  # Add on the meta info from the OM object
  fit <- list(presence=fit.p, abundance=fit.a, 
              meta=c(x$meta, start.forecast.year=start.forecast.year))
  class(fit) <- c("SDM", "gam")
  
  return(fit)
}